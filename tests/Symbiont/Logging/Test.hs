{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Symbiont.Logging.Test where

import           Control.Concurrent.STM
import           Control.Lens             ((^?))
import           Control.Monad.Writer     hiding ((<>))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text                (lines)
import           Data.Time.Clock
import           Katip
import           Protolude                hiding (error)
import           Symbiont.Logging
import           Symbiont.Logging.Rewrite
import           System.Directory         (removeFile)
import           System.IO                (hClose, hFlush)
import           System.Posix.Temp
import           System.Random
import           Test.Tasty.Hspec

spec :: Spec
spec = parallel $ do
  let foo = "foo" :: Text
      _12 = 12 :: Integer
      _42 = 42 :: Int
      jsonValue = object [ "foo" .= _12, "bar" .= ("secret" :: Text) ]

  describe "Logger API" $ do

    it "can log a dynamic object at info level" $ do
      let action = info "item" $"foo" =: foo <> "bar" =: _12
          logged = runDummyLogger "prod" InfoS action

      logged `shouldBe` [ object [ "foo" .= foo, "bar" .= _12 ] ]

    it "can log a JSONable object at debug level" $ do
      let jsonable = JSONable "foo" 42
          action = debug "object" jsonable
          logged = runDummyLogger "prod" DebugS action

      logged `shouldBe` [ toJSON jsonable ]


  describe "Field rewriter" $ do
    let nonString = object [ "foo" .= _42, "bar" .= object [ "secret" .= _42 ] ]
        nested    = object [ "foo" .= _42, "baz" .= object [ "bar" .= _42 ] ]
        rewrite'  = appEndo . rewrite

    it "can rewrite a field from an object" $ do
      rewrite' "bar" jsonValue `shouldBe` object [ "foo" .= _12
                                               , "bar" .= ("REDACTED" :: Text)
                                               ]
      rewrite' "bar" nonString `shouldBe` object [ "foo" .= _42
                                               , "bar" .= ("REDACTED" :: Text)
                                               ]
      rewrite' "bar" nested  `shouldBe` object [ "foo" .= _42
                                               , "baz" .= object [ "bar" .= ("REDACTED" :: Text) ]
                                               ]

    it "can rewrite a field with a custom JSON value" $ do
      (appEndo $ rewriteCustom "bar" jsonValue) nested
        `shouldBe` object [ "foo" .= _42
                          , "baz" .= object [ "bar" .= jsonValue ]
                          ]

    it "can combine fields rewriters" $ do
      let rewriter = rewrite "bar" <> rewrite "foo"

      appEndo rewriter nested `shouldBe` object [ "foo" .= ("REDACTED" :: Text)
                                        , "baz" .= object [ "bar" .= ("REDACTED" :: Text) ]
                                        ]

  describe "IO Logger" $ do

    it "can log a JSONable object at error level to IO logger" $ do
      let jsonable = JSONable "foo" 12
          action = debug "object" jsonable

      logged <- getLoggerOutput action

      fmap (^? key "data". key "object") (readJSON  logged)
        `shouldBe` [ Just $ object [ "aFoo" .= foo, "aBar" .= _12 ] ]

    it "rewrite field from logged object given field is registered with current environment" $ do
      let
          action   = redact "bar" "prod"     >>
                     debug "object" jsonValue >>
                     debug "object" jsonValue

      logged <- getLoggerOutput action

      fmap (^? key "data". key "object") (readJSON $ logged)
        `shouldBe` [ Just $ object [ "foo" .= _12
                                   , "bar" .= ("REDACTED" :: Text)
                                   ]
                   , Just $ object [ "foo" .= _12
                                   , "bar" .= ("REDACTED" :: Text)
                                   ]]

    it "combine rewriters for different fields" $ do
      let action   = redact "bar" "prod"     >>
                     redact "foo" "prod"     >>
                     debug "object" jsonValue

      logged <- getLoggerOutput action

      fmap (^? key "data". key "object") (readJSON $ logged)
        `shouldBe` [ Just $ object [ "foo" .= ("REDACTED" :: Text)
                                   , "bar" .= ("REDACTED" :: Text)
                                   ]]

    it "does not rewrite field from logged object given rewriter is registered with different environment" $ do
      let action   = redact "bar" "dev"     >>
                     debug "object" jsonValue

      logged <- getLoggerOutput action

      fmap (^? key "data". key "object") (readJSON $ logged)
        `shouldBe` [ Just $ object [ "foo" .= _12
                                   , "bar" .= ("secret" :: Text)
                                   ]]

    it "rewrite field with custom value given field is registered with current environment" $ do
      let action   = redactCustom "foo" _42 "prod"     >>
                     debug "object" jsonValue

      logged <- getLoggerOutput action

      fmap (^? key "data". key "object") (readJSON $ logged)
        `shouldBe` [ Just $ object [ "foo" .= _42
                                   , "bar" .= ("secret" :: Text)
                                   ]
                   ]

  describe "Spans & Trace" $ do

    it "adds span to log namespace when pushing new span" $ do
      let action = pushSpan "span"          >>
                   debug "object" jsonValue >>
                   popSpan                  >>
                   debug "object" jsonValue


      logged <- getLoggerOutput action

      fmap (^? key "ns") (readJSON $ logged)
        `shouldBe` [ Just $ toJSON ["Symbiont" :: Text, "span", "object"]
                   , Just $ toJSON ["Symbiont" :: Text, "object"]
                   ]

    it "ignores popping span given no corresponding pushSpan" $ do
      let action = debug "object" jsonValue >>
                   popSpan                  >>
                   debug "object" jsonValue


      logged <- getLoggerOutput action

      fmap (^? key "ns") (readJSON $ logged)
        `shouldBe` [ Just $ toJSON ["Symbiont" :: Text, "object"]
                   , Just $ toJSON ["Symbiont" :: Text, "object"]
                   ]

  around mkTempFile $ describe "File Logger" $ do

    it "can handle concurrent logging" $ \ (file, hdl) -> do
      env <- defaultEnvironment hdl

      let logLoop count = runIOLogger env $ do
            forM_ [ 1 .. 100 ] (\ n -> do
                                   info "log" (JSONable "foo" n)
                                   delay <- liftIO $ randomRIO (500,1500)
                                   liftIO $ threadDelay delay)
            liftIO $ atomically $ modifyTVar' count (+1)

          logs = do
            count <- newTVarIO (0 :: Int)
            replicateM_ 10 $ forkIO (logLoop count)
            atomically $ do
              done <- readTVar count
              unless (done == 10)  retry
            hFlush hdl
            hClose hdl

      logs
      logged <- readFile file

      length (readJSON logged) `shouldBe` 1000


readJSON :: Text -> [ Value ]
readJSON = catMaybes . fmap (decode. LBS.fromStrict . encodeUtf8) . lines

getLoggerOutput act = bracket (mkstemp "effects-test") deleteTempFile go
  where
    go (filepath,hdl) = do
      env <- defaultEnvironment hdl
      runIOLogger env act
      closeEnvironment env
      hFlush hdl
      hClose hdl
      readFile filepath

-- utility functions
-- we create a temporary file as a log file then delete it after test execution
deleteTempFile :: (FilePath, Handle) -> IO ()
deleteTempFile (filepath, hdl) = hClose hdl >> removeFile filepath

mkTempFile :: ((FilePath,Handle) -> IO ()) -> IO ()
mkTempFile = bracket (mkstemp "effects-test") deleteTempFile


data JSONable = JSONable { aFoo :: Text
                         , aBar :: Integer
                         }
              deriving (Generic, Show, Eq)

instance FromJSON JSONable
instance ToJSON JSONable

-- * Testing

-- | A simple `Writer` based logged for testing purpose
-- Puts dummy values in contextual logging information (e.g. time, threadId,...)
instance Logger (Writer [Item Value]) where
  doLog _ severity item = tell  [
    Item "app" "dev" severity (ThreadIdText "12") "localhost" 1234 (toJSON item) "log string" (UTCTime (toEnum 0) 0) "ns" Nothing]
  redactCustom _field _value _envName = pure ()
  pushSpan _ = undefined
  popSpan = undefined


runDummyLogger :: Environment
               -> Severity
               -> Writer [Item Value] ()
               -> [ Value ]

runDummyLogger _env severity = map _itemPayload . filter ((>= severity). _itemSeverity) . snd . runWriter

