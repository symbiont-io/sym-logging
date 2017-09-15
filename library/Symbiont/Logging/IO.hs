{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TupleSections              #-}

{-| Provides IO based `Logger` and needed low-level routines
-}
module Symbiont.Logging.IO
  ( -- * Basic IO Logger
    IOLogger, runIOLogger,
    -- * Environments Creation
    defaultEnvironment, namespacedEnvironment, nullEnvironment, closeEnvironment
  )where

import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.Reader     (ReaderT (..))
import           Control.Monad.State      (StateT (..))
import           Control.Monad.Trans
import           Control.Monad.Writer     hiding ((<>))
import           Data.Aeson
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Map                 as Map
import           Data.Text.IO             (hPutStrLn)
import           Data.Time                (getCurrentTime)
import           Katip
import           Protolude                hiding (error)

import           Symbiont.Logging.Rewrite
import           Symbiont.Logging.Types



-- | Monad transformer for logging to IO
-- Maintains a `LoggerEnv` in a `StateT` monad and uses `KatipT` as underlying logging mechanism.
newtype IOLogger m a = IOLogger { runLogger :: StateT LoggerEnv (KatipT m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState LoggerEnv, MonadThrow)

instance MonadTrans IOLogger where
  lift m = IOLogger $ StateT $ \ s -> KatipT (ReaderT $ \_ -> m >>= pure . (,s))


instance MonadIO m => Katip (IOLogger m) where
    getLogEnv = IOLogger (_katipEnv <$> get)
    localLogEnv f m = modify (modifyKatipEnv f) >> m
      where
        modifyKatipEnv :: (LogEnv -> LogEnv) -> LoggerEnv -> LoggerEnv
        modifyKatipEnv modifier le@LoggerEnv{ _katipEnv } = le{ _katipEnv = modifier _katipEnv }



instance (MonadIO m) => Logger (IOLogger m) where
  doLog id severity item = do
    LoggerEnv rewriters env ns <- get
    let rewrite' = appEndo (fromMaybe mempty (Map.lookup (_logEnvEnv env) rewriters))
        logged   = rewrite' (toJSON item)
        ns' = ns <> Namespace [id]
    logItem (id =: logged) ns' Nothing severity ""

  redactCustom field value envName =
    modify (\ e@LoggerEnv{_rewriter} ->
               e { _rewriter = Map.insertWith (<>) envName (rewriteCustom field value) _rewriter})

  pushSpan span =
    modify (pushNamespace span)

  popSpan =
    modify popNamespace

-- | Provides a `LoggingEnv` for execution of some actions within `IOLogger` monad.
runIOLogger :: (MonadIO m)
            => LoggerEnv
            -> IOLogger m a
            -> m a

runIOLogger e@(LoggerEnv _ env _) = runKatipT env . (`evalStateT` e) . runLogger

-- | An empty environment
-- This environment has no scribe attached hence nothing gets output.

nullEnvironment :: LoggerEnv
nullEnvironment = LoggerEnv Map.empty nullEnv mempty
  where
    nullEnv = LogEnv "localhost" 0 "Symbiont" "dev" getCurrentTime Map.empty

-- | A simple environment that uses `jsonScribe` to log to `sdtout` using `"Symbiont"` as
-- global namespace.
-- Logs all severities and all verbosities.
defaultEnvironment :: Handle
                   -> IO LoggerEnv

defaultEnvironment hdl = namespacedEnvironment hdl defaultNamespace defaultDeploymentEnvironment


-- | Defines environment for logger to run
-- This environment will log to given output using json formatting (one json object per line of log).
namespacedEnvironment :: Handle
                      -> Namespace
                      -> Environment
                      -> IO LoggerEnv

namespacedEnvironment hdl nameSpace env = do
  scribe  <- registerScribe "STDOUT" (jsonScribe defaultSeverity defaultVerbosity hdl) defaultScribeSettings
              =<< initLogEnv nameSpace env

  pure $ LoggerEnv Map.empty scribe mempty

-- | Close a logging environment.
-- This ''closes'' environment @env@ effectively meaning it cannot be used anymore for logging. This ''must''
-- be called whenever one wants to dispose of logging system.

closeEnvironment :: LoggerEnv -> IO ()
closeEnvironment (LoggerEnv _ env _) = void $ closeScribes env

-- | Simple `Scribe` outputting JSON to given `Handle`.
--
--  * All writes to underlying `Handle` are serialized which might lead to contention in case of
--    a great number of threads concurrently trying to log statements at high speed
--
--  * Output is colorized using ANSI escape sequences if `hdl` is terminal (see `hIsTerminalDevice`)
jsonScribe :: Severity
           -> Verbosity
           -> Handle
           -> Scribe

jsonScribe severity verbosity hdl = Scribe jsonToHandle (pure ()) -- caller is responsible for reclaiming Handle
  where
    jsonToHandle :: (LogItem a) => Item a -> IO ()
    jsonToHandle item = do
      when (severity `permitItem` item) $ logToHandle item


    logToHandle :: (LogItem a) => Item a -> IO ()
    logToHandle item =
      itemJson verbosity item               &
      encode                                &
      LBS.toStrict                          &
      decodeUtf8                            &
      hPutStrLn hdl

