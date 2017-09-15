{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-| Base types and default values for logging library. -}
module Symbiont.Logging.Types where

import           Control.Monad.Identity
import           Control.Monad.Writer   hiding ((<>))
import           Data.Aeson
import qualified Data.Map               as Map
import           Katip
import           Protolude              hiding (error)


-- |Basic logging interface
class Logger m where

   {-# MINIMAL doLog, redactCustom, pushSpan, popSpan #-}

   -- | Low-level logging interface, to implement by instances
   doLog :: (ToJSON a)
         => Text
         -> Severity
         -> a
         -> m ()

   -- | Request redacting all given fields in given logging environment
   redact :: Text
          -> Environment
          -> m ()
   redact field = redactCustom field ("REDACTED" :: Text)

   -- | Request redacting given field in given logging environment with custom value
   redactCustom :: (ToJSON a)
          => Text
          -> a
          -> Environment
          -> m ()

   -- |Log some item at `ErrorS` severity level
   error :: (ToJSON a)
         => Text
         -- ^An identifier for this log event
         -> a
         -- ^Data associated with this event
         -> m ()
   error id = doLog id ErrorS

   -- |Log some item at `WarningS` severity level
   warning :: (ToJSON a)
         => Text
         -- ^An identifier for this log event
         -> a
         -- ^Data associated with this event
         -> m ()
   warning id = doLog id WarningS

   -- |Log some item at `DebugS` severity level
   debug :: (ToJSON a)
         => Text
         -- ^An identifier for this log event
         -> a
         -- ^Data associated with this event
         -> m ()
   debug id = doLog id DebugS

   -- |Log some item at `InfoS` severity level
   info :: (ToJSON a)
         => Text
         -- ^An identifier for this log event
         -> a
         -- ^Data associated with this event
         -> m ()
   info id = doLog id InfoS

   -- | Starts a new 'span'
   -- A span identifies some sequence in the stream of logging with hierarchical
   -- names
   pushSpan :: Text
            -> m ()

   -- | Ends current span, if any
   -- `popSpan` must always be paired with a corresponding `pushSpan`.
   popSpan :: m ()


instance Logger Identity where
  doLog _ _  _      = pure ()
  redactCustom _ _  = const $ pure ()
  pushSpan          = const $ pure ()
  popSpan           = pure ()

instance (Monad m, Logger m) => Logger (ReaderT s m) where
  doLog        id s = lift . doLog id s
  redactCustom t l  = lift . redactCustom t l
  pushSpan          = lift . pushSpan
  popSpan           = lift popSpan

instance (Monad m, Logger m) => Logger (ExceptT s m) where
  doLog        id s = lift . doLog id s
  redactCustom t l  = lift . redactCustom t l
  pushSpan          = lift . pushSpan
  popSpan           = lift popSpan

instance (Monad m, Logger m) => Logger (StateT s m) where
  doLog        id s = lift . doLog id s
  redactCustom t l  = lift . redactCustom t l
  pushSpan          = lift . pushSpan
  popSpan           = lift popSpan

instance {-# OVERLAPPABLE #-} (Monad m, Logger m, Monoid s) => Logger (WriterT s m) where
  doLog        id s = lift . doLog id s
  redactCustom t l  = lift . redactCustom t l
  pushSpan          = lift . pushSpan
  popSpan           = lift popSpan


-- | Build simple loggable objects dynamically.
-- Associates a `Text`ual key with a JSON-like value. The returned value can be composed
-- with another object using standard monoidal composition operator `(<>)`:
--
-- @
-- info $ "key" =: "aKey" <> "value" =: aValue
-- @
(=:) :: (ToJSON a)
    => Text
    -- ^Tag name
    -> a
    -- ^Arbitrary JSONable value attached to the tag
    -> SimpleLogPayload
    -- ^A payload that can be further composed with other tags using `(<>)`

(=:) = sl


-- | The environment in which to run the logger.
--
-- This is currently a thin wrapper over Katip's `LogEnv.`
data LoggerEnv = LoggerEnv { _rewriter  :: Map.Map Environment (Endo Value)
                           , _katipEnv  :: LogEnv
                           , _namespace :: Namespace
                           }

pushNamespace :: Text -> LoggerEnv -> LoggerEnv
pushNamespace ns e@LoggerEnv{_namespace} = e{ _namespace = _namespace <> Namespace [ns]}


popNamespace :: LoggerEnv -> LoggerEnv
popNamespace e@LoggerEnv{_namespace} = e { _namespace = Namespace $ dropLast (unNamespace _namespace) }
    where
      dropLast []     = []
      dropLast [_]    = []
      dropLast (x:xs) = x : dropLast xs


-- | Default `Severity` for new environments.
defaultSeverity = DebugS

-- | Default `Verbosity` for new environments.
defaultVerbosity = V3

-- | Default `Namepace` for new environments.
defaultNamespace = Namespace [ "Symbiont" ]

-- | Default Deployment `Environment` name.
defaultDeploymentEnvironment = Environment "prod"

