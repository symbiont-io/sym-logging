{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Symbiont.Logging.Katip (
    Katip
  , ls
  , logCritical
  , logError
  , logWarning
  , logNotice
  , logInfo
  , logDebug
  , logBracket
  , Severity(..)
  ) where


import           Katip
import           Protolude


logCritical
  :: Katip m
  => LogStr
  -> m ()
logCritical = logSym CriticalS


logError
  :: Katip m
  => LogStr
  -> m ()
logError    = logSym ErrorS


logWarning
  :: Katip m
  => LogStr
  -> m ()
logWarning  = logSym WarningS


logNotice
  :: Katip m
  => LogStr
  -> m ()
logNotice = logSym NoticeS


logInfo
  :: Katip m
  => LogStr
  -> m ()
logInfo = logSym InfoS


logDebug
  :: Katip m
  => LogStr
  -> m ()
logDebug = logSym DebugS



logSym
  :: Katip m
  => Severity
  -> LogStr
  -> m ()
logSym = logMsg mempty


logBracket
  :: Katip m
  => Severity
  -> Text
  -> m a
  -> m a
logBracket severity context action = do
  logSym severity . ls $ context <> "..."
  x <- action
  logSym severity . ls $ "...done " <> context
  pure x
