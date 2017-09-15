{-| A thin wrapper over <Katip https://hackage.haskell.org/package/katip>'s logging infrastructure.

Simple usage looks like:
2
@
myaction = do
  env <- defaultEnvironment stdout
  runIOLogger env $ do
     info "somevalue" someDataConvertibleToJSON
     ...
     error "someerror" someError
  closeEnvironment env
@

This creates a logging environment with `defaultNamespace` and `defaultDeploymentEnvironment`
names, respectively @["Symbiont"]@ and @"prod"@, ouputting JSON-formatted data to `stdout`.

This library provide some more features:

 * Users can request `redact`ing some fields of the logged items within some deployment environment. This will replace the content of the field with the string @"REDACTED"@,
   or alternatively with some custom JSON value if using `redactCustom`,

 * Logged items are expected to be data structures implementing `Data.Aeson.ToJSON` typeclass. The default `Scribe` outputs this JSON data as-is to the given `Handle`,

 * When using `IOLogger` the user is responsible for the lifecycle of the `Handle` which is used for logging, e.g. ensuring proper termination and flushing all buffers at exit,

 * All writes are serialized.

Here is a sample output:

@
{"at":"2017-06-25T18:16:19.813226Z","env":"prod","ns":["Symbiont","object"],"data":{"object":{"foo":12,"bar":"XXXXXXXX"}},"app":["Symbiont"],"msg":"","pid":"56573","loc":null,"host":"Arnauds-MacBook-Pro.local","sev":"Warning","thread":"ThreadId 24"}
{"at":"2017-06-25T18:16:19.813226Z","env":"prod","ns":["Symbiont","object"],"data":{"object":{"foo":12,"bar":"XXXXXXXX"}},"app":["Symbiont"],"msg":"","pid":"56573","loc":null,"host":"Arnauds-MacBook-Pro.local","sev":"Info","thread":"ThreadId 24"}
{"at":"2017-06-25T18:16:19.813226Z","env":"prod","ns":["Symbiont","object"],"data":{"object":{"foo":12,"bar":"XXXXXXXX"}},"app":["Symbiont"],"msg":"","pid":"56573","loc":null,"host":"Arnauds-MacBook-Pro.local","sev":"Error","thread":"ThreadId 24"}
{"at":"2017-06-25T18:16:19.813226Z","env":"prod","ns":["Symbiont","object"],"data":{"object":{"foo":12,"bar":"XXXXXXXX"}},"app":["Symbiont"],"msg":"","pid":"56573","loc":null,"host":"Arnauds-MacBook-Pro.local","sev":"Debug","thread":"ThreadId 24"}
@

-}
module Symbiont.Logging (
  -- * Types
  -- ** From Katip
  Severity(..), (=:),Verbosity(..),
  Namespace(..), Environment(..),
  -- ** Base class
  Logger,
  -- ** Logger configuration
  LoggerEnv, IOLogger,
  -- * Logging
  -- ** Logging statements
  doLog, error, warning, debug, info,
  -- ** Rewriting logic
  redact, redactCustom,
  -- ** Spans
  pushSpan, popSpan, pushNamespace, popNamespace,
  -- * Loggers
  runIOLogger, defaultEnvironment, namespacedEnvironment, nullEnvironment, closeEnvironment
  ) where


import           Katip
import           Symbiont.Logging.IO
import           Symbiont.Logging.Types

