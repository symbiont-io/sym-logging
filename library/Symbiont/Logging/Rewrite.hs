module Symbiont.Logging.Rewrite where

import           Control.Lens         hiding ((&))
import           Control.Monad.Writer hiding ((<>))
import           Data.Aeson
import           Data.Aeson.Lens
import           Protolude            hiding (error)


-- * Rewriter

-- | Replace a single field of a JSON value with the string `REDACTED`.
-- If field is not present in the object nor any of its subtree, it is left untouched.
--
rewrite :: Text
        -- ^The field to rewrite
        -> Endo Value

rewrite field = Endo (& deep (key field) .~ "REDACTED")


-- | Replace a single field of a JSON value with a custom value.
rewriteCustom :: (ToJSON a)
              => Text
              -- ^The field to rewrite
              -> a
              -> Endo Value

rewriteCustom field value = Endo (& deep (key field) .~ toJSON value)
