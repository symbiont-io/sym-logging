
module Symbiont.Logging.Test.Unit where

import           Protolude
import           Test.Tasty.Hspec


spec :: Spec
spec = parallel $
  it "is sane" $
    (1 :: Integer) `shouldBe` (1 :: Integer)
