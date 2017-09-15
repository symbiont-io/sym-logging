
import           Protolude
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Runners.AntXML (antXMLRunner)

import qualified Symbiont.Logging.Test     as LoggingTest
import           Symbiont.TestTypes

main :: IO ()
main = do
  (args, testTypes) <- getArgs >>= selectTestTypesToRun >>= withDefaultTypes [ Unit, QuickCheck ]

  tests <- testSpec "Logging Tests" LoggingTest.spec

  let classifiedTests = [ (Unit, testGroup "Unit Tests" [ tests ]) ]
      testsToRun      = classifiedTests `pruneNotRunnableTests` testTypes

  withArgs args $ do
    defaultMainWithIngredients (antXMLRunner:defaultIngredients) $ testGroup "Logging tests" testsToRun
