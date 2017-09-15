{-| A poor man's version of Tasty `Ingredient` to tag and filter categories of test

This simple module provides elements to ''tag'' some `TestTree` in a Tasty test suite
and filter the tests that should be executed depending on command-line arguments.

An example use, from a toplevel `Test` module:

@
main :: IO ()
main = do
  (args, testTypes) <- getArgs >>= selectTestTypesToRun >>= withDefaultTypes [ Unit, QuickCheck ]
  withArgs args $ do

    consistencySpec       <- testSpecs TestConsistency.spec
    networkSystem         <- testSpecs WiredNetwork.spec
    realLedgerClientSpec  <- testSpecs RealLedgerClient.systemSpec

    specs <- traverse testSpecs [ Unit.spec
                                , TestBasicFlow.spec
                                , TestBasicNetwork.spec
                                , SailfishExecutorTest.spec
                                , PublicApiHttp.spec
                                , RDCP.spec
                                , SailfishJson.spec
                                , SyncDB.spec
                                , RealLedgerClient.spec
                                -- todo : fix reset logic, #915
                                --, Reset.spec
                                ]

    let classifiedTests = [ (Unit, testGroup "Unit Tests" $ unitTests)
                          , (QuickCheck, testGroup "QuickCheck Tests" consistencySpec)
                          , (System, testGroup "System Tests" [ testGroup "Docker Tests" networkSystem
                                                              , testGroup "Local Server Tests" $ mconcat [ realLedgerClientSpec ]
                                                              ])
                          ]
        testsToRun = classifiedTests `pruneNotRunnableTests` testTypes

    defaultMainWithIngredients (antXMLRunner:defaultIngredients) $
      testGroup "TXE All Tests" testsToRun
@

To run all `Unit` and `QuickCheck` tests, simply execute the test suite:

@
$ stack test txe
@

To run `System` tests (only):

@
$ stack test txe --test-arguments='--system'
@

To run `System` and `QuickCheck` tests:

@
$ stack test txe --test-arguments='--system --qc'
@

Available command-line flags are :

 * @--unit@ to select `Unit` tests
 * @--qc@ to select `QuickCheck` tests
 * @--system@ to select `System` tests

TODO: Turn this into a genuine `Ingredient` using <Tasty http://hackage.haskell.org/package/tasty-0.11.2.5/docs/Test-Tasty-Ingredients.html> API
-}
module Symbiont.TestTypes
  ( -- * Types of Tests
    TestTypes (..)
    -- * Reexports for convenience
  , withArgs
    -- * Test selectors
  , pruneNotRunnableTests, selectTestTypesToRun, withDefaultTypes
  )where

import           Data.String
import           Protolude
import           System.Environment (withArgs)

-- * Test Types
data TestTypes = Unit
               | QuickCheck
               | System
               deriving (Eq, Show)

-- | Select elements from `tests` that are tagged with element in `types`
pruneNotRunnableTests :: [ ( TestTypes, a ) ]
                      -> [ TestTypes ]
                      -> [ a ]

pruneNotRunnableTests tests types  =
  map snd $ filter ((`elem` types) . fst) tests

-- | Select types of test to run based on given list of arguments and returns the
-- list pruned of used arguments
selectTestTypesToRun :: [ String ]
                     -> IO ([ String ], [ TestTypes ])

selectTestTypesToRun []          = pure ([], [])
selectTestTypesToRun (arg:rest) = do
  (args, tts) <- selectTestTypesToRun rest
  pure $ classify arg args tts
  where
    classify "--unit"   args tts = (args, Unit:tts)
    classify "--qc"     args tts = (args, QuickCheck:tts)
    classify "--system" args tts = (args, System:tts)
    classify other      args tts = (other:args, tts)

-- | Add default test types to run if none is selected
-- This function is intended to be used in conjunction with `selectTestTypesToRun` to
-- provide some sensible defaults if user does not select explicitly type of tests to
-- run with command-line flags.
withDefaultTypes :: [ TestTypes ]
                 -> ([ String ], [ TestTypes ])
                 -> IO ([ String ], [ TestTypes ])

withDefaultTypes defs (args, [])  = return (args, defs)
withDefaultTypes _    (args, tts) =return (args, tts)

