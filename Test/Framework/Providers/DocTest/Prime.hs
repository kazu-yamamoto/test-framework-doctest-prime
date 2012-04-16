{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Test framework provider for doctest.

module Test.Framework.Providers.DocTest.Prime (
    DocTest
  , docTest
  ) where

import Data.List (intercalate)
import Test.DocTest
import Test.Framework.Providers.API

----------------------------------------------------------------

-- | Type for doctest
type DocTest = IO Test

-- |
-- Gathering tests from Haddock documentation of functions.
docTest :: [String] -- ^ File names
        -> [String] -- ^ GHC options
        -> DocTest
docTest files options = do
    modules <- getDocTests options files
    tests <- mapM (moduleToTest opts) modules
    return $ TestGroup "DocTest" tests
 where
    opts = options ++ files

moduleToTest :: [String] -> Module Example -> IO Test
moduleToTest opts mdl = do
    i <- newInterpreter opts
    return $ TestGroup name (tests i)
  where
    name = moduleName mdl
    examples = moduleContent mdl
    others   = init examples
    sentinel = last examples
    tests i  = map (exampleToTest i name) others
            ++ [exampleToTest' i name sentinel]

exampleToTest :: Interpreter -> String -> Example -> Test
exampleToTest i name example = Test (exampleLabel example) . DocTestAction $
    runExample i name example

exampleToTest' :: Interpreter -> String -> Example -> Test
exampleToTest' i name example = Test (exampleLabel example) . DocTestAction $ do
    res <- runExample i name example
    closeInterpreter i
    return res

----------------------------------------------------------------

data DocTestRunning = DocTestRunning

instance Show DocTestRunning where
    show DocTestRunning = "Running"

----------------------------------------------------------------

instance Show InteractionResult where
    show Success = "OK"
    show (Failure loc expression expected actual) = loc ++ ": expression `" ++ expression ++ "'\nexpected: " ++ (intercalate "\n" expected) ++ "\n but got: " ++ (intercalate "\n" actual)
    show (Error loc expression err) = loc ++ ": expression `" ++ expression ++ "'\nError: " ++ err

----------------------------------------------------------------

instance TestResultlike DocTestRunning InteractionResult where
    testSucceeded = doctestSucceeded

doctestSucceeded :: InteractionResult -> Bool
doctestSucceeded Success = True
doctestSucceeded _       = False

----------------------------------------------------------------

newtype DocTestAction = DocTestAction (IO InteractionResult)

instance Testlike DocTestRunning InteractionResult DocTestAction where
    runTest = runDocTest
    testTypeName _ = "DocTest"

runDocTest :: CompleteTestOptions -> DocTestAction -> IO (DocTestRunning :~> InteractionResult, IO ())
runDocTest topts (DocTestAction doctest) = runImprovingIO $ do
    yieldImprovement DocTestRunning
    mb_result <- maybeTimeoutImprovingIO (unK $ topt_timeout topts) $ liftIO doctest
    return (mb_result `orElse` Success)
