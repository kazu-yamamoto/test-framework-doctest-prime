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
docTest files opts = return $ Test (testName files) (DocTestAction doctest)
  where
    testName []    = "DocTest"
    testName (f:_) = "DocTest " ++ f
    doctest = do
        modules <- getDocTests opts files
        withInterpreter (opts ++ files) $ \repl -> -- FIXME
            runModules repl False modules

----------------------------------------------------------------

data DocTestRunning = DocTestRunning

instance Show DocTestRunning where
    show DocTestRunning = "Running"

----------------------------------------------------------------

instance Show Result where
    show r
      | doctestSucceeded r = "OK, " ++ show (rExamples r) ++ " exmaples (" ++ show (rInteractions r) ++" interactions)"
      | otherwise          = intercalate "\n" $ rFailureMessages r

----------------------------------------------------------------

instance TestResultlike DocTestRunning Result where
    testSucceeded = doctestSucceeded

doctestSucceeded :: Result -> Bool
doctestSucceeded = isSucceeded

----------------------------------------------------------------

newtype DocTestAction = DocTestAction (IO Result)

instance Testlike DocTestRunning Result DocTestAction where
    runTest = runDocTest
    testTypeName _ = "DocTest"

runDocTest :: CompleteTestOptions -> DocTestAction -> IO (DocTestRunning :~> Result, IO ())
runDocTest topts (DocTestAction doctest) = runImprovingIO $ do
    yieldImprovement DocTestRunning
    mb_result <- maybeTimeoutImprovingIO (unK $ topt_timeout topts) $ liftIO doctest
    return (mb_result `orElse` errorResult)
    
errorResult :: Result
errorResult = Result 0 0 0 0 0 []
