{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Framework.Providers.DocTest.Prime (
    docTest
  ) where

import Test.DocTest
import Test.Framework.Providers.API
import Data.List (intercalate)

data DocTestRunning = DocTestRunning

instance Show DocTestRunning where
    show DocTestRunning = "Running"

instance Show Result where
    show r
      | doctestSucceeded r = "OK, " ++ show (rExamples r) ++ " exmaples (" ++ show (rInteractions r) ++" interactions)"
      | otherwise          = intercalate "\n" $ rFailureMessages r

instance TestResultlike DocTestRunning Result where
    testSucceeded = doctestSucceeded

doctestSucceeded :: Result -> Bool
doctestSucceeded = isSucceeded

instance Testlike DocTestRunning Result DocTest where
    runTest = runDocTest
    testTypeName _ = "DocTest"

newtype DocTest = DocTest (IO Result)

docTest :: [String] -> [String] -> IO Test
docTest files opts = return $ Test (testName files) (DocTest doctest)
  where
    testName []    = "DocTest"
    testName (f:_) = "DocTest " ++ f
    doctest = do
        modules <- getDocTests opts files
        withInterpreter (opts ++ files) $ \repl -> -- FIXME
            runModules repl False modules

runDocTest :: CompleteTestOptions -> DocTest -> IO (DocTestRunning :~> Result, IO ())
runDocTest topts (DocTest doctest) = runImprovingIO $ do
    yieldImprovement DocTestRunning
    mb_result <- maybeTimeoutImprovingIO (unK $ topt_timeout topts) $ liftIO doctest
    return (mb_result `orElse` errorResult)
    
errorResult :: Result
errorResult = Result 0 0 0 0 0 []

