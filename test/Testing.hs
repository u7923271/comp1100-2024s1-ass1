module Testing where

import System.Exit
import System.Timeout
import Control.Exception
import Data.Maybe

-- | A 'Test' is a 'String' label identifying the thing being tested,
-- and the result of the test after running it. We give labels to
-- tests so we have some idea what they mean.
data Test = Test String TestResult

-- | A test either passes, or fails with an error message.
data TestResult = OK | Fail String

-- | Test that two things are equal. The first argument is the
-- computed result, the second argument is the value it should be
-- equal to.
assertEqual :: (Eq a, Show a) => a -> a -> TestResult
assertEqual actual expected
  | actual == expected = OK
  | otherwise = Fail (show actual ++ " is not equal to\n" ++ show expected)

-- | Test that two things are different. The first argument is the
-- computed result, the second argument is the value it should be
-- different from.
assertNotEqual :: (Eq a, Show a) => a -> a -> TestResult
assertNotEqual actual expected
  | actual /= expected = OK
  | otherwise = Fail (show actual ++ " is equal to\n" ++ show expected)

-- | Test that two 'Double's are basically equal. The first argument
-- is the computed result, the second argument is the value it should be
-- close to.
assertApproxEqual :: Double -> Double -> TestResult
assertApproxEqual actual expected
  | abs (actual - expected) < 0.0001 = OK
  | otherwise =
    Fail (show actual ++ " is not approx. equal to\n" ++ show expected)

-- | Safely evaluates a test and returns if it is passing.
-- Prints information about which tests pass and which fail.
-- You are not expected to understand how this works.
runTest :: Test -> IO Bool
runTest (Test name result) = do
  res <- try $ evaluate result :: IO (Either SomeException TestResult)
  (success,text) <- return $ case res of
    Left e                -> (False, "ERROR: " <> name <> "\n" <> show e)
    Right (Fail failMsg)  -> (False, "FAIL: " <> name <> "\n" <> failMsg)
    Right OK              -> (True, "PASS: " <> name)
  putStrLn text
  return success

-- | Adds a timeout wrapper around the test being executed, which results in an
-- error being thrown if timeout is exceeded, which is caught by `runTest`.
-- Takes a timeout in seconds and a test case to evaluate.
-- You are not expected to understand how this works.
runTestTimeout :: Int -> Test -> IO Bool
runTestTimeout time test = do
  res <- timeout (time*10^(6::Int)) (runTest test)
  return $ fromMaybe False res

-- | Run a list of tests safely using runTest
-- You are not expected to understand how this works.
runTests :: [Test] -> IO ()
runTests tests = do
  res <- mapM (runTestTimeout 5) tests
  let exit | and res = exitSuccess
           | otherwise = exitFailure
  exit