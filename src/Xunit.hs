module Xunit where

import           Control.Exception
import           Control.Monad

newtype TestCase a =
  TestCase (a -> IO a)

class TC a where
  run :: a -> TestResult -> IO (a, TestResult)
  run x result = do
    let startedResult = testStarted result
    setUpped <- setUp x
    (tested, testedResult) <-
      catch
        (do tested <- method setUpped setUpped
            return (tested, startedResult))
        (\(SomeException _) -> return (setUpped, testFailed startedResult))
    tearDowned <- tearDown tested
    return (tearDowned, testedResult)
  setUp :: a -> IO a
  setUp = return
  method :: a -> (a -> IO a)
  tearDown :: a -> IO a
  tearDown = return

newtype TestSuite a =
  TestSuite [a]

suiteAdd :: TC a => TestSuite a -> a -> TestSuite a
suiteAdd (TestSuite cs) c = TestSuite $ cs ++ [c]

suiteRun :: TC a => TestSuite a -> TestResult -> IO TestResult
suiteRun (TestSuite cs) result =
  foldM
    (\r c -> do
       r2 <- run c r
       return . snd $ r2)
    result
    cs

data TestResult = TestResult
  { trRunCount   :: Int
  , trErrorCount :: Int
  }

makeTestResult = TestResult 0 0

testStarted :: TestResult -> TestResult
testStarted t = t {trRunCount = trRunCount t + 1}

testFailed :: TestResult -> TestResult
testFailed t = t {trErrorCount = trErrorCount t + 1}

summary :: TestResult -> String
summary t =
  (show . trRunCount $ t) ++ " run, " ++ (show . trErrorCount $ t) ++ " failed"

instance TC WasRun where
  method (WasRun (TestCase f) _) = f
  setUp x = return x {wasRunLog = "setUp "}
  tearDown x = return x {wasRunLog = wasRunLog x ++ "tearDown "}

data WasRun = WasRun
  { testCase  :: TestCase WasRun
  , wasRunLog :: String
  }

makeWasRun :: (WasRun -> IO WasRun) -> WasRun
makeWasRun f = WasRun (TestCase f) ""

testMethod :: (WasRun -> IO WasRun)
testMethod x = return x {wasRunLog = wasRunLog x ++ "testMethod "}

testBrokenMethod :: (WasRun -> IO WasRun)
testBrokenMethod = assert False . return

data TestCaseTest = TestCaseTest
  { tCTF      :: TestCaseTest -> IO TestCaseTest
  , tCTResult :: TestResult
  }

instance TC TestCaseTest where
  method (TestCaseTest x _) = x
  setUp t = return $ t {tCTResult = makeTestResult}

makeTestCaseTest f = TestCaseTest f makeTestResult

testTemplateMethod x = do
  let test = makeWasRun testMethod
  tested <- run test $ tCTResult x
  assert ("setUp testMethod tearDown " == (wasRunLog . fst $ tested)) dummy
  return x

testResult x = do
  let test = makeWasRun testMethod
  result <- run test $ tCTResult x
  assert ("1 run, 0 failed" == (summary . snd $ result)) dummy
  return x

testFailedResult x = do
  let test = makeWasRun testBrokenMethod
  result <- run test $ tCTResult x
  assert ("1 run, 1 failed" == (summary . snd $ result)) dummy
  return x

testFailedResultFormatting x = do
  let result = tCTResult x
  let startedResult = testStarted result
  let failedResult = testFailed startedResult
  assert ("1 run, 1 failed" == summary failedResult) dummy
  return x

testSuite x = do
  let suite = TestSuite [] :: TestSuite WasRun
  let suite1 = suiteAdd suite $ makeWasRun testMethod
  let suite2 = suiteAdd suite1 $ makeWasRun testBrokenMethod
  suiteRunResult <- suiteRun suite2 $ tCTResult x
  assert ("2 run, 1 failed" == summary suiteRunResult) dummy
  return x

dummy = putStr ""

main = do
  result <- suiteRun suite makeTestResult
  putStrLn $ summary result
  where
    suite =
      TestSuite . map makeTestCaseTest $
      [ testTemplateMethod
      , testResult
      , testFailedResult
      , testFailedResultFormatting
      , testSuite
      ]
