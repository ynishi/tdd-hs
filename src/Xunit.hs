module Xunit where

import           Control.Exception
import           Control.Monad

type Name = String

data TestCase =
  TestCase Name

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

data TestSuite a =
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
  method w =
    case testCase w of
      TestCase "testMethod"       -> testMethod w
      TestCase "testBrokenMethod" -> testBrokenMethod w
  setUp x = return x {wasRunLog = "setUp "}
  tearDown x = return x {wasRunLog = wasRunLog x ++ "tearDown "}

data WasRun = WasRun
  { testCase  :: TestCase
  , wasRunLog :: String
  }

makeWasRun :: Name -> WasRun
makeWasRun name = WasRun (TestCase name) ""

testMethod :: WasRun -> (WasRun -> IO WasRun)
testMethod _ = \x -> return x {wasRunLog = wasRunLog x ++ "testMethod "}

testBrokenMethod :: WasRun -> (WasRun -> IO WasRun)
testBrokenMethod _ = \x -> assert False (return x)

data TestCaseTest = TestCaseTest
  { tCTName :: Name
  }

instance TC TestCaseTest where
  method t@(TestCaseTest x) =
    case x of
      "testTemplateMethod"         -> testTemplateMethod t
      "testResult"                 -> testResult t
      "testFailedResult"           -> testFailedResult t
      "testFailedResultFormatting" -> testFailedResultFormatting t
      "testSuite"                  -> testSuite t
  setUp = return

testTemplateMethod _ =
  \x -> do
    let test = makeWasRun "testMethod"
    tested <- run test $ makeTestResult
    assert ("setUp testMethod tearDown " == (wasRunLog . fst $ tested)) dummy
    return x

testResult _ =
  \x -> do
    let test = makeWasRun "testMethod"
    result <- run test $ makeTestResult
    assert ("1 run, 0 failed" == (summary . snd $ result)) dummy
    return x

testFailedResult _ =
  \x -> do
    let test = makeWasRun "testBrokenMethod"
    result <- run test $ makeTestResult
    assert ("1 run, 1 failed" == (summary . snd $ result)) dummy
    return x

testFailedResultFormatting _ =
  \x -> do
    let result = makeTestResult
    let startedResult = testStarted result
    let failedResult = testFailed startedResult
    assert ("1 run, 1 failed" == summary failedResult) dummy
    return x

testSuite _ =
  \x -> do
    let suite = TestSuite [] :: TestSuite WasRun
    let suite1 = suiteAdd suite $ makeWasRun "testMethod"
    let suite2 = suiteAdd suite1 $ makeWasRun "testBrokenMethod"
    let result = makeTestResult
    suiteRunResult <- suiteRun suite2 result
    assert ("2 run, 1 failed" == summary suiteRunResult) dummy
    return x

dummy = putStr ""

main = do
  let suite = TestSuite [] :: TestSuite TestCaseTest
  let suite1 = suiteAdd suite $ TestCaseTest "testTemplateMethod"
  let suite2 = suiteAdd suite1 $ TestCaseTest "testResult"
  let suite3 = suiteAdd suite2 $ TestCaseTest "testFailedResult"
  let suite4 = suiteAdd suite3 $ TestCaseTest "testFailedResultFormatting"
  let suite5 = suiteAdd suite4 $ TestCaseTest "testSuite"
  let result = makeTestResult
  runnedResult <- suiteRun suite5 result
  putStrLn $ summary runnedResult
