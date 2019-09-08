module Xunit where

import           Control.Exception

type Name = String

data TestCase =
  TestCase Name

class TC a where
  run :: a -> IO (a, TestResult)
  run x = do
    let result = TestResult 0 0
    let startedResult = testStarted result
    setUpped <- setUp x
    tested <- method setUpped $ setUpped
    tearDowned <- tearDown tested
    return (tearDowned, startedResult)
  setUp :: a -> IO a
  setUp = return
  method :: a -> (a -> IO a)
  tearDown :: a -> IO a
  tearDown = return

data TestResult = TestResult
  { trRunCount   :: Int
  , trErrorCount :: Int
  }

testStarted :: TestResult -> TestResult
testStarted t = t {trRunCount = (trRunCount t) + 1}

testFailed :: TestResult -> TestResult
testFailed t = t {trErrorCount = (trErrorCount t) + 1}

summary :: TestResult -> String
summary t =
  (show . trRunCount $ t) ++ " run, " ++ (show . trErrorCount $ t) ++ " failed"

instance TC WasRun where
  method w =
    case testCase w of
      TestCase "testMethod"       -> testMethod w
      TestCase "testBrokenMethod" -> testBrokenMethod w
  setUp x = return x {wasRunLog = "setUp "}
  tearDown x = return x {wasRunLog = (wasRunLog x) ++ "tearDown "}

data WasRun = WasRun
  { testCase  :: TestCase
  , wasRunLog :: String
  }

makeWasRun :: Name -> WasRun
makeWasRun name = WasRun (TestCase name) ""

testMethod :: WasRun -> (WasRun -> IO WasRun)
testMethod _ = \x -> return x {wasRunLog = (wasRunLog x) ++ "testMethod "}

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
  setUp = return

testTemplateMethod _ =
  \x -> do
    let test = makeWasRun "testMethod"
    tested <- run test
    assert ("setUp testMethod tearDown " == (wasRunLog . fst $ tested)) dummy
    return x

testResult _ =
  \x -> do
    let test = makeWasRun "testMethod"
    result <- run test
    assert ("1 run, 0 failed" == (summary . snd $ result)) dummy
    return x

testFailedResult _ =
  \x -> do
    let test = makeWasRun "testBrokenMethod"
    result <- run test
    assert ("1 run, 1 failed" == (summary . snd $ result)) dummy
    return x

testFailedResultFormatting _ =
  \x -> do
    let result = TestResult 0 0
    let startedResult = testStarted result
    let failedResult = testFailed startedResult
    assert ("1 run, 1 failed" == summary failedResult) dummy
    return x

dummy = putStr ""

main = do
  run $ TestCaseTest "testTemplateMethod"
  run $ TestCaseTest "testResult"
  -- run $ TestCaseTest "testFailedResult"
  run $ TestCaseTest "testFailedResultFormatting"
