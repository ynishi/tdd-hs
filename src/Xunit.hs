module Xunit where

import           Control.Exception

type Name = String

data TestCase =
  TestCase Name

class TC a where
  run :: a -> IO (a, TestResult)
  run x = do
    let result = TestResult 0
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
  { trRunCount :: Int
  }

testStarted :: TestResult -> TestResult
testStarted (TestResult x) = TestResult (x + 1)

summary :: TestResult -> String
summary (TestResult x) = (show x) ++ " run, 0 failed"

instance TC WasRun where
  method = testMethod
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

data TestCaseTest = TestCaseTest
  { tCTName :: Name
  }

instance TC TestCaseTest where
  method t@(TestCaseTest x) =
    case x of
      "testTemplateMethod" -> testTemplateMethod t
      "testResult"         -> testResult t
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

dummy = putStr ""

main = do
  run $ TestCaseTest "testTemplateMethod"
  run $ TestCaseTest "testResult"
