module Xunit where

import           Control.Exception

type Name = String

data TestCase =
  TestCase Name

class TC a where
  run :: a -> IO a
  run x = do
    setUpped <- setUp x
    method setUpped $ setUpped
  setUp :: a -> IO a
  method :: a -> (a -> IO a)

instance TC WasRun where
  method = testMethod
  setUp x = return x {wasRun = False, wasRunLog = "setUp "}

data WasRun
  = WasRun { testCase :: TestCase
           , wasRun   :: Bool
           , wasSetUp :: Bool
           , wasRunLog :: String
           }
  | EmptyWasRun

makeWasRun :: Name -> WasRun
makeWasRun name = WasRun (TestCase name) False False ""

testMethod :: WasRun -> (WasRun -> IO WasRun)
testMethod _ = \x -> return x {wasRun = True, wasSetUp = True}

data TestCaseTest = TestCaseTest
  { tCTName   :: Name
  , tCTWasRun :: WasRun
  }

instance TC TestCaseTest where
  method t@(TestCaseTest x _) =
    case x of
      "testRunning" -> testRunning t
      "testSetUp"   -> testSetUp t
  setUp t = do
    let newWasRun = makeWasRun "testMethod"
    return t {tCTWasRun = newWasRun}

testRunning _ =
  \x -> do
    tested <- run $ tCTWasRun x
    assert (wasRun tested) dummy
    return x {tCTWasRun = tested}

testSetUp _ =
  \x -> do
    setUpped <- run $ tCTWasRun x
    assert ("setUp " == wasRunLog setUpped) dummy
    return x {tCTWasRun = setUpped}

dummy = putStr ""

main = do
  run $ TestCaseTest "testRunning" EmptyWasRun
  run $ TestCaseTest "testSetUp" EmptyWasRun
