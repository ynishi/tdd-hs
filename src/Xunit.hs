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
  setUp x = return x {wasRunLog = "setUp "}

data WasRun
  = WasRun { testCase  :: TestCase
           , wasRunLog :: String }
  | EmptyWasRun

makeWasRun :: Name -> WasRun
makeWasRun name = WasRun (TestCase name) ""

testMethod :: WasRun -> (WasRun -> IO WasRun)
testMethod _ = \x -> return x {wasRunLog = (wasRunLog x) ++ "testMethod "}

data TestCaseTest = TestCaseTest
  { tCTName   :: Name
  , tCTWasRun :: WasRun
  }

instance TC TestCaseTest where
  method t@(TestCaseTest x _) =
    case x of
      "testTemplateMethod" -> testTemplateMethod t
  setUp t = do
    let newWasRun = makeWasRun "testMethod"
    return t {tCTWasRun = newWasRun}

testTemplateMethod _ =
  \x -> do
    setUpped <- run $ tCTWasRun x
    assert ("setUp testMethod " == wasRunLog setUpped) dummy
    return x {tCTWasRun = setUpped}

dummy = putStr ""

main = do
  run $ TestCaseTest "testTemplateMethod" EmptyWasRun
