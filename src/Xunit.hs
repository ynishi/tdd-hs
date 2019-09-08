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
  { tCTName :: Name
  }

instance TC TestCaseTest where
  method t@(TestCaseTest x) =
    case x of
      "testTemplateMethod" -> testTemplateMethod t
  setUp = return

testTemplateMethod _ =
  \x -> do
    let test = makeWasRun "testMethod"
    setUpped <- run test
    assert ("setUp testMethod " == wasRunLog setUpped) dummy
    return x

dummy = putStr ""

main = do
  run $ TestCaseTest "testTemplateMethod"
