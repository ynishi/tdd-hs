module Xunit where

import           Control.Exception

type Name = String

data TestCase =
  TestCase Name

class TC a where
  run :: a -> IO a
  run x = method x $ x
  method :: a -> (a -> IO a)

instance TC WasRun where
  method = testMethod

data WasRun = WasRun
  { testCase :: TestCase
  , wasRun   :: Bool
  }

testMethod :: WasRun -> (WasRun -> IO WasRun)
testMethod _ = \x -> return $ WasRun (testCase x) True

data TestCaseTest =
  TestCaseTest Name

instance TC TestCaseTest where
  method t@(TestCaseTest x) =
    case x of
      "testRunning" -> testRunning t
      "testSetUp"   -> testSetUp t

testRunning _ =
  \x -> do
    let test = WasRun (TestCase "testMethod") False
    assert (not . wasRun $ test) dummy
    tested <- run test
    assert (wasRun tested) dummy
    return x

testSetUp _ =
  \x -> do
    let test = WasRun (TestCase "testMethod") False
    tested <- run test
    assert (wasSetUp tested) dummy
    return x

dummy = putStr ""

main = do
  run $ TestCaseTest "testRunning"
  run $ TestCaseTest "testSetUp"
