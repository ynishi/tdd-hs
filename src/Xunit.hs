module Xunit where

import           Control.Exception

type Name = String

data TestCase =
  TestCase Name

class TC a where
  run :: a -> IO a
  run x = do
    setUp x
    method x $ x
  setUp :: a -> IO a
  setUp = return
  method :: a -> (a -> IO a)

instance TC WasRun where
  method = testMethod
  setUp x = return $ x {wasRun = False}

data WasRun = WasRun
  { testCase :: TestCase
  , wasRun   :: Bool
  , wasSetUp :: Bool
  }

makeWasRun :: Name -> WasRun
makeWasRun name = WasRun (TestCase name) False False

testMethod :: WasRun -> (WasRun -> IO WasRun)
testMethod _ = \x -> return $ x {wasRun = True, wasSetUp = True}

data TestCaseTest =
  TestCaseTest Name

instance TC TestCaseTest where
  method t@(TestCaseTest x) =
    case x of
      "testRunning" -> testRunning t
      "testSetUp"   -> testSetUp t

testRunning _ =
  \x -> do
    let test = makeWasRun "testMethod"
    assert (not . wasRun $ test) dummy
    tested <- run test
    assert (wasRun tested) dummy
    return x

testSetUp _ =
  \x -> do
    let test = makeWasRun "testMethod"
    tested <- run test
    assert (wasSetUp tested) dummy
    return x

dummy = putStr ""

main = do
  run $ TestCaseTest "testRunning"
  run $ TestCaseTest "testSetUp"
