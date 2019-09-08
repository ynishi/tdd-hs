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
testMethod x = (\(WasRun _ _) -> return $ WasRun (testCase x) True)

test = WasRun (TestCase "testMethod") False

data TestCaseTest =
  TestCaseTest Name

instance TC TestCaseTest where
  method = testRunning

testRunning x =
  \_ -> do
    assert (not . wasRun $ test) dummy
    tested <- run test
    assert (wasRun tested) dummy
    return x

dummy = putStr ""

main = run $ TestCaseTest "testRunning"
