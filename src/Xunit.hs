module Xunit where

import           Control.Exception

type Name = String

data TestCase =
  TestCase Name

class TC a where
  run :: a -> IO a
  run x = return $ (method x) x
  method :: a -> (a -> a)

instance TC WasRun where
  method = testMethod

data WasRun = WasRun
  { testCase :: TestCase
  , wasRun   :: Bool
  }

testMethod :: WasRun -> (WasRun -> WasRun)
testMethod x = (\(WasRun _ _) -> WasRun (testCase x) True)

test = WasRun (TestCase "testMethod") False

dummy = putStr ""

main = do
  assert (not . wasRun $ test) dummy
  tested <- run test
  assert (wasRun tested) dummy
