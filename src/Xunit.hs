module Xunit where

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

main = do
  print $ wasRun test
  tested <- run test
  print $ wasRun tested
