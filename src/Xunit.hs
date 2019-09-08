module Xunit where

type Name = String

data TestCase =
  TestCase Name
           (TestCase -> IO WasRun)

run :: TestCase -> IO WasRun
run t = method t $ t

method :: TestCase -> (TestCase -> IO WasRun)
method (TestCase _ f) = f

data WasRun = WasRun
  { testCase :: TestCase
  , wasRun   :: Bool
  }

testMethod :: TestCase -> IO WasRun
testMethod t = return $ WasRun t True

test = WasRun (TestCase "testMethod" testMethod) False

main = do
  print $ wasRun test
  tested <- run . testCase $ test
  print $ wasRun tested
