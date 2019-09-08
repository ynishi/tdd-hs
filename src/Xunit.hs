module Xunit where

type Name = String

data TestCase =
  TestCase Name

data WasRun =
  WasRun TestCase
         (WasRun -> IO WasRun)
         Bool

wasRun :: WasRun -> Bool
wasRun (WasRun _ _ x) = x

testMethod :: WasRun -> IO WasRun
testMethod (WasRun t f _) = return $ WasRun t f True

method :: WasRun -> (WasRun -> IO WasRun)
method (WasRun _ f _) = f

run :: WasRun -> IO WasRun
run x = method x $ x

test = WasRun (TestCase "testMethod") testMethod False

main = do
  print $ wasRun test
  tested <- run test
  print $ wasRun tested
