module Xunit where

data TestCase = TestCase

data WasRun =
  WasRun TestCase String
         (WasRun -> IO WasRun)
         Bool

wasRun :: WasRun -> Bool
wasRun (WasRun TestCase _ _ x) = x

testMethod :: WasRun -> IO WasRun
testMethod (WasRun TestCase s f _) = return $ WasRun TestCase s f True

method :: WasRun -> (WasRun -> IO WasRun)
method (WasRun TestCase _ f _) = f

run :: WasRun -> IO WasRun
run x = method x $ x

test = WasRun TestCase "testMethod" testMethod False

main = do
  print $ wasRun test
  tested <- run test
  print $ wasRun tested
