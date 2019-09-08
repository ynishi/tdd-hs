module Xunit where

data WasRun =
  WasRun String
         Bool

wasRun :: WasRun -> Bool
wasRun (WasRun _ x) = x

testMethod :: WasRun -> IO WasRun
testMethod (WasRun s _) = return $ WasRun s True

test = WasRun "testMethod" False

main = do
  print $ wasRun test
  tested <- testMethod test
  print $ wasRun tested
