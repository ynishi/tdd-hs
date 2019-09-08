module Xunit where

data WasRun =
  WasRun String
         Bool

wasRun :: WasRun -> Bool
wasRun (WasRun _ x) = x

testMethod :: WasRun -> IO WasRun
testMethod (WasRun s _) = return $ WasRun s True

run :: WasRun -> IO WasRun
run = testMethod

test = WasRun "testMethod" False

main = do
  print $ wasRun test
  tested <- run test
  print $ wasRun tested
