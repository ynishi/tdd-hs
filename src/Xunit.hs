module Xunit where

data WasRun = WasRun String

wasRun :: WasRun -> Bool
wasRun _ = False

test = WasRun "testMethod"

main = do
  print $ wasRun test
  testMethod test
  print $ wasRun test
