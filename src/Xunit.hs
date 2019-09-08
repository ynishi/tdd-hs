module Xunit where

test = WasRun "testMethod"

main = do
  print $ wasRun test
  testMethod test
  print $ wasRun test
