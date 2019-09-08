module Xunit where

data WasRun = WasRun String

test = WasRun "testMethod"

main = do
  print $ wasRun test
  testMethod test
  print $ wasRun test
