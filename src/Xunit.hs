module Xunit where

data WasRun = WasRun

test = WasRun "testMethod"

main = do
  print $ wasRun test
  testMethod test
  print $ wasRun test
