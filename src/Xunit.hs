module Xunit where

data WasRun =
  WasRun String
         (WasRun -> IO WasRun)
         Bool

wasRun :: WasRun -> Bool
wasRun (WasRun _ _ x) = x

testMethod :: WasRun -> IO WasRun
testMethod (WasRun s f _) = return $ WasRun s f True

method :: WasRun -> (WasRun -> IO WasRun)
method (WasRun _ f _) = f

run :: WasRun -> IO WasRun
run x = method x $ x

test = WasRun "testMethod" testMethod False

main = do
  print $ wasRun test
  tested <- run test
  print $ wasRun tested
