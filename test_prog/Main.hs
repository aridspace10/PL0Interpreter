module Main where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PL0 Tests"
  [ testCase "Basic sanity check" $
      2 + 2 @?= 4
  ]