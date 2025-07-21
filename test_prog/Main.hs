module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently (capture)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PL0 Tests"
  [ testCase "Basic sanity check" $
      2 + 2 @?= 4,
    testCase "Basic sanity check" $
      2 + 2 @?= 4
  ]