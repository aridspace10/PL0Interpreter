module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently (capture)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM)

import AppRunner (run)

main :: IO ()
main = do
  testCases <- discoverTestCases "test/cases"
  defaultMain $ testGroup "PL0 Interpreter Tests" testCases

discoverTestCases :: FilePath -> IO [TestTree]
discoverTestCases dir = do
  caseDirs <- listDirectory dir
  forM caseDirs $ \caseName -> do
    let inputPath = dir </> caseName </> "input.txt"
    let expectedPath = dir </> caseName </> "expected.txt"

    expectedOutput <- readFile expectedPath

    return $ testCase caseName $ do
      (output, _) <- capture $ run inputPath
      output @?= expectedOutput