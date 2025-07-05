module FileIO (
    readFileContents,
    writeFileContents,
    appendFileContents,
    fileExists,
    readFileLines,
    writeFileLines
) where

import System.IO (withFile, IOMode(..), hPutStr, hGetContents)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import GHC.Weak (finalize)

-- | Read entire contents of a file as a String.
readFileContents :: FilePath -> IO (Either String String)
readFileContents path = 
    catch (Right <$> readFile path)
          handleReadError
  where
    handleReadError :: IOException -> IO (Either String String)
    handleReadError e = return $ Left ("Error reading file: " ++ show e)

-- | Write a String to a file (overwriting any existing content).
writeFileContents :: FilePath -> String -> IO (Either String ())
writeFileContents path content = 
    catch (Right <$> writeFile path content)
          handleWriteError
  where
    handleWriteError :: IOException -> IO (Either String ())
    handleWriteError e = return $ Left ("Error writing file: " ++ show e)

-- | Append a String to a file.
appendFileContents :: FilePath -> String -> IO (Either String ())
appendFileContents path content =
    catch (Right <$> appendFile path content)
          handleAppendError
  where
    handleAppendError :: IOException -> IO (Either String ())
    handleAppendError e = return $ Left ("Error appending file: " ++ show e)

-- | Check if a file exists.
fileExists :: FilePath -> IO Bool
fileExists = doesFileExist

-- | Read a file and return its contents as a list of lines.
readFileLines :: FilePath -> IO (Either String [String])
readFileLines path =
    catch (Right . lines <$> readFile path)
          handleReadError
  where
    handleReadError :: IOException -> IO (Either String [String])
    handleReadError e = return $ Left ("Error reading file: " ++ show e)

-- | Write a list of Strings to a file, one per line (overwriting).
writeFileLines :: FilePath -> [String] -> IO (Either String ())
writeFileLines path linesContent =
    writeFileContents path (unlines linesContent)

makeUseable :: Int -> String -> String -> String
makeUseable _ [] final = final
makeUseable removing (x:y:xs) final
    | x == '/' && y == '/'  = makeUseable 1 xs final
    | x == '/' && y == 'n'  = makeUseable 0 xs final
    | removing == 1         = makeUseable 1 (y:xs) final
    | otherwise             = makeUseable 0 (y:xs) (final ++ [x])
makeUseable removing [x] final
    | removing == 1  = final
    | otherwise      = final ++ [x]