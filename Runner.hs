module Runner where

import Parser
import Interpreter
import FileIO
import StaticChecker (runStaticChecker, nullScope, checkProgram)
import Grammer

run :: FilePath -> IO ()
run path = do
  res <- readFileContents path
  case res of
    Left err -> putStrLn ("Error reading file: " ++ err)
    Right content -> do
        let result = removeNewlines content
        case parseWithErrors parseProgram result of
            Left err -> do
                putStrLn $ "Parse Error at position " ++ show (errorPosition err)
                putStrLn $ "Message: " ++ errorMessage err
            Right (program, "") -> do
                checked <- runStaticChecker (checkProgram program) nullScope
                case checked of
                    Left err -> putStrLn ("Static error: " ++ err)
                    Right (_, _) -> do
                        result <- runInterpreter (evalProgram program) emptyEnv
                        case result of
                            Left err -> putStrLn ("Runtime error: " ++ err)
                            Right (_, env) -> print env