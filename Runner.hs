module Runner where

import Parser
import Interpreter
import FileIO
import StaticChecker (runStaticChecker, nullScope, checkProgram)
import Grammer

getErrors :: Scope -> [Error]
getErrors (Scope _ errs parent) = errs ++ getErrors parent
getErrors NullScope             = []

run :: FilePath -> IO ()
run path = do
  res <- readFileContents path
  case res of
    Left err -> putStrLn ("Error reading file: " ++ err)
    Right content -> do
        let result = removeNewlines content
        case parse parseProgram result of
            Nothing -> putStrLn "Parse error."
            Just (program, _) -> do
                checked <- runStaticChecker (checkProgram program) nullScope
                case checked of
                    Left err -> putStrLn ("Fatal Static error: " ++ err)
                    Right (_, finalScope) -> do
                        result <- runInterpreter (evalProgram program) emptyEnv
                        case result of
                            Left err -> putStrLn ("Runtime error: " ++ err)
                            Right (_, env) -> print env