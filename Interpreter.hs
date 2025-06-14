module Interpreter where
import Parser
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

type Env = Map.Map String Value

data Value = IntVal Int | BoolVal Bool
  deriving (Show, Eq)

type Interpreter a = StateT Env (Except String) a

lookupVar :: String -> Interpreter Value
lookupVar name = do
  env <- get
  case Map.lookup name env of
    Just val -> return val
    Nothing  -> throwError ("Undefined variable: " ++ name)

assignVar :: String -> Value -> Interpreter ()
assignVar name val = do
  env <- get
  put (Map.insert name val env)

