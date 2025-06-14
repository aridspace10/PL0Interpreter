{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
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

evalStatement :: Statement -> Interpreter ()
evalStatement (WriteStatement exp) = do
    val <- evalExp exp
    liftIO $ print val
evalStatement (IfStatement cond stat1 stat2) = do
    r <- evalCondition
    if r then evalStatement stat1 else evalStatement stat2
evalStatement (WhileStatement cond stat) = do
    r <- evalCondition
    (if r then (do
        evalStatement stat
        evalStatement (WhileStatement cond stat)) 
    else return ())