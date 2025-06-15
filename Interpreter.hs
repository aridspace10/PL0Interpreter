{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Interpreter where
import Parser
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Control.Monad.Trans.Accum (look)

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

evalCondition :: Condition -> Interpreter Value
evalCondition (SimpleCondition exp) = evalExp exp

evalExp :: Exp -> Interpreter Value
evalExp (SingleExp str term) = do
    eterm <- evalTerm term
    if str == "+" 
    then return eterm
    else return $ eterm * (-1)

evalTerm :: Term -> Interpreter Value
evalTerm (SingleFactor fact) = evalFactor fact

evalFactor :: Factor -> Interpreter Value
evalFactor (FactorLValue lval) = evalLValue lval
evalFactor (FactorNumber num) = return (IntVal num)
evalFactor (FactorParen cond) = do
    b <- evalCondition cond
    return (BoolVal b)

evalIdentifier :: Identifier -> Interpreter Value
evalIdentifier (Identifier name) = lookupVar name

-- Eval lvalues
evalLValue :: LValue -> Interpreter Value
evalLValue (LValue x) = evalIdentifier x

testEnv :: Env
testEnv = Map.fromList [("x", IntVal 5), ("y", BoolVal True)]