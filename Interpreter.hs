{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Interpreter where
import Parser
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Control.Monad.Trans.Accum (look)
import Foreign.C (throwErrno)

type Env = Map.Map String Value

data Value = IntVal Int | BoolVal Bool
  deriving (Show, Eq)

type Interpreter a = StateT Env (ExceptT String IO) a

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
    val <- evalCondition cond
    case (val) of
        (BoolVal r) -> if r then evalStatement stat1 else evalStatement stat2
        _ -> throwError ("Big Boy Problem")
evalStatement (WhileStatement cond stat) = do
    val <- evalCondition cond
    case (val) of
        (BoolVal r) -> do
            if r then (do 
                    evalStatement stat
                    evalStatement (WhileStatement cond stat))
            else return ()
        _ -> throwError ("Big Boy Problem")

evalCondition :: Condition -> Interpreter Value
evalCondition (SimpleCondition exp) = evalExp exp
evalCondition (RelationalCondition lexp (RelOp op) rexp) = do
  elexp <- evalExp lexp
  erexp <- evalExp rexp
  case (elexp, erexp) of
    (IntVal l, IntVal r) -> case op of
      ">"  -> return $ BoolVal (l > r)
      ">=" -> return $ BoolVal (l >= r)
      "<"  -> return $ BoolVal (l < r)
      "<=" -> return $ BoolVal (l <= r)
      "!=" -> return $ BoolVal (l /= r)
      "==" -> return $ BoolVal (l == r)
      _    -> throwError ("Unknown relational operator: " ++ op)
    (BoolVal l, BoolVal r) -> case op of
      "==" -> return $ BoolVal (l == r)
      "!=" -> return $ BoolVal (l /= r)
      _    -> throwError ("Unsupported operator for boolean values: " ++ op)
    _ -> throwError "Type error in relational condition"

evalExp :: Exp -> Interpreter Value
evalExp (SingleExp str term) = do
    val <- evalTerm term
    case (val) of
        (IntVal e) -> do
            if str == "+" 
            then return (IntVal e)
            else return $ IntVal (e * (-1))
        (BoolVal e) -> throwError ("Big Bad Moment No.1")
evalExp (BinaryExp str term exp) = do
    eval <- evalTerm term
    eexp <- evalExp exp
    case (eval, eexp) of
        (IntVal left, IntVal right) -> do
            if str == "+"
            then return (IntVal $ left + right)
            else return (IntVal $ (left * (-1)) + right)


evalTerm :: Term -> Interpreter Value
evalTerm (SingleFactor fact) = evalFactor fact

evalFactor :: Factor -> Interpreter Value
evalFactor (FactorLValue lval) = evalLValue lval
evalFactor (FactorNumber num) = return (IntVal $ fromIntegral num)
evalFactor (FactorParen cond) = do evalCondition cond

evalIdentifier :: Identifier -> Interpreter Value
evalIdentifier (Identifier name) = lookupVar name

-- Eval lvalues
evalLValue :: LValue -> Interpreter Value
evalLValue (LValue x) = evalIdentifier x

testEnv :: Env
testEnv = Map.fromList [("x", IntVal 5), ("y", BoolVal True)]