{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Interpreter where
import Parser
import FileIO
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Vector as V
import Grammer

type Address       = Int
type MemoryMapping = Map.Map String Address
type Memory        = V.Vector Value
type ProcEnv       = Map.Map String Procedure

data VarEnv = VarEnv {
    mapping :: MemoryMapping,
    memory :: Memory,
    nextFree :: Int
} deriving Show

data Env = Env {
  varEnv  :: VarEnv,
  procEnv :: ProcEnv
} deriving (Show)

data Procedure = Procedure {
  procName   :: String,
  parameters :: [String],
  body       :: Block
} deriving (Show)

data Value = IntVal (Maybe Int) 
            | BoolVal (Maybe Bool) 
            | NotUsed
            deriving (Show, Eq)

type Interpreter a = StateT Env (ExceptT String IO) a

lookupVar :: String -> Interpreter Value
lookupVar name = do
    env <- get
    let vEnv = varEnv env
    case Map.lookup name (mapping vEnv) of
        Just address -> case memory vEnv V.!? address of
            Just val -> return val
            Nothing -> throwError ("Variable '" ++ name ++ "' is uninitialized")
        Nothing  -> throwError ("Undefined variable: " ++ name)

assignVar :: String -> Value -> Interpreter ()
assignVar name val = do
    env <- get
    let vEnv = varEnv env
    case Map.lookup name (mapping vEnv) of
        -- Variable already exists
        Just address -> do
            let newMemory = memory vEnv V.// [(address, val)]
            let newVEnv = vEnv { memory = newMemory }
            put env { varEnv = newVEnv }

        -- New variable, allocate memory
        Nothing -> do
            let address = nextFree vEnv
            let newMapping = Map.insert name address (mapping vEnv)
            let newMemory = memory vEnv V.// [(address, val)]
            let newVEnv = vEnv { mapping = newMapping, memory = newMemory, nextFree = address + 1 }
            put env { varEnv = newVEnv }

lookupProc :: String -> Interpreter Procedure
lookupProc name = do
  env <- get
  case Map.lookup name (procEnv env) of
    Nothing -> throwError $ "Undefined procedure: " ++ name
    Just p  -> return p

evalConstant :: Constant -> Interpreter Value
evalConstant (ConstNumber (Number op val)) = do
    let eval = fromIntegral val
    if op == "-" then return (IntVal $ Just (-eval)) else return (IntVal $ Just eval)
evalConstant (ConstIdentifier id) = evalIdentifier id

evalProgram :: Program -> Interpreter ()
evalProgram (Program blk) = do
    evalBlock blk

evalBlock :: Block -> Interpreter ()
evalBlock (Block decs cmpStmt) = do
    evalDeclarationList decs 
    evalStatement cmpStmt

evalDeclarationList :: DecleratonList -> Interpreter ()
evalDeclarationList (DecleratonList []) = return ()
evalDeclarationList (DecleratonList (dec:decs)) = do
    evalDeclaration dec
    evalDeclarationList (DecleratonList decs)

evalDeclaration :: Decleration -> Interpreter ()
evalDeclaration (DecConstDefList cdf) = evalConstDefList cdf
evalDeclaration (DecVarDeclList vdf) = evalVarDecList vdf
evalDeclaration (DecTypeDefList tdf) = evalTypeDefList tdf
evalDeclaration (DecProcedureDef pd) = evalProcedureDef pd

evalProcedureDef :: ProcedureDef -> Interpreter ()
evalProcedureDef (ProcedureDef ph blk) = do
    env <- get
    name <- evalProcedureHead ph
    let newProcEnv = Map.insert name (Procedure name [] blk) (procEnv env)
    put env { procEnv = newProcEnv }

evalProcedureHead :: ProcedureHead -> Interpreter [Char]
evalProcedureHead (ProcedureHead (Identifier id)) = return id

evalTypeDefList :: TypeDefList -> Interpreter ()
evalTypeDefList (TypeDefList []) = return ()
evalTypeDefList (TypeDefList (td:tds)) = do
    evalTypeDef td
    evalTypeDefList (TypeDefList tds)

evalTypeDef :: TypeDef -> Interpreter ()
evalTypeDef (TypeDef (Identifier id) ty) = undefined

evalConstDefList :: ConstDefList -> Interpreter ()
evalConstDefList (ConstDefList []) = return ()
evalConstDefList (ConstDefList (cd:cds)) = do
    evalConstDef cd
    evalConstDefList (ConstDefList cds)

evalConstDef :: ConstDef -> Interpreter ()
evalConstDef (ConstDef (Identifier id) val) = do
    eval <- evalConstant val
    assignVar id eval

evalVarDecList :: VarDeclList -> Interpreter ()
evalVarDecList (VarDeclList []) = return ()
evalVarDecList (VarDeclList (vd:vds)) = do
    evalVarDec vd
    evalVarDecList (VarDeclList vds)

evalVarDec :: VarDecl -> Interpreter ()
evalVarDec (VarDecl (Identifier id) ty) = do
    e <- evalType ty
    assignVar id e

evalType :: Type -> Interpreter Type
evalType (ArrayType ty) = do
    g <- evalType ty
    return (ArrayType g)
evalType (TypeIdentifer (Identifier ty)) = do
    if ty == "int" 
    then return (IntVal Nothing) 
    else if ty == "bool"
    then return (BoolVal Nothing)
    else throwError ("Unknown Type ")

evalStatementList :: StatementList -> Interpreter ()
evalStatementList (ComplexStatement stmt stmtList) = do
    evalStatement stmt
    evalStatementList stmtList
evalStatementList (SimpleStatement stmt) = evalStatement stmt

evalStatement :: Statement -> Interpreter ()
evalStatement (WriteStatement exp) = do
    val <- evalExp exp
    case (val) of
        (IntVal (Just v)) -> liftIO $ print v
        (BoolVal (Just v)) -> liftIO $ print v
evalStatement (IfStatement cond stat1 stat2) = do
    val <- evalCondition cond
    case (val) of
        (BoolVal r) -> do
            case (r) of
                (Just v) -> if v then evalStatement stat1 else evalStatement stat2
                _ -> return ()
        _ -> throwError ("Big Boy Problem")
evalStatement (WhileStatement cond stat) = do
    val <- evalCondition cond
    case (val) of
        (BoolVal r) -> do
            case (r) of 
                (Just v) -> do
                    if v then (do 
                            evalStatement stat
                            evalStatement (WhileStatement cond stat))
                    else return ()
                _ -> return ()
        _ -> throwError ("Big Boy Problem")
evalStatement (Assignment ty (LValue (Identifier id)) cond) = do
    econd <- evalCondition cond  
    case ty of
        "" -> assignVar id econd
        _ -> do
            val <- lookupVar id
            case (val, econd) of
                (IntVal (Just lval), IntVal (Just rval)) -> do
                    case ty of
                        "-" -> assignVar id (IntVal $ Just (lval - rval))
                        "+" -> assignVar id (IntVal $ Just (lval + rval))

evalStatement (CallStatement (Identifier id)) = do
    pro <- lookupProc id
    evalBlock (body pro)
evalStatement (CompoundStatement stmtList) = do
    evalStatementList stmtList
evalStatement (ForStatement (ForHeader assign cond expr) stmt) = do
    evalStatement assign
    case assign of
        (Assignment _ (LValue (Identifier id)) _) -> evalForLoop id cond expr stmt
        _ -> throwError "Assingment wasn't used"

evalForLoop :: String -> Condition -> Exp -> Statement -> Interpreter ()
evalForLoop id cond exp stmt = do
    val <- evalCondition cond
    case val of
        (BoolVal r) -> do
            case r of
                (Just v) -> do
                    if v then (do
                        evalStatement stmt
                        result <- evalExp exp
                        assignVar id result
                        evalForLoop id cond exp stmt)
                    else return ()
                _ -> throwError ("Unknown Error")
        _ -> throwError "Condition should evaluate to a bool Value"

evalCondition :: Condition -> Interpreter Value
evalCondition (SimpleCondition exp) = evalExp exp
evalCondition (RelationalCondition lexp (RelOp op) rexp) = do
  elexp <- evalExp lexp
  erexp <- evalExp rexp
  case (elexp, erexp) of
    (IntVal ml, IntVal mr) -> case (ml, mr) of       
        (Just l, Just r) -> case op of
            ">"  -> return $ BoolVal $ Just (l > r)
            ">=" -> return $ BoolVal $ Just (l >= r)
            "<"  -> return $ BoolVal $ Just (l < r)
            "<=" -> return $ BoolVal $ Just (l <= r)
            "!=" -> return $ BoolVal $ Just (l /= r)
            "=" -> return $ BoolVal $ Just (l == r)
            _    -> throwError ("Unknown relational operator: " ++ op)
    (BoolVal l, BoolVal r) -> case op of
      "==" -> return $ BoolVal $ Just (l == r)
      "!=" -> return $ BoolVal $ Just (l /= r)
      _    -> throwError ("Unsupported operator for boolean values: " ++ op)
    _ -> throwError "Type error in relational condition"

evalExp :: Exp -> Interpreter Value
evalExp (SingleExp str term) = do
    val <- evalTerm term
    case (val) of
        (IntVal me) -> case (me) of
            (Just e) -> do
                if str == "-" 
                then return (IntVal $ Just (-e))
                else return (IntVal $ Just e)
        (BoolVal e) -> throwError ("Big Bad Moment No.1")
evalExp (BinaryExp str term exp) = do
    eval <- evalTerm term
    eexp <- evalExp exp
    case (eval, eexp) of
        (IntVal mleft, IntVal mright) -> case (mleft, mright) of
            (Just left, Just right) -> do
                if str == "-"
                then return (IntVal $ Just $ -left + right)
                else return (IntVal $ Just $ left + right)


evalTerm :: Term -> Interpreter Value
evalTerm (SingleFactor fact) = evalFactor fact
evalTerm (BinaryTerm fact op term) = do
    fact' <- evalFactor fact 
    term' <- evalTerm term
    case (op) of
        "*" -> do
            case (fact', term') of
                (IntVal (Just left), IntVal (Just right)) -> return $ IntVal $ Just (left * right)
        "/" -> do
            case (fact', term') of
                (IntVal (Just left), IntVal (Just right)) -> return $ IntVal $ Just (div left right)
        _ -> throwError ("Undefined Variable: " ++ op)


evalFactor :: Factor -> Interpreter Value
evalFactor (FactorLValue lval) = evalLValue lval
evalFactor (FactorNumber num) = return (IntVal $ Just $ fromIntegral num)
evalFactor (FactorParen cond) = do evalCondition cond

evalIdentifier :: Identifier -> Interpreter Value
evalIdentifier (Identifier name) = lookupVar name

evalLValue :: LValue -> Interpreter Value
evalLValue (LValue x) = evalIdentifier x

emptyEnv :: Env
emptyEnv = Env {
  varEnv = emptyVarEnv,
  procEnv = Map.empty
}

emptyVarEnv = VarEnv {
    mapping = Map.empty,
    memory = V.replicate 64 NotUsed,
    nextFree = 0
}

runInterpreter :: Interpreter a -> Env -> IO (Either String (a, Env))
runInterpreter action env = runExceptT (runStateT action env)