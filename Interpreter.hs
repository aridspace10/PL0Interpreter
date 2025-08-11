{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use camelCase" #-}
module Interpreter where
import Data.Data
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
type Params        = [(String, Value)] -- Maps id to type

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
  parameters :: Params,
  ty       :: Type,
  body       :: Block
} deriving (Show)

data Value = IntVal (Maybe Int)
           | BoolVal (Maybe Bool)
           | ArrayContent [Value]
           | ArrayVal Value
           | ReferenceVal String Value
           | Uninitialized
           | Undefined
           | NotUsed
           deriving (Show, Eq, Data)

type Builtin = [Condition] -> Interpreter Value
builtinMap :: Map.Map String Builtin
builtinMap = Map.fromList [ ("malloc", builtin_malloc), ("length", builtin_length)]

sameConstructor :: Value -> Value -> Bool
sameConstructor (ArrayContent _) (ArrayVal _) = True
sameConstructor a b = toConstr a == toConstr b

type Interpreter a = StateT Env (ExceptT String IO) a

getAddress :: String -> Interpreter Int
getAddress name = do
    env <- get
    let vEnv = varEnv env
    case Map.lookup name (mapping vEnv) of
        Just address -> return address
        _ -> throwError (show $ mapping vEnv)

assignAddress :: String -> Address -> Interpreter ()
assignAddress id address = do
    env <- get
    let vEnv = varEnv env
    case address of
        (-1) -> do
            let newMapping = Map.delete id (mapping vEnv)
            let newVEnv = vEnv { mapping = newMapping}
            put env { varEnv = newVEnv }
        _ -> do
            let newMapping = Map.insert id address (mapping vEnv)
            let newVEnv = vEnv { mapping = newMapping}
            put env { varEnv = newVEnv }

accessMemory :: Int -> Interpreter Value
accessMemory address = do
    env <- get
    let vEnv = varEnv env
    case memory vEnv V.!? address of
        Just val -> return val
        Nothing -> throwError "Unknown"

assignMemory :: Int -> Value -> Interpreter ()
assignMemory address val = do
    env <- get
    let vEnv = varEnv env
    let newMemory = memory vEnv V.// [(address, val)]
    let newVEnv = vEnv { memory = newMemory }
    put env { varEnv = newVEnv }

getArrayContent :: Int -> Int -> [Value] -> Interpreter Value
getArrayContent _ 0 vals = return (ArrayContent vals)
getArrayContent address left vals = do
    val <- accessMemory address
    getArrayContent (address + 1) (left - 1) (val : vals)

lookupVar :: String -> Interpreter Value
lookupVar name = do
    address <- getAddress name
    val <- accessMemory address
    case val of
        ArrayVal (IntVal (Just len)) -> getArrayContent (address + 1) len []
        _ -> return val

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
    return ()

evalBlock :: Block -> Interpreter (Either () Value)
evalBlock (Block decs cmpStmt) = do
    evalDeclarationList decs
    g <- evalStatement cmpStmt
    return g

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
    (name, params, ty) <- evalProcedureHead ph
    let newProcEnv = Map.insert name (Procedure name params ty blk) (procEnv env)
    put env { procEnv = newProcEnv }

evalProcedureHead :: ProcedureHead -> Interpreter ([Char], Params, Type)
evalProcedureHead (ProcedureHead (Identifier id) lst ty) = do
    params <- evalParametersList lst []
    return (id, params, ty)

evalParametersList :: ParametersList -> Params -> Interpreter Params
evalParametersList (ParametersList []) lst = return lst
evalParametersList (ParametersList (Parameter (Identifier id) ty : params)) lst = do
    ety <- evalType ty
    let param = (id, ety)
    evalParametersList (ParametersList params) (param : lst)

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

evalType :: Type -> Interpreter Value
evalType (ArrayType ty) = do
    g <- evalType ty
    return (ArrayVal g)
evalType (TypeIdentifer (Identifier ty)) = do
    if ty == "int"
    then return (IntVal Nothing)
    else if ty == "bool"
    then return (BoolVal Nothing)
    else throwError ("Unknown Type ")

evalStatementList :: StatementList -> Interpreter (Either () Value)
evalStatementList (ComplexStatement stmt stmtList) = do
    g <- evalStatement stmt
    case g of
        Left () -> evalStatementList stmtList
        Right val -> return (Right val)
evalStatementList (SimpleStatement stmt) = do
    g <- evalStatement stmt
    case g of
        Left () -> return (Left ())
        Right val -> return (Right val)

print' :: Value -> Exp -> Interpreter ()
print' (IntVal Nothing) _ = liftIO $ putStr ("null")
print' (BoolVal Nothing) _ = liftIO $ putStr ("null")
print' (IntVal (Just v)) Empty = liftIO $ putStr (show v)
print' (BoolVal (Just v)) Empty = liftIO $ putStr (show v)
print' (IntVal (Just v)) _ = liftIO $ print v
print' (BoolVal (Just v)) _ = liftIO $ print v
print' (ArrayVal (IntVal (Just space))) (SingleExp "" (SingleFactor (FactorLValue (LValue (Identifier id) [])))) = do
    liftIO $ putStr "["
    address <- getAddress id
    printArray (address + 1) space
    where
        printArray _ 0 = liftIO $ putStr "]"
        printArray address 1 = do
            temp <- accessMemory address
            print' temp Empty
            liftIO $ putStrLn "]"
            return ()
        printArray address space = do
            temp <- accessMemory address
            print' temp Empty
            liftIO $ putStr ","
            printArray (address + 1) (space - 1)
print' v _ = throwError ("Error in print: " ++ show v)

evalStatement :: Statement -> Interpreter (Either () Value)
evalStatement (WriteStatement exp) = do
    val <- evalExp exp
    print' val exp
    return (Left ())
evalStatement (ReturnStatement cond) = do
    econd <- evalCondition cond
    return $ Right econd
evalStatement (IfStatement cond stat1 stat2) = do
    val <- evalCondition cond
    case (val) of
        (BoolVal r) -> do
            case (r) of
                (Just v) -> if v then evalStatement stat1 else evalStatement stat2
                _ -> return (Left ())
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
                    else return (Left ())
                _ -> return (Left ())
        _ -> throwError ("Big Boy Problem")
evalStatement (Assignment lval (AssignOperator op) cond) = do
    econd <- evalCondition cond
    case lval of
        (LValue (Identifier id) []) -> do
            case op of
                ":=" -> do
                    case econd of
                        (ArrayContent values) -> do
                            val <- lookupVar id
                            case val of
                                Undefined -> throwError (id ++ " is not defined")
                                Uninitialized -> throwError (id ++ " is not initalized to a certain size")
                                (ArrayVal (IntVal (Just size))) -> case (size < (length values)) of
                                    (True) -> throwError (id ++ " is of set size " ++ show size ++ ", however given array is of size " ++ show (length values))
                                    (False) -> do
                                        address <- getAddress id
                                        arrayBuild (address + 1) values
                                        return (Left ())
                        (ReferenceVal str val) -> do
                            case (str, val) of
                                ("malloc", IntVal (Just size)) -> do
                                    t <- lookupVar id
                                    case t of
                                        (ArrayVal ty) -> do
                                            initaladd <- getAddress id
                                            assignMemory initaladd NotUsed
                                            env <- get
                                            let vEnv = varEnv env
                                            let address = nextFree vEnv
                                            assignAddress id address
                                            assignMemory address (ArrayVal (IntVal (Just size)))
                                            address <- assignArray ty size (address + 1)
                                            env' <- get
                                            let vEnv' = varEnv env'
                                            let newVEnv = vEnv' {nextFree = address + 1 }
                                            put env { varEnv = newVEnv }
                                            return (Left ())
                                        _ -> throwError "DO LATER"
                        _ -> do
                            assignVar id econd
                            return (Left ())
                _ -> do
                    val <- lookupVar id
                    case (val, econd) of
                        (IntVal (Just lval), IntVal (Just rval)) -> do
                            case op of
                                "-=" -> assignVar id (IntVal $ Just (lval - rval))
                                "+=" -> assignVar id (IntVal $ Just (lval + rval))
                            return (Left ())
            return (Left ())
        (LValue (Identifier id) (const:[])) -> do
            a <- getAddress id
            (IntVal (Just c)) <- evalConstant const
            let address = a + c + 1
            case op of
                ":=" -> assignMemory address econd
                _ -> do
                    val <- accessMemory address
                    case (val, econd) of
                        (IntVal (Just lval), IntVal (Just rval)) -> do
                            case op of
                                "-=" -> assignMemory address (IntVal $ Just (lval - rval))
                                "+=" -> assignMemory address (IntVal $ Just (lval + rval))
            return (Left ())
evalStatement (CallStatement (Identifier id) (CallParamList params)) = do
    case Map.lookup id builtinMap of
        Just func -> do
            val <- func params
            return (Right val)
        Nothing -> do
            pro <- lookupProc id
            assignParams params (parameters pro)
            env <- get
            let vEnv = varEnv env
            g <- evalBlock (body pro)
            put env {varEnv = vEnv}
            return g
evalStatement (CompoundStatement stmtList) = do
    g <- evalStatementList stmtList
    return g
evalStatement (ForStatement (ForHeader assign cond expr) stmt) = do
    evalStatement assign
    case assign of
        (Assignment (LValue (Identifier id) _) (AssignOperator ":=") _) -> do
            evalForLoop id cond expr stmt
            return (Left ())
        _ -> throwError "Assingment wasn't used"
evalStatement (ArrayCreation (LValue (Identifier id) cs) _ const) = do
    val <- lookupVar id
    add <- getAddress id
    assignMemory add NotUsed
    space <- evalConstant const
    env <- get
    let vEnv = varEnv env
    let address = nextFree vEnv
    case (val, space) of
        (ArrayVal ty, IntVal (Just space')) -> do
            assignAddress id address
            assignMemory address (ArrayVal (IntVal (Just space')))
            address <- assignArray ty space' (address + 1)
            env' <- get
            let vEnv' = varEnv env'
            let newVEnv = vEnv' {nextFree = address + 1 }
            put env { varEnv = newVEnv }
            return (Left ())
evalStatement stuff = throwError ("Compiler Error in EvalStatement: " ++ show stuff)

builtin_length :: [Condition] -> Interpreter Value
builtin_length [] = throwError "Expecting an argument"
builtin_length [cond] = do
    case cond of
        SimpleCondition (SimpleRelCondition (SingleExp "" (SingleFactor (FactorLValue (LValue (Identifier id) []))))) -> do
            address <- getAddress id
            val <- accessMemory address
            case val of
                (ArrayVal (IntVal Nothing)) -> return (IntVal (Just 0))
                (ArrayVal val) -> return val
                _ -> throwError ("Type of " ++ id ++ " can not work with length()")
        _ -> throwError "Unexpected a value given to length() function" 
builtin_length conds = throwError ("Expecting 1 argument, instead receieved" ++ (show $ length conds + 1))

builtin_malloc :: [Condition] -> Interpreter Value
builtin_malloc [] = throwError "Expecting an argument"
builtin_malloc [cond] = do
    econd <- evalCondition cond
    case econd of
        IntVal (Just val) -> return (ReferenceVal "malloc" (IntVal (Just val)))
        _ -> throwError ("Unable to malloc with " ++ show econd)
builtin_malloc conds = throwError ("Expecting 1 argument, instead receieved" ++ (show $ length conds + 1))

unassignParams :: Params -> Interpreter ()
unassignParams [] = return ()
unassignParams ((id, _):params) = do
    assignVar id NotUsed
    assignAddress id (-1)
    unassignParams params

assignParams :: [Condition] -> Params -> Interpreter ()
assignParams [] [] = return ()
assignParams lst [] = throwError "Too Many Arguments"
assignParams [] lst = throwError "Too few arguments"
assignParams (given: givens) ((id, ty): params) = do
    econd <- evalCondition given
    case (sameConstructor econd ty) of
        True -> do
            case (ty, econd) of
                (ArrayVal _, ArrayContent values) -> do
                    liftIO $ print values
                    env <- get
                    let vEnv = varEnv env
                    let address = nextFree vEnv
                    assignAddress id address
                    assignMemory address (ArrayVal $ IntVal $ Just (length values))
                    next <- arrayBuild (address + 1) values
                    env' <- get
                    let vEnv' = varEnv env'
                    let newVEnv = vEnv' {nextFree = next }
                    put env { varEnv = newVEnv }
                _ -> do
                    assignVar id econd
                    assignParams givens params
        False -> throwError ("Wrong Parameter Types: " ++ show econd ++ " != " ++ show ty)

arrayBuild :: Int -> [Value] -> Interpreter Int
arrayBuild add [] = return add
arrayBuild address (elem:elems) = do
    assignMemory address elem
    arrayBuild (address + 1) elems

assignArray :: Value -> Int -> Int -> Interpreter Int
assignArray _ 0 add = return add
assignArray ty left address = do
    assignMemory address ty
    assignArray ty (left - 1) (address + 1)

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
evalCondition (NotCondition cond) = do
    econd <- evalCondition cond
    case econd of
        (BoolVal (Just True)) -> return (BoolVal (Just False))
        (BoolVal (Just False)) -> return (BoolVal (Just True))
        (IntVal (Just 0)) -> return (IntVal (Just 1))
        (IntVal (Just _)) -> return (IntVal (Just 0))
evalCondition (SimpleCondition cond) = evalRelationalCondition cond
evalCondition (LogicCondition lcond (LogOp op) rcond) = do
    elcond <- evalRelationalCondition lcond
    ercond <- evalCondition rcond
    case (elcond, ercond) of
        (BoolVal (Just l), BoolVal (Just r)) -> case op of
            "&&" -> case (l, r) of
                (True, True)   -> return $ BoolVal (Just True)
                _              -> return $ BoolVal (Just False)
            "||" -> case (l, r) of
                (False, False) -> return $ BoolVal (Just False)
                _              -> return $ BoolVal (Just True)
            "^^" -> case (l, r) of
                (True, False)  -> return $ BoolVal (Just True)
                (False, True)  -> return $ BoolVal (Just True)
                _              -> return $ BoolVal (Just False)

evalRelationalCondition :: RelationalCondition -> Interpreter Value
evalRelationalCondition (SimpleRelCondition exp) = evalExp exp
evalRelationalCondition (ComplexRelCondition lexp (RelOp op) rexp) = do
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
    env <- get
    case (val) of
        (IntVal me) -> case (me) of
            (Just e) -> do
                if str == "-"
                then return (IntVal $ Just (-e))
                else return (IntVal $ Just e)
            _ -> throwError (show term)
        (BoolVal e) -> return (BoolVal e)
        (ArrayContent e) -> return (ArrayContent e)
        (ArrayVal e) -> return (ArrayVal e)
        (ReferenceVal str val) -> return (ReferenceVal str val)
        _ -> throwError $ (show val ++ show env)
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
    env <- get
    fact' <- evalFactor fact
    term' <- evalTerm term
    case (op) of
        "*" -> do
            case (fact', term') of
                (IntVal (Just left), IntVal (Just right)) -> return $ IntVal $ Just (left * right)
                _ -> throwError (show fact' ++ " " ++ show term' ++ show env)
        "/" -> do
            case (fact', term') of
                (IntVal (Just left), IntVal (Just right)) -> return $ IntVal $ Just (div left right)
        _ -> throwError ("Undefined Variable: " ++ op)


evalFactor :: Factor -> Interpreter Value
evalFactor (FactorLValue lval) = evalLValue lval
evalFactor (FactorNumber num) = return (IntVal $ Just $ fromIntegral num)
evalFactor (FactorParen cond) = evalCondition cond
evalFactor (FactorCall call) = do
    g <- evalStatement call
    case g of
        Left _ -> throwError "Compiler Error in evalFactor"
        Right val -> return val
evalFactor (ArrayLiteral exps) = do
    values <- mapM evalExp exps  -- [Exp] -> Interpreter [Value]
    return $ ArrayContent values

evalIdentifier :: Identifier -> Interpreter Value
evalIdentifier (Identifier name) = do
    case name of
        "True" -> return (BoolVal (Just True))
        "False" -> return (BoolVal (Just False))
        _ -> lookupVar name

evalLValue :: LValue -> Interpreter Value
evalLValue (LValue id []) = evalIdentifier id
evalLValue (LValue (Identifier id) (const: [])) = do
    c <- evalConstant const
    initalAdd <- getAddress id
    case (c) of
        (IntVal (Just val)) -> accessMemory (initalAdd + val + 1)

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