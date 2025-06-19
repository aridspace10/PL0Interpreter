{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Interpreter where
import Parser
import FileIO
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

type Env = Map.Map String Value

data Value = IntVal (Maybe Int) | BoolVal (Maybe Bool) | Uninitialized Value
  deriving (Show, Eq)

type Interpreter a = StateT Env (ExceptT String IO) a

lookupVar :: String -> Interpreter Value
lookupVar name = do
    env <- get
    case Map.lookup name env of
        Just (Uninitialized val) -> throwError ("Variable '" ++ name ++ "' is uninitialized")
        Just val -> return val
        Nothing  -> throwError ("Undefined variable: " ++ name)

assignVar :: String -> Value -> Interpreter ()
assignVar name val = do
    env <- get
    put (Map.insert name val env)

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
    evalCompoundStatement cmpStmt

evalDeclarationList :: DecleratonList -> Interpreter ()
evalDeclarationList (DecleratonList []) = return ()
evalDeclarationList (DecleratonList (dec:decs)) = do
    evalDeclaration dec
    evalDeclarationList (DecleratonList decs)

evalDeclaration :: Decleration -> Interpreter ()
evalDeclaration (DecConstDefList cdf) = evalConstDefList cdf
evalDeclaration (DecVarDeclList vdf) = evalVarDecList vdf

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
evalVarDec (VarDecl (Identifier id) (TypeIdentifer (Identifier ty))) = do
    if ty == "int" 
    then assignVar id (Uninitialized $ IntVal Nothing) 
    else if ty == "bool"
    then assignVar id (Uninitialized $ BoolVal Nothing)
    else throwError ("Unknown Type ")

evalCompoundStatement :: CompoundStatement -> Interpreter ()
evalCompoundStatement (CompoundStatement stmtList) = evalStatementList stmtList

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
evalStatement (Assignment (LValue (Identifier id)) cond) = do
    econd <- evalCondition cond 
    assignVar id econd

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
            "==" -> return $ BoolVal $ Just (l == r)
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
                if str == "+" 
                then return (IntVal $ Just e)
                else return $ IntVal $ Just (e * (-1))
        (BoolVal e) -> throwError ("Big Bad Moment No.1")
evalExp (BinaryExp str term exp) = do
    eval <- evalTerm term
    eexp <- evalExp exp
    case (eval, eexp) of
        (IntVal mleft, IntVal mright) -> case (mleft, mright) of
            (Just left, Just right) -> do
                if str == "+"
                then return (IntVal $ Just $ left + right)
                else return (IntVal $ Just $ -left + right)


evalTerm :: Term -> Interpreter Value
evalTerm (SingleFactor fact) = evalFactor fact

evalFactor :: Factor -> Interpreter Value
evalFactor (FactorLValue lval) = evalLValue lval
evalFactor (FactorNumber num) = return (IntVal $ Just $ fromIntegral num)
evalFactor (FactorParen cond) = do evalCondition cond

evalIdentifier :: Identifier -> Interpreter Value
evalIdentifier (Identifier name) = lookupVar name

-- Eval lvalues
evalLValue :: LValue -> Interpreter Value
evalLValue (LValue x) = evalIdentifier x

testEnv :: Env
testEnv = Map.fromList [("x", IntVal $ Just 5), ("y", BoolVal $ Just True), ("z", Uninitialized (IntVal $ Nothing))]

emptyEnv :: Env
emptyEnv = Map.empty

runInterpreter :: Interpreter a -> Env -> IO (Either String (a, Env))
runInterpreter action env = runExceptT (runStateT action env)

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
                result <- runInterpreter (evalProgram program) emptyEnv
                case result of
                    Left err -> putStrLn ("Runtime error: " ++ err)
                    Right (_, env) -> print env