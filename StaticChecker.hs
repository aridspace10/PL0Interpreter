{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module StaticChecker where
import Parser
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import GHC.Natural ( Natural )
import Grammer

data Error = Error Natural String
data AssignedType = IntType 
                    | BoolType 
                    | RefType String 
                    | SubType Int Int
                    | ProcedureType AssignedType
                    | NoneType
                    | ConstantType AssignedType
                    | ArrType AssignedType deriving (Show, Eq)
data Scope = Scope SymTable [Error] Scope
type SymTable = Map.Map String AssignedType
type StaticChecker a = StateT Scope (ExceptT String IO) a

---------------- HELPER -----------------------------
assignVar :: String -> AssignedType -> StaticChecker ()
assignVar name val = do
    Scope symTable errors parent <- get
    let newSymTable = Map.insert name val symTable
    put (Scope newSymTable errors parent)

addError :: Error -> StaticChecker ()
addError err = do
    Scope symTable errors parent <- get
    let newErrors = errors ++ [err]
    put (Scope symTable newErrors parent)

lookupType :: String -> StaticChecker AssignedType
lookupType id = do
    Scope symTable errors parent <- get
    case Map.lookup id symTable of
        Just IntType -> return IntType
        Just BoolType -> return BoolType
        Just (ArrType ty) -> return $ ArrType ty
        Just (ProcedureType ty) -> return $ ProcedureType ty
        Just (ConstantType ty) -> return (ConstantType ty)
        nothing -> throwError (show symTable ++ " with id: " ++ id)


getUnresolvedTypes :: [(String , AssignedType)] -> [(String , AssignedType)] -> [(String , AssignedType)]
getUnresolvedTypes ys [] = ys
getUnresolvedTypes ys (x:xs) = do
    case x of
        (_, RefType str) -> getUnresolvedTypes (ys ++ [x]) xs
        (_, ArrType (RefType str)) -> getUnresolvedTypes (ys ++ [x]) xs
        _ -> getUnresolvedTypes ys xs

resolveUnresolvedType :: [String] -> (String, AssignedType) -> StaticChecker ()
resolveUnresolvedType backlog x = do
    case x of
        (id, RefType str) -> do
            if id `elem` backlog then throwError "Circular Definition Moment"
            else do
                ty <- lookupType str
                case ty of
                    IntType -> assignVar id IntType
                    BoolType -> assignVar id BoolType
                    ArrType g -> assignVar id (ArrType g)
                    RefType otherid -> resolveUnresolvedType (backlog ++ [id]) (str, RefType otherid)

resolveUnresolvedTypes :: [(String, AssignedType)] -> StaticChecker ()
resolveUnresolvedTypes [] = return ()
resolveUnresolvedTypes (y:ys) = do
    resolveUnresolvedType [] y
    resolveUnresolvedTypes ys

resolveTypes :: StaticChecker ()
resolveTypes = do
    Scope symTable errors parent <- get
    let before = getUnresolvedTypes [] (Map.toList symTable)
    resolveUnresolvedTypes before
    let after = getUnresolvedTypes [] (Map.toList symTable)
    if null after
    then return ()
    else if before == after
        then throwError "Unresolved Types !!!!!!!!!!"
        else resolveTypes


----------------------------- CHECKING
checkProgram :: Program -> StaticChecker ()
checkProgram (Program code) = do
    checkBlock code

checkBlock :: Block -> StaticChecker ()
checkBlock (Block decList compStat) = do
    checkDecList decList
    resolveTypes
    checkStatement compStat

checkDecList :: DecleratonList -> StaticChecker ()
checkDecList (DecleratonList []) = return ()
checkDecList (DecleratonList (dec:decs)) = do
    checkDecleraton dec
    checkDecList (DecleratonList decs)

checkDecleraton (DecConstDefList (ConstDefList cdf)) = checkConstDef cdf
checkDecleraton (DecVarDeclList (VarDeclList vdf)) = checkVarDef vdf
checkDecleraton (DecTypeDefList (TypeDefList tdf)) = checkTypeDef tdf
checkDecleraton (DecProcedureDef (ProcedureDef pd blk)) = do
    checkProcedureHead pd
    checkBlock blk

checkProcedureHead :: ProcedureHead -> StaticChecker ()
checkProcedureHead (ProcedureHead (Identifier id) (ParametersList params) ty) = do
    Scope symTable errors parent <- get
    ety <- checkType ty
    case Map.lookup id symTable of
        Nothing -> assignVar id (ProcedureType ety)
        _ -> do 
            ty <- lookupType id 
            throwError (id ++ "already has type of " ++ show ty)
    checkParams params
    return ()

checkParams :: [Parameter] -> StaticChecker ()
checkParams [] = return ()
checkParams ((Parameter (Identifier id) ty):params) = do
    ety <- checkType ty
    assignVar id ety
    checkParams params

checkTypeDef :: [TypeDef] -> StaticChecker ()
checkTypeDef [] = return ()
checkTypeDef ((TypeDef (Identifier id) ty):tds) = do
    tid <- checkType ty
    assignVar id tid
    checkTypeDef tds

checkType (ArrayType ty) = do
    g <- checkType ty
    return $ ArrType g
checkType (SubrangeType c1 c2) = do
    let ec1 = getConst c1
    let ec2 = getConst c2
    return (SubType ec1 ec2)
checkType (TypeIdentifer (Identifier tid)) = do
    case tid of
        "int" -> return IntType
        "bool" ->  return BoolType
        _ -> return (RefType tid)
checkType (None) = return NoneType
checkType _ = throwError "Unknown Type Given"


getConst :: Constant -> Int
getConst (ConstNumber (Number "-" num)) = fromIntegral (-num)
getConst (ConstNumber (Number op num)) = fromIntegral num

checkConstDef :: [ConstDef] -> StaticChecker ()
checkConstDef [] = return ()
checkConstDef ((ConstDef (Identifier id) const):cds) = do
    case lookup id of
        nothing -> do
            case const of
                (ConstNumber (Number op num)) -> assignVar id (ConstantType IntType)
                (ConstIdentifier (Identifier otherid)) -> do
                    ty <- lookupType otherid
                    assignVar id (ConstantType ty)
                (ConstArray fact) -> do
                    ty <- checkFactor fact
                    assignVar id (ConstantType ty)
        _ -> addError (Error 0 ("Reassignment of " ++ id))
    checkConstDef cds

checkVarDef :: [VarDecl] -> StaticChecker ()
checkVarDef [] = return ()
checkVarDef ((VarDecl (Identifier id) ty):vds) = do
    case ty of
        (TypeIdentifer tid) -> do
            case tid of
                (Identifier tid') -> do
                    case tid' of
                        "int" -> assignVar id IntType
                        "bool" -> assignVar id BoolType
                        _ -> assignVar id (RefType tid')
        (ArrayType ty) -> do
            g <- checkType ty
            assignVar id (ArrType g)
    checkVarDef vds

checkStatementList :: StatementList -> StaticChecker ()
checkStatementList (SimpleStatement stat) = do
    checkStatement stat
checkStatementList (ComplexStatement stat statLst) = do
    checkStatement stat
    checkStatementList statLst

checkAccessing :: AssignedType -> [Constant] -> StaticChecker AssignedType
checkAccessing (ArrType (ArrType (innerty))) [] = throwError "Cannot assign Condition to array"
checkAccessing (ArrType ty) [] = return (ArrType ty)
checkAccessing ty [] = return ty
checkAccessing (ArrType innerty) (c:cs) = checkAccessing innerty cs
checkAccessing (ty) (c:cs) = throwError "Int is not subscriptable"

checkStatement :: Statement -> StaticChecker ()
checkStatement (Assignment lval (AssignOperator op) cond) = do
    checkLValue lval
    condType <- checkCondition cond
    case (lval) of
        (LValue (Identifier id) const) -> do
            idType <- lookupType id
            targetTy <- checkAccessing idType const
            case condType of
                (ProcedureType innerty) -> do
                    if innerty == targetTy
                    then return ()
                    else throwError ("Cannot Assign Procedure with return type " ++ (show condType) ++ " to " ++ (show targetTy))
                _ ->
                    if condType == targetTy
                    then return ()
                    else throwError ("Cannot Assign " ++ (show condType) ++ " to " ++ (show targetTy))
        _ -> throwError "IDK how"
checkStatement (IfStatement cond stat1 stat2) = do
    checkCondition cond
    checkStatement stat1
    checkStatement stat2
checkStatement (WriteStatement exp) = do
    ty <- checkExp exp
    return ()
checkStatement (ReadStatement lval) = do
    ty <- checkLValue lval
    return ()
checkStatement (WhileStatement cond stat) = do
    checkCondition cond
    checkStatement stat
checkStatement (CompoundStatement stmtList) = do
    checkStatementList stmtList
checkStatement (CallStatement id params) = return ()
checkStatement (ForStatement header stmt) = do
    checkForHeader header
    checkStatement stmt
checkStatement (ReturnStatement assign) = return ()

checkForHeader :: ForHeader -> StaticChecker ()
checkForHeader (ForRegular assign cond exp) = do
    case assign of
        (Assignment lval (AssignOperator op) cond) ->
            case lval of
                (LValue (Identifier id) _) -> do
                    ty <- checkCondition cond
                    assignVar id ty
checkForHeader (ForEach (Identifier lid) rid) = do
    erid <- checkLValue rid
    case erid of
        ArrType ty -> assignVar lid ty
        _ -> throwError ("Can't iterate over " ++ show erid)

checkLValue :: LValue -> StaticChecker AssignedType
checkLValue (LValue (Identifier id) consts) = 
    case id of
        "True" -> return BoolType
        "False" -> return BoolType
        _ -> lookupType id

checkCondition :: Condition -> StaticChecker AssignedType
checkCondition (NotCondition cond) = checkCondition cond
checkCondition (SimpleCondition cond) = checkRelationCondition cond
checkCondition (LogicCondition lcond op rcond) = do
    checkRelationCondition lcond
    checkCondition rcond
    return BoolType

checkRelationCondition :: RelationalCondition -> StaticChecker AssignedType
checkRelationCondition (SimpleRelCondition exp) = checkExp exp
checkRelationCondition (ComplexRelCondition lexp op rexp) = do
    checkExp lexp
    checkExp rexp
    return BoolType

checkExp :: Exp -> StaticChecker AssignedType
checkExp (SingleExp op term) = do
    case op of
        "" -> checkTerm term
        _ -> return IntType
checkExp (BinaryExp op term exp) = return IntType

checkTerm :: Term -> StaticChecker AssignedType
checkTerm (SingleFactor fact) = checkFactor fact
checkTerm (BinaryTerm fact op term) = return IntType

checkFactor :: Factor -> StaticChecker AssignedType
checkFactor (FactorNumber _) = return IntType
checkFactor (FactorLValue lval) = checkLValue lval
checkFactor (FactorParen cond) = checkCondition cond
checkFactor (FactorCall (CallStatement (Identifier id) _)) = lookupType id
checkFactor (ArrayLiteral (exp: exps)) = do
    ty <- checkExp exp
    checkArray exps ty

checkArray :: [Exp] -> AssignedType -> StaticChecker AssignedType
checkArray [] ty = return (ArrType ty)
checkArray (exp : exps) ty = do
    e <- checkExp exp
    if e == ty
    then checkArray exps ty
    else throwError "Array is not all of one type"

nullScope :: Scope
nullScope = Scope (Map.fromList [("malloc", ProcedureType (ArrType IntType)), ("length", ProcedureType IntType)]) [] nullScope 

runStaticChecker :: StaticChecker a -> Scope -> IO (Either String (a, Scope))
runStaticChecker action scope = runExceptT (runStateT action scope)