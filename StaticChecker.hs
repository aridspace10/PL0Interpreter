{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module StaticChecker where
import Parser
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import GHC.Natural ( Natural )

data Error = Error Natural String
data AssignedType = IntType | BoolType | RefType String deriving (Eq)
data Scope = Scope SymTable [Error] Scope
type SymTable = Map.Map String AssignedType
type StaticChecker a = StateT Scope (Except String) a

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
        nothing -> throwError ("Undefined error " ++ id)


countUnresolvedTypes :: Int -> [(String , AssignedType)] -> Int
countUnresolvedTypes count [] = count
countUnresolvedTypes count (x:xs) = do
    case x of
        (_, RefType str) -> countUnresolvedTypes (count + 1) xs
        _ -> countUnresolvedTypes count xs

resolveTypes :: StaticChecker ()
resolveTypes = do
    Scope symTable errors parent <- get
    let before = countUnresolvedTypes 0 (Map.toList symTable)
    let after = countUnresolvedTypes 0 (Map.toList symTable)
    if after == 0
    then return ()
    else if before == after
        then throwError ("Unresolved Types !!!!!!!!!!")
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

checkConstDef :: [ConstDef] -> StaticChecker ()
checkConstDef [] = return ()
checkConstDef ((ConstDef (Identifier id) const):cds) = do
    case lookup id of
        nothing -> do
            case const of
                (ConstNumber (Number op num)) -> assignVar id IntType
                (ConstIdentifier (Identifier otherid)) -> do
                    ty <- lookupType otherid
                    assignVar id ty
        _ -> addError (Error 0 ("Reassignment of " ++ id))
    checkConstDef cds

checkVarDef :: [VarDecl] -> StaticChecker ()
checkVarDef ((VarDecl (Identifier id) ty):vds) = do
    case ty of
        (TypeIdentifer tid) -> do
            case tid of
                (Identifier tid') -> do
                    case tid' of
                        "int" -> assignVar id IntType
                        "bool" -> assignVar id BoolType
    checkVarDef vds

checkStatementList :: StatementList -> StaticChecker ()
checkStatementList (SimpleStatement stat) = do
    checkStatement stat
checkStatementList (ComplexStatement stat statLst) = do
    checkStatement stat
    checkStatementList statLst

checkStatement :: Statement -> StaticChecker ()
checkStatement (Assignment lval cond) = do
    checkLValue lval
    condType <- checkCondition cond
    case (lval) of
        (LValue (Identifier id)) -> do
            idType <- lookupType id
            if idType == condType
            then return ()
            else throwError "Cannot Assign"
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

checkLValue :: LValue -> StaticChecker AssignedType
checkLValue (LValue (Identifier id)) = lookupType id

checkCondition :: Condition -> StaticChecker AssignedType
checkCondition (SimpleCondition exp) = checkExp exp
checkCondition (RelationalCondition lexp op rexp) = do
    checkExp lexp
    checkExp rexp
    return BoolType

checkExp :: Exp -> StaticChecker AssignedType
checkExp (SingleExp op term) = do
    case op of
        "" -> checkTerm term
        _ -> return IntType

checkTerm :: Term -> StaticChecker AssignedType
checkTerm (SingleFactor fact) = checkFactor fact

checkFactor :: Factor -> StaticChecker AssignedType
checkFactor (FactorNumber _) = return IntType
checkFactor (FactorLValue lval) = checkLValue lval
checkFactor (FactorParen cond) = checkCondition cond