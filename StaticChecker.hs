{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module StaticChecker where
import Parser
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import GHC.Natural

data Scope = Scope SymTable [Error] Scope
type SymTable = Map.Map String AssignedType
data Error = Error Natural String
data AssignedType = IntType | BoolType
type StaticChecker a = StateT Scope (Except String) a

-- assignVar function
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

checkProgram :: Program -> StaticChecker ()
checkProgram (Program code) = do
    checkBlock code

checkBlock :: Block -> StaticChecker ()
checkBlock (Block decList compStat) = do
    checkDecList decList
    checkCompoundStatement compStat

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

checkCompoundStatement :: CompoundStatement -> StaticChecker ()
checkCompoundStatement (CompoundStatement statlst) = do
    checkStatementList statlst

checkStatementList :: StatementList -> StaticChecker ()
checkStatementList (SimpleStatement stat) = do
    checkStatement stat
checkStatementList (ComplexStatement stat statLst) = do
    checkStatement stat
    checkStatementList statLst

checkStatement :: Statement -> StaticChecker ()
checkStatement (Assignment lval cond) = do
    checkLValue lval
    checkCondition cond
checkStatement (IfStatement cond stat1 stat2) = do
    checkCondition cond
    checkStatement stat1
    checkStatement stat2
checkStatement (WriteStatement exp) = do
    checkExp exp
checkStatement (ReadStatement lval) = do
    checkLValue lval
checkStatement (WhileStatement cond stat) = do
    checkCondition cond
    checkStatement stat

checkLValue :: LValue -> StaticChecker ()
checkLValue = undefined

checkCondition :: Condition -> StaticChecker ()
checkCondition = undefined

checkExp :: Exp -> StaticChecker ()
checkExp = undefined
