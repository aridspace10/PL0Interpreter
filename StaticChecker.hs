module StaticChecker where
import Parser
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Control.Monad.Trans.Accum (look)
import GHC.Natural

-- Scope = SymTable Errors ParentScope
data Scope = Scope SymTable [Error] Scope

type SymTable = Map.Map String Type
data Error = Error Natural String

type StaticChecker a = StateT Scope (Except String) a

checkProgram :: Program -> StaticChecker ()
checkProgram (Program code) = do 
    checkBlock code

checkBlock :: Block -> StaticChecker ()
checkBlock (Block decList compStat) = do
    -- No static checking for decList
    checkCompoundStatement compStat

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
