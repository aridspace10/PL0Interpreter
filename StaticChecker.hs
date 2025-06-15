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

checkProgram :: Program -> Either String [Error]
checkProgram (Program code) = do 
    checkBlock code

checkBlock :: Block -> Either String [Error]
checkBlock = undefined