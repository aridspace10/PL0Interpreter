module Interpreter where
import Parser
import qualified Data.Map as Map

type Env = Map.Map String Value
data Value 
  = IntVal Int 
  | BoolVal Bool
  deriving (Show, Eq)