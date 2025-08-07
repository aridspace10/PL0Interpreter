module Grammer where
import           GHC.Natural              (Natural)

data Program = Program Block deriving (Show)
data Block = Block DecleratonList Statement deriving (Show)
data DecleratonList = DecleratonList [Decleration] deriving (Show)
data Decleration =
    DecConstDefList ConstDefList
    | DecTypeDefList TypeDefList
    | DecVarDeclList VarDeclList
    | DecProcedureDef ProcedureDef deriving (Show)
data ConstDefList = ConstDefList [ConstDef] deriving (Show)
data ConstDef = ConstDef Identifier Constant deriving (Show)
data Constant
  = ConstNumber Number
  | ConstIdentifier Identifier
  | ConstMinus Constant deriving (Show)
data TypeDefList = TypeDefList [TypeDef] deriving (Show)
data TypeDef = TypeDef Identifier Type deriving (Show)
data Type =
    TypeIdentifer Identifier
    | ArrayType Type
    | SubrangeType Constant Constant
    | None deriving (Show)
data VarDeclList = VarDeclList [VarDecl] deriving (Show)
data VarDecl = VarDecl Identifier Type deriving (Show)
data ProcedureDef = ProcedureDef ProcedureHead Block deriving (Show)
data ProcedureHead = ProcedureHead Identifier ParametersList Type deriving Show
data ParametersList = ParametersList [Parameter] deriving Show
data Parameter = Parameter Identifier Type deriving Show
data StatementList = ComplexStatement Statement StatementList | SimpleStatement Statement | EmptyStatement deriving Show
data Statement =
    Assignment LValue AssignOperator Assignables
    | ArrayCreation LValue Type Constant
    | CallStatement Identifier CallParamList
    | ReadStatement LValue
    | WriteStatement Exp
    | WhileStatement Condition Statement
    | IfStatement Condition Statement Statement
    | ForStatement ForHeader Statement
    | ReturnStatement Assignables
    | CompoundStatement StatementList deriving Show

data Assignables = AssignedCall Statement
                   | AssignedCondition Condition deriving Show

data AssignOperator = AssignOperator String deriving Show

data CallParamList = CallParamList [Condition] deriving Show

data ForHeader = ForHeader Statement Condition Exp deriving Show

data Exp =
    SingleExp String Term
    | BinaryExp String Term Exp
    | Empty
    deriving (Show)

data Term =
    SingleFactor Factor
    | BinaryTerm Factor String Term
    deriving (Show)

data RelOp = RelOp String deriving (Show)
data LogOp = LogOp String deriving Show

data Condition =
    NotCondition Condition
    | SimpleCondition RelationalCondition
    | LogicCondition RelationalCondition LogOp Condition
    deriving Show

data RelationalCondition = 
    SimpleRelCondition Exp
    | ComplexRelCondition Exp RelOp Exp
    deriving (Show)

data Factor =
    FactorNumber Natural
    | FactorLValue LValue
    | FactorParen Condition
    | ArrayLiteral [Exp]
    deriving (Show)

data LValue = LValue Identifier [Constant] deriving (Show)
data Identifier = Identifier String deriving (Show)
data Number = Number String Natural deriving (Show)