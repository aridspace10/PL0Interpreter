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
    | SubrangeType Constant Constant deriving (Show)
data VarDeclList = VarDeclList [VarDecl] deriving (Show)
data VarDecl = VarDecl Identifier Type deriving (Show)
data ProcedureDef = ProcedureDef ProcedureHead Block deriving (Show)
data ProcedureHead = ProcedureHead Identifier deriving Show
data StatementList = ComplexStatement Statement StatementList | SimpleStatement Statement | EmptyStatement deriving Show
data Statement =
    -- Assignment -> LValue Assign Condition
    Assignment String LValue Condition
    -- CallStatement -> KW_CALL Identifier LPAREN RPAREN
    | CallStatement Identifier
    -- ReadStatement -> KW_READ LValue
    | ReadStatement LValue
    -- WriteStatement -> KW_WRITE Exp
    | WriteStatement Exp
    -- WhileStatement -> KW_WHILE Condition KW_DO Statement
    | WhileStatement Condition Statement
    -- IfStatement -> KW_IF Condition KW_THEN Statement KW_ELSE Statement
    | IfStatement Condition Statement Statement
    | ForStatement ForHeader Statement
    | CompoundStatement StatementList deriving Show

data ForHeader = ForHeader Statement Condition Exp deriving Show

data Exp =
    SingleExp String Term
    | BinaryExp String Term Exp  -- operator, left term, right expression
    | Empty
    deriving (Show)

data Term =
    SingleFactor Factor
    | BinaryTerm Factor String Term  -- left factor, operator, right term
    deriving (Show)

data RelOp = RelOp String deriving (Show)

-- Fixed condition to properly store both expressions and operator
data Condition =
    SimpleCondition Exp
    | RelationalCondition Exp RelOp Exp
    deriving (Show)

data Factor =
    FactorNumber Natural
    | FactorLValue LValue
    | FactorParen Condition
    deriving (Show)

data LValue = LValue Identifier deriving (Show)
data Identifier = Identifier String deriving (Show)
data Number = Number String Natural deriving (Show)