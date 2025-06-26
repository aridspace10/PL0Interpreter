{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where
import           Control.Applicative
import           GHC.Natural              (Natural)
import GHC.TypeLits (Nat)
import FileIO
import Data.List (filter)

assign = ":=";
colon = ":";
semicolon = ";";
range = "..";
lparen = "(";
rparen = ")";
lbracket = "[";
rbracket = "]";
equal = "=";
nequal = "!=";
less = "<";
greater = ">";
lesseq = "<=";
greatereq = ">=";
plus = "+";
minus = "-";
times = "*";
divides = "/";
kwBegin = "begin";
kwCall = "call";
kwConst = "const";
kwDo = "do";
kwElse = "else";
kwEnd = "end";
kwIf = "if";
kwProcedure = "procedure";
kwRead = "read";
kwThen = "then";
kwType = "type";
kwVar = "var";
kwWhile = "while";
kwWrite = "write";

newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap g pa = do
      a <- pa
      return $ g a

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P (\cs -> Just (a,cs))

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> pa = do
      g <- pg
      g <$> pa

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P $ \cs ->
        case parse p cs of
          Nothing        -> Nothing
          Just (a, str') -> parse (f a) str'

instance Alternative Parser where
    empty :: Parser a
    empty = P $ \str -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \cs ->
        case parse p cs of
          Nothing -> parse q cs
          mx      -> mx

-- aux function for removing decorator
parse :: Parser a -> String -> Maybe (a, String)
parse (P p) cs = p cs

-- parse one character
item :: Parser Char
item = P $ foo
  where
    foo (c:cs) = Just $ (c, cs)
    foo _      = Nothing

-- parse a char c when P c.
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

-- parse a digit
parseDigit :: Parser Char
parseDigit = sat (\x -> elem x ['0'..'9'])

-- parse the character x
char :: Char -> Parser Char
char x = sat (== x)

-- parse the string xs
string :: String -> Parser String
string []     = return []
string (x:xs) = (\x xs -> x:xs) <$> (char x) <*> (string xs)

-- parse a natural number
nat :: Parser Natural
nat = read <$> (some parseDigit)

-- throw away space
space :: Parser ()
space = (\x -> ()) <$> (many $ char ' ')

parseOptionalsString :: [String] -> Parser String
parseOptionalsString opts =
  asum (map tryString opts) <|> return ""
  where
    tryString s = string s

-- ignore surrounding whitespace
token :: Parser a -> Parser a
token pa = do
    space
    a <- pa
    space
    return a

-- parse a symbol, ignoring whitespace
symbol :: String -> Parser String
symbol xs = token $ string xs

isSymbol :: String -> Parser Bool
isSymbol xs = (symbol xs >> return True) <|> return False

data KW_IF = KW_IF String deriving (Show)
data KW_THEN = KW_THEN String deriving (Show)
data KW_ELSE = KW_ELSE String deriving (Show)
data KW_WHILE = KW_WHILE String deriving (Show)
data KW_READ = KW_READ String deriving (Show)
data KW_WRITE = KW_WRITE String deriving (Show)
data KW_PROCEDURE = KW_PROCEDURE String deriving (Show)
data KW_CALL = KW_CALL String deriving (Show)
data KW_BEGIN = KW_BEGIN String deriving (Show)
data KW_END = KW_END String deriving (Show)
data KW_DO = KW_DO String deriving (Show)
data LPAREN = LPAREN String deriving (Show)
data RPAREN = RPAREN String deriving (Show)
data ASSIGN = ASSIGN String deriving (Show)

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
data StatementList = ComplexStatement Statement StatementList | SimpleStatement Statement deriving Show
data Statement =
    -- Assignment -> LValue Assign Condition
    Assignment LValue Condition
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
    | CompoundStatement StatementList deriving Show

data Exp =
    SingleExp String Term
    | BinaryExp String Term Exp  -- operator, left term, right expression
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

parseProgram :: Parser Program
parseProgram = do
    b <- parseBlock
    return (Program b)

parseBlock :: Parser Block
parseBlock = do
    decs <- parseDeclerationList
    cs <- parseCompoundStatement
    return (Block decs cs)

parseDeclerationList :: Parser DecleratonList
parseDeclerationList = do
    decs <- many parseDecleration
    return (DecleratonList decs)

parseDecleration :: Parser Decleration
parseDecleration = do
    c <- parseConstDefList
    return (DecConstDefList c)
    <|> do
    c <- parseTypeDefList
    return (DecTypeDefList c)
    <|> do
    c <- parseVarDeclList
    return (DecVarDeclList c)
    <|> do
    c <- parseProcedureDef
    return (DecProcedureDef c)

parseConstDefList :: Parser ConstDefList
parseConstDefList = do
    symbol kwConst
    f <- parseConstDef
    m <- many parseConstDef
    return (ConstDefList (f : m))

parseConstDef :: Parser ConstDef
parseConstDef = do
    id <- identifier
    symbol equal
    const <- parseConstant
    symbol semicolon
    return (ConstDef id const)

parseNum :: Parser Number
parseNum = do
    op <- parseOptionalsString ["-", "+"]
    num <- nat
    return (Number op num)

parseConstant :: Parser Constant
parseConstant = do
    num <- parseNum
    return (ConstNumber num)
    <|> do
    id <- identifier
    return (ConstIdentifier id)
    <|> do
    op <- parseOptionalsString ["-"]
    c <- parseConstant
    return (ConstMinus c)

parseTypeDefList :: Parser TypeDefList
parseTypeDefList = do
    symbol kwType
    t1 <- parseTypeDef
    t2 <- many parseTypeDef
    return (TypeDefList (t1 : t2))

parseTypeDef :: Parser TypeDef
parseTypeDef = do
    id <- identifier
    symbol equal
    ty <- parseType
    symbol semicolon
    return (TypeDef id ty)


parseType :: Parser Type
parseType =
    parseSubrangeType
    <|>
    parseTypeIdentifer

parseSubrangeType :: Parser Type
parseSubrangeType = do
    symbol lbracket
    c1 <- parseConstant
    symbol range
    c2 <- parseConstant
    symbol rbracket
    return (SubrangeType c1 c2)

parseTypeIdentifer :: Parser Type
parseTypeIdentifer = do
    id <- identifier
    return (TypeIdentifer id)

parseVarDeclList :: Parser VarDeclList
parseVarDeclList = do
    symbol kwVar
    var1 <- parseVarDecl
    var2 <- many parseVarDecl
    return (VarDeclList (var1 : var2))


parseVarDecl :: Parser VarDecl
parseVarDecl = do
    id <- identifier
    symbol colon
    ty <- parseTypeIdentifer
    symbol semicolon
    return (VarDecl id ty)

parseProcedureDef :: Parser ProcedureDef
parseProcedureDef = do
    head <- parseProcedureHead
    symbol equal
    blk <- parseBlock
    symbol semicolon
    return (ProcedureDef head blk)

parseProcedureHead :: Parser ProcedureHead
parseProcedureHead = do
    symbol kwProcedure
    id <- identifier
    symbol lparen
    symbol rparen
    return (ProcedureHead id)

parseIfStatement :: Parser Statement
parseIfStatement = do
    symbol kwIf
    cond <- parseCondition
    symbol kwThen
    stat1 <- parseStatement
    symbol kwElse
    stat2 <- parseStatement
    return (IfStatement cond stat1 stat2)

parseAssignment :: Parser Statement
parseAssignment = do
    lval <- parseLValue
    symbol assign
    cond <- parseCondition
    return (Assignment lval cond)

parseReadStatement :: Parser Statement
parseReadStatement = do
    symbol kwRead
    lval <- parseLValue
    return (ReadStatement lval)

parseCallStatement :: Parser Statement
parseCallStatement = do
    symbol kwCall
    ident <- identifier
    symbol lparen
    symbol rparen
    return (CallStatement ident)

parseWhileStatement :: Parser Statement
parseWhileStatement = do
    symbol kwWhile
    cond <- parseCondition
    symbol kwDo
    stat <- parseStatement
    return (WhileStatement cond stat)

parseWriteStatement :: Parser Statement
parseWriteStatement = do
    symbol kwWrite
    exp <- parseExp
    return (WriteStatement exp)

parseCompoundStatement :: Parser Statement
parseCompoundStatement = do
    symbol kwBegin
    lst <- parseStatementList
    symbol kwEnd
    return (CompoundStatement lst)

parseStatement :: Parser Statement
parseStatement =
        parseAssignment
    <|> parseCallStatement
    <|> parseReadStatement
    <|> parseWriteStatement
    <|> parseWhileStatement
    <|> parseIfStatement
    <|> parseCompoundStatement

parseStatementList :: Parser StatementList
parseStatementList = do
    stat <- parseStatement
    do symbol semicolon
       rest <- parseStatementList
       return (ComplexStatement stat rest)
      <|> return (SimpleStatement stat)

parseOptional :: [String] -> Parser String
parseOptional ([]) = return ""
parseOptional (str:strs) = do
    symbol str
    return str
    <|> parseOptional strs

parseExp :: Parser Exp
parseExp = do
    op <- parseOptional ["+", "-"]
    term <- parseTerm
    ex <- parseExp
    return (BinaryExp op term ex)
    <|> do
    op <- parseOptional ["+", "-"]
    term <- parseTerm
    return (SingleExp op term)

parseTerm :: Parser Term
parseTerm = do
    f <- parseFactor
    symbol "*"
    t <- parseTerm
    return (BinaryTerm f "*" t)
    <|> do
    f <- parseFactor
    symbol "/"
    t <- parseTerm
    return (BinaryTerm f "/" t)
    <|> do
    f <- parseFactor
    return (SingleFactor f)


parseRelOp :: Parser RelOp
parseRelOp = do
    symbol "="
    return (RelOp "=")
    <|> do
    symbol ">"
    return (RelOp ">")
    <|> do
    symbol ">="
    return (RelOp ">=")
    <|> do
    symbol "<"
    return (RelOp "<")
    <|> do
    symbol "<="
    return (RelOp "<=")
    <|> do
    symbol "!="
    return (RelOp "!=")

parseCondition :: Parser Condition
parseCondition = do
    exp1 <- parseExp
    op <- parseRelOp
    exp2 <- parseExp
    return (RelationalCondition exp1 op exp2)
    <|> do
    exp <- parseExp
    return (SimpleCondition exp)


parseFactor :: Parser Factor
parseFactor =
        (FactorNumber <$> number)
    <|> (FactorLValue <$> parseLValue)
    <|> do
        symbol lparen
        cond <- parseCondition
        symbol rparen
        return (FactorParen cond)

parseLValue :: Parser LValue
parseLValue = do
    id <- identifier
    return (LValue id)

number :: Parser Natural
number = do
    n <- nat
    return n

identifier :: Parser Identifier
identifier = do
    first <- sat isAlpha
    rest <- many (sat isAlphaNum)
    return (Identifier (first : rest))

isAlpha :: Char -> Bool
isAlpha c = elem c (['a'..'z'] ++ ['A'..'Z'])
isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || elem c ['0'..'9']

removeNewlines :: String -> String
removeNewlines = map (\c -> if c == '\n' then ' ' else c)

parseFile :: FilePath -> IO ()
parseFile path = do
  res <- readFileContents path
  case res of
    Left err -> putStrLn ("Error reading file: " ++ err)
    Right content -> do
        let result = removeNewlines content
        print (parse parseProgram result)