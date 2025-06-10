{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where
import           Control.Applicative
import           GHC.Natural              (Natural)

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
data Identifier = Identifier String deriving (Show)

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
data Term = Term Factor String Factor deriving (Show)
data Exp = Exp String Term String Term deriving (Show)
-- RelOp -> == | >= | > | <= | < | !=
data RelOp = RelOp String deriving (Show)
data Condition = Condition RelCondition deriving (Show)
data RelCondition = RelCondition Exp deriving (Show)
data Factor = FactorNumber Natural | FactorLValue LValue | FactorParen Condition deriving (Show)
data LValue = LValue Identifier deriving (Show)

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

parseStatement :: Parser Statement
parseStatement = do 
    parseAssignment
    <|>
    parseCallStatement
    <|>
    parseReadStatement
    <|>
    parseWriteStatement
    <|>
    parseWhileStatement
    <|>
    parseIfStatement

parseExp :: Parser Exp
parseExp = do
    op1 <- parseOptionalsString ["+", "-"]
    term <- parseTerm
    op2 <- parseOptionalsString ["+", "-"]
    term2 <- parseTerm
    return (Exp op1 term op2 term2)

parseTerm :: Parser Term
parseTerm = do
    f1 <- parseFactor
    op <- parseOptionalsString ["*", "/"]
    f2 <- parseFactor
    return (Term f1 op f2)

parseRelOp :: Parser RelOp
parseRelOp = do
    op <- parseOptionalsString ["==", "!=", ">", ">=", "<", "<="]
    return (RelOp op)

parseCondition :: Parser Condition
parseCondition = do
    cond <- parseRelCondition
    return (Condition cond)

parseRelCondition :: Parser RelCondition
parseRelCondition = do 
    leftExp <- parseExp
    relOp <- parseRelOp
    rightExp <- parseExp
    return (RelCondition leftExp)

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