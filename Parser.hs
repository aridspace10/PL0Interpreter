{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where
import           Control.Applicative
import           GHC.Natural              (Natural)
import GHC.TypeLits (Nat)
import FileIO
import Data.List (filter)
import Grammer

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
kwFor = "for";
kwMinusEquals = "-=";
kwPlusEquals = "+=";
kwNew = "new";
kwArray = "array";

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
    parseArrayType
    <|>
    parseTypeIdentifer

parseArrayType :: Parser Type
parseArrayType = do
    symbol kwArray
    symbol lparen
    ty <- parseType
    symbol rparen
    return (ArrayType ty)

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
    ty <- parseType
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
    symbol kwNew
    ty <- parseTypeIdentifer
    symbol lparen
    c <- parseConstant
    symbol rparen
    return (ArrayCreation lval ty c)
    <|> do
    lval <- parseLValue
    symbol assign
    symbol lbracket
    fcond <- parseCondition
    rcond <- many parseConds
    symbol rbracket
    return (ArrayBuild lval (fcond : rcond))
    <|> do
    lval <- parseLValue
    symbol assign
    cond <- parseCondition
    return (Assignment "" lval cond)
    <|> do
    lval <- parseLValue
    symbol kwPlusEquals
    cond <- parseCondition
    return (Assignment "+" lval cond)
    <|> do
    lval <- parseLValue
    symbol kwMinusEquals
    cond <- parseCondition
    return (Assignment "-" lval cond)
  where
    parseConds = do
        symbol ","
        parseCondition 

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
    <|> parseForStatement
    <|> parseCompoundStatement

parseForStatement :: Parser Statement
parseForStatement = do
    symbol kwFor
    symbol lparen
    header <- parseForHeader
    symbol rparen
    symbol kwDo
    stat <- parseStatement
    return (ForStatement header stat)

parseForHeader :: Parser ForHeader
parseForHeader = do
    assign <- parseAssignment 
    symbol semicolon
    cond <- parseCondition
    symbol semicolon
    exp <- parseExp
    return (ForHeader assign cond exp)

peekSymbol :: String -> Parser Bool
peekSymbol s = P $ \cs ->
  case parse (symbol s) cs of
    Just _  -> Just (True, cs)
    Nothing -> Just (False, cs)

parseStatementList :: Parser StatementList
parseStatementList = do
    firstStatement <- parseStatement
    restStatements <- many parseStatementWithSemicolon
    return $ buildStatementList (firstStatement : restStatements)
  where
    parseStatementWithSemicolon = do
        symbol semicolon
        parseStatement
    
    buildStatementList [s] = SimpleStatement s
    buildStatementList (s:ss) = ComplexStatement s (buildStatementList ss)
    buildStatementList [] = EmptyStatement -- not reachable

parseOptionalStatementList :: Parser StatementList
parseOptionalStatementList = do
    symbol semicolon
    stat <- parseStatement
    rest <- parseOptionalStatementList
    case rest of
        EmptyStatement -> return (SimpleStatement stat)
        _ -> return (ComplexStatement stat rest)
    <|>
    return EmptyStatement

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
    rest <- parseAdditionalExp
    case rest of
        Empty -> return (SingleExp op term)
        _ -> return (BinaryExp "" term rest)

parseAdditionalExp :: Parser Exp
parseAdditionalExp = do
    symbol "+"
    term <- parseTerm
    rest <- parseAdditionalExp
    case rest of
        Empty -> return (SingleExp "+" term)
        _ -> return (BinaryExp "-" term rest)
    <|> do
    symbol "-"
    term <- parseTerm
    rest <- parseAdditionalExp
    case rest of
        Empty -> return (SingleExp "-" term)
        _ -> return (BinaryExp "-" term rest)
    <|> return Empty

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
    space
    id <- identifier
    ops <- many parseOptionalAccesses
    return $ LValue id ops
  where
    parseOptionalAccesses = do
        symbol lbracket
        c <- parseConstant
        symbol rbracket
        return c

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