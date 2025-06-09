module Parser where
import           Control.Applicative
import           GHC.Natural              (Natural)
import GHC.Generics (D1)

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

isAlpha :: Char -> Bool
isAlpha c = elem c (['a'..'z'] ++ ['A'..'Z'])
isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || elem c ['0'..'9']

number :: Parser Natural
number = do
    n <- nat
    return n

identifier :: Parser String
identifier = do
    first <- sat isAlpha
    rest <- many (sat isAlphaNum)
    return (first : rest)