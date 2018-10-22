import           Control.Monad
-- import           Data.List                      ( intercalate )
-- import           Data.Function                  ( (&)
--                                                 , fix
--                                                 )
-- import           Test.QuickCheck
import           Control.Applicative
import           Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s

item :: Parser Char
item = P
  (\inp -> case inp of
    []       -> []
    (x : xs) -> [(x, xs)]
  )

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case (parse p inp) of
    [] -> []
    [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> [(x,  inp)])

  -- (<*>) :: Parser (a->b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case (parse pg inp) of
    [] -> []
    [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= g = P (\inp -> case parse p inp of
    [] -> []
    [(v, out)] -> parse (g v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat pred = do
  x <- item
  if pred x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (x ==)

string :: String -> Parser String
string []       = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x  <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Integer
nat = fmap read (some digit)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Integer
int =
  do
      symbol "-"
      x <- nat
      return (-x)
    <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Integer
natural = token nat

integer :: Parser Integer
integer = token int

symbol :: String -> Parser String
symbol s = token (string s)

nats :: Parser [Integer]
nats = do
  symbol "["
  n  <- natural
  ns <- many
    (do
      symbol ","
      natural
    )
  symbol "]"
  return (n : ns)

parenthesis :: Parser a -> Parser a
parenthesis p = do
  symbol "("
  v <- p
  symbol ")"
  return v

{-
Expression ::= Term [+-] Expression
              | Term

Term       ::= Factor [*/] Term
              | Factor

Factor     ::= Number
              | [+-] Factor
              | '(' Expression ')'
-}

expr :: Parser Integer
expr = do
  t <- term
  addP t <|> subP t <|> return t


addP :: Integer -> Parser Integer
addP t = do
  symbol "+"
  e <- expr
  return (t + e)

subP :: Integer -> Parser Integer
subP t = do
  symbol "-"
  e <- expr
  return (t - e)

term :: Parser Integer
term = do
  f <- factor
  mulP f <|> divP f <|> return f

mulP :: Integer -> Parser Integer
mulP f = do
  symbol "*"
  t <- term
  return (f * t)

divP :: Integer -> Parser Integer
divP f = do
  symbol "/"
  t <- term
  return (f `divide` t)

factor :: Parser Integer
factor = parenthesis expr <|> unaryExpr

unaryExpr :: Parser Integer
unaryExpr = negP <|> posP <|> natural

negP :: Parser Integer
negP = do
  symbol "-"
  x <- factor
  return (-x)

posP :: Parser Integer
posP = do
  symbol "+"
  factor

divide :: Integer -> Integer -> Integer
divide x y = if y /= 0 && x `mod` y == 0 then x `div` y else x * modInverse y

modInverse 0 = 0
modInverse a = expMod a (p - 2) p

expMod :: Integer -> Integer -> Integer -> Integer
expMod b e m | e == 0    = 1
             | odd e     = b `mod` m * expMod b (e - 1) m `mod` m
             | otherwise = expMod ((b * b) `mod` m) (e `div` 2) m


p :: Integer
p = 1000000007

solve :: String -> Integer
solve x = case parse expr x of
  [(x, _)] -> x `mod` p

-- solve2 = parse expr

main :: IO ()
main = interact $ show . solve