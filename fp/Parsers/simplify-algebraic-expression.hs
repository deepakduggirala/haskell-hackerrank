import           Control.Monad
import           Data.List                      ( intercalate
                                                , sort
                                                , groupBy
                                                , sortBy
                                                )
import           Data.Function                  ( on )

import           Control.Applicative
import           Data.Char

liftSnd :: (a -> b) -> (c, a) -> (c, b)
liftSnd f (m, n) = (m, f n)

liftFst :: (a -> b) -> (a, c) -> (b, c)
liftFst f (m, n) = (f m, n)

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

data Expr = E SubExpr | Add SubExpr Expr deriving Show
data SubExpr = Su HalfExpr | Sub HalfExpr SubExpr deriving Show
data HalfExpr = Plus HalfExpr | Minus HalfExpr | H DivTerm deriving Show
data DivTerm = DT Term | Div Term DivTerm deriving Show
data Term = T CatFactor | Mul CatFactor Term  deriving Show
data CatFactor = CF Factor | Cat Factor CatFactor deriving Show
data Factor = F Base | Pow Base Factor deriving Show
data Base = Paren Expr | Val Integer | Var deriving Show

--(24x)*(2x+4)/4"
-- SE [(Exp, Coeff)]
data SimplifiedExpr = SE [(Exp, Coeff)] deriving (Show)
type Coeff = Integer
type Exp = Integer

class Simplify a where
  simplify :: a -> SimplifiedExpr

idExp :: SimplifiedExpr
idExp = SE [(0, 0)]

instance Simplify Expr where
  simplify (E he    ) = simplify he
  simplify (Add t e) = addExprs (simplify t) (simplify e)

instance Simplify SubExpr where
  simplify (Su h) = simplify h
  simplify (Sub t e) = subExprs (simplify t) (simplify e)

instance Simplify HalfExpr where
  simplify (Plus e) = simplify e
  simplify (Minus e) = subExprs idExp (simplify e)
  simplify (H t) = simplify t

instance Simplify DivTerm where
  simplify (DT t) = simplify t
  simplify (Div t dt) = divExprs (simplify t) (simplify dt)

instance Simplify Term where
  simplify (T cf) = simplify cf
  simplify (Mul cf t) = mulExprs (simplify cf) (simplify t)

instance Simplify CatFactor where
  simplify (CF f) = simplify f
  simplify (Cat f cf) = mulExprs (simplify f) (simplify cf)

instance Simplify Factor where
  simplify (F b) = simplify b
  simplify (Pow b f) = powExprs (simplify b) (simplify f)

instance Simplify Base where
  simplify (Paren e) = simplify e
  simplify (Val v) = SE [(0,v)]
  simplify Var = SE [(1,1)]


-- instance Show Expr where
--   show (E t    ) = show t
--   show (Add t e) = show t ++ " + " ++ show e
--   show (Sub t e) = show t ++ " - " ++ show e

-- instance Show Term where
--   show (T fe) = show fe
--   show (Mul fe t) = show fe ++ "*" ++ show t

-- instance Show DivTerm where
--   show (DT t) = show t
--   show (Div fe t) = show fe ++ "/" ++ show t

-- instance Show Factor where
--   show (F b) = show b
--   show (Pow b f) = show b ++ "^" ++ show f

-- instance Show CatFactor where
--   show (CF f) = show f
--   show (Cat f fm) = show f ++  "*"  ++ show fm

-- instance Show Base where
--   show (Paren e) = "( " ++ show e ++ " )"
--   show (Val v) = show v
--   show Var = "x"

-- instance Show HalfExpr where
--   show (Plus h) = "+" ++ show h
--   show (Minus h) = "-" ++ show h
--   show (H dt) = show dt




powExprs :: SimplifiedExpr -> SimplifiedExpr -> SimplifiedExpr
powExprs (SE [(0, v1)]) (SE [(0, v2)]) = SE $ [(0, v1 ^ v2)]
powExprs (SE x        ) (SE [(0, v )]) = SE $ map (liftFst (* v)) x

mulExprs :: SimplifiedExpr -> SimplifiedExpr -> SimplifiedExpr
mulExprs (SE xs) (SE ys) = SE $ mergeExprs $ do
  (ex, cx) <- xs
  (ey, cy) <- ys
  return (ex + ey, cx * cy)

divExprs :: SimplifiedExpr -> SimplifiedExpr -> SimplifiedExpr
divExprs (SE x) (SE [(0, y)]) = SE $ applyToCoeff (`div` y) x

subExprs :: SimplifiedExpr -> SimplifiedExpr -> SimplifiedExpr
subExprs (SE x) (SE y) = SE $ mergeExprs (x ++ applyToCoeff ((-1) *) y)

addExprs :: SimplifiedExpr -> SimplifiedExpr -> SimplifiedExpr
addExprs (SE x) (SE y) = SE $ mergeExprs (x ++ y)

applyToCoeff :: (Integer -> Integer) -> [(Exp, Coeff)] -> [(Exp, Coeff)]
applyToCoeff f = map (liftSnd f)

mergeExprs :: [(Exp, Coeff)] -> [(Exp, Coeff)]
mergeExprs =
  filter ((0 /=) . snd)
    . map (foldr (\(m, n) (accM, accN) -> (m, n + accN)) (0, 0))
    . groupBy (on (==) fst)
    . sort


expr :: Parser Expr
expr = do
  t <- subExpr
  add t <|> return (E t)

subExpr :: Parser SubExpr
subExpr = do
  h <- halfExpr
  sub h <|> return (Su h)

add :: SubExpr -> Parser Expr
add t = do
  symbol "+"
  Add t <$> expr

sub :: HalfExpr -> Parser SubExpr
sub t = do
  symbol "-"
  Sub t <$> subExpr

halfExpr :: Parser HalfExpr
halfExpr = plusExpr <|> minusExpr <|> (H <$> divTerm)

plusExpr :: Parser HalfExpr
plusExpr = do
  symbol "+"
  Plus <$> halfExpr

minusExpr :: Parser HalfExpr
minusExpr = do
  symbol "-"
  Minus <$> halfExpr

divTerm :: Parser DivTerm
divTerm = do
  t <- term
  div' t <|> return (DT t)

term :: Parser Term
term = do
  cf <- catFactor
  mul cf <|> return (T cf)

mul :: CatFactor -> Parser Term
mul f = do
  symbol "*"
  Mul f <$> term

div' :: Term -> Parser DivTerm
div' f = do
  symbol "/"
  Div f <$> divTerm

catFactor :: Parser CatFactor
catFactor = do
  f <- factor
  (Cat f <$> catFactor) <|> return (CF f)

factor :: Parser Factor
factor = do
  b <- base
  (do
      symbol "^"
      f <- factor
      return (Pow b f)
    )
    <|> return (F b)

base :: Parser Base
base =
  (Paren <$> parenthesis expr)
    <|> (Val <$> natural)
    <|> (do
          symbol "x"
          return Var
        )

solveE = parse expr

sim x = case solveE x of
  []       -> "error"
  [(e, _)] -> showSim . simplify $ e

showSim :: SimplifiedExpr -> String
showSim (SE x) = case map showTerm . sortBy (flip compare) $ x of
  []       -> "0"
  [(p, s)] -> if p then s else ("-" ++ s)
  ((p, s) : ts) ->
    (if p then s else "-" ++ s) ++ " " ++ (unwords $ foldr f [] ts)
    where f (p, s) ts = if p then "+" : s : ts else "-" : s : ts

showTerm :: (Integer, Integer) -> (Bool, String)
showTerm (0, c ) = (c >= 0, show . abs $ c)
showTerm (1, 1 ) = (True, "x")
showTerm (1, -1) = (False, "x")
showTerm (1, c ) = (c >= 0, (show . abs $ c) ++ "x")
showTerm (e, 1 ) = (True, "x^" ++ show e)
showTerm (e, -1) = (False, "x^" ++ show e)
showTerm (e, c ) = (c >= 0, (show . abs $ c) ++ "x^" ++ show e)

main :: IO ()
main = interact $ unlines . map sim . tail . lines
