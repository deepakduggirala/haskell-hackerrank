module BrainFuck2 where


import           Control.Monad
import           Data.List                      ( intercalate
                                                , sort
                                                , groupBy
                                                , sortBy
                                                )
import           Data.Function                  ( on
                                                , (&)
                                                )

import           Control.Applicative
import           Data.Char
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy
import           Control.Monad.Except

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

data Instruction =
    Forward
  | Backward
  | Increment
  | Decrement
  | Output
  | Input
  | Loop [Instruction] deriving Show

instP :: Parser Instruction
instP =
  forwardP
    <|> backwardP
    <|> incrementP
    <|> decrementP
    <|> outputP
    <|> inputP
    <|> loopP

nonBfChar :: Parser String
nonBfChar = many (sat (not . flip elem "><+-.,[]"))

bfToken :: Parser a -> Parser a
bfToken p = do
  nonBfChar
  v <- p
  nonBfChar
  return v

forwardP :: Parser Instruction
forwardP = do
  bfToken (char '>')
  return Forward

backwardP :: Parser Instruction
backwardP = do
  bfToken (char '<')
  return Backward

incrementP :: Parser Instruction
incrementP = do
  bfToken (char '+')
  return Increment

decrementP :: Parser Instruction
decrementP = do
  bfToken (char '-')
  return Decrement

outputP :: Parser Instruction
outputP = do
  bfToken (char '.')
  return Output

inputP :: Parser Instruction
inputP = do
  bfToken (char ',')
  return Input

loopP :: Parser Instruction
loopP = do
  bfToken (char '[')
  insts <- many instP
  bfToken (char ']')
  return (Loop insts)

parseIntrutions s = case parse (many instP) s of
  []            -> []
  [(insts, "")] -> insts

type ListZipper a = ([a], [a])

type Memory = ListZipper Int

data World = World {
  inp :: [Int],
  out :: [Int],
  mem :: Memory,
  count :: Int
} deriving Show

emptyMemory = ([0], [])
op_limit = 100000

increment :: Memory -> Memory
increment (m : ms, bs) = ((m + 1) `mod` 256 : ms, bs)
increment ([]    , bs) = ([], bs)

decrement :: Memory -> Memory
decrement (m : ms, bs) = ((if m == 0 then 255 else (m - 1)) : ms, bs)
decrement ([]    , bs) = ([], bs)

replace :: Int -> Memory -> Memory
replace x (m : ms, bs) = (x : ms, bs)
replace x ([]    , bs) = ([], bs)

extract :: Memory -> Int
extract (m : ms, _) = m

forward :: Memory -> Memory
forward ([ x ] , bs) = ([0], x : bs)
forward (x : xs, bs) = (xs, x : bs)

backward :: Memory -> Memory
backward (xs, []    ) = (0 : xs, [])
backward (xs, b : bs) = (b : xs, bs)

loop
  :: [Instruction]
  -> StateT World (Except World) ()
  -> StateT World (Except World) ()
loop terms sp = sp >> do
  s <- get
  if head (fst (mem s)) == 0
    then put s
    else
      limit
      >> tick
      >> loop terms (foldM (\_ t -> eval t) () terms)
      >> limit
      >> tick

limit :: StateT World (Except World) ()
limit = do
  s <- get
  when (count s >= op_limit) $ throwError s

tick :: StateT World (Except World) ()
tick = modify (\s -> s { count = count s + 1 })

eval :: Instruction -> StateT World (Except World) ()
eval Input = limit >> tick >> modify
  (\s -> s { inp = tail (inp s), mem = replace (head (inp s)) (mem s) })
eval Output =
  limit >> tick >> modify (\s -> s { out = extract (mem s) : out s })
eval Increment = limit >> tick >> modify (\s -> s { mem = increment $ mem s })
eval Decrement = limit >> tick >> modify (\s -> s { mem = decrement $ mem s })
eval Forward = limit >> tick >> modify (\s -> s { mem = forward $ mem s })
eval Backward = limit >> tick >> modify (\s -> s { mem = backward $ mem s })
eval (Loop terms) = loop terms (void get)

type Instructions = [Instruction]

interp :: Instructions -> World -> Either World World
interp insts w =
  let sp = foldM (\_ inst -> eval inst) () insts in runExcept (execStateT sp w)

interpret :: [Int] -> String -> IO ()
interpret input text =
  let ins   = parseIntrutions text
      world = interp ins $ World input [] emptyMemory 0
  in  either (\w -> printOutput w >> putStrLn "PROCESS TIME OUT. KILLED!!!")
              printOutput
              world

printOutput :: World -> IO ()
printOutput = putStrLn . map chr . reverse . out

main :: IO ()
main = do
  getLine
  input <- map ord . init <$> getLine
  text  <- getContents
  interpret input text

