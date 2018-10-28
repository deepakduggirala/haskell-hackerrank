-- Doesn't work when there are exactly 10^5 operations. 

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

forward :: Memory -> Memory
forward ([ x ] , bs) = ([0], x : bs)
forward (x : xs, bs) = (xs, x : bs)

backward :: Memory -> Memory
backward (xs, []    ) = (0 : xs, [])
backward (xs, b : bs) = (b : xs, bs)

increment :: Memory -> Memory
increment (m : ms, bs) = ((m + 1) : ms, bs)

decrement :: Memory -> Memory
decrement (m : ms, bs) = ((m - 1) : ms, bs)

output :: Memory -> Int
output (m : ms, _) = m

input :: Int -> Memory -> Memory
input x (m : ms, bs) = (x : ms, bs)

type IM = ([Int], Memory, Int)

go :: [Instruction] -> IM -> IO IM
go []             im                    = return im
-- go _              im@(_, _, op_limit) = return im
go (inst : insts) im@(inp, mem, numops) = if numops >= op_limit
  then return im
  else do
-- print numops
    im' <- applyInst inst im
    go insts im'

applyInst :: Instruction -> IM -> IO IM
applyInst inst im@(inp, mem, numops) = if numops >= op_limit
  then return im
  else case inst of
    Forward   -> return (inp, forward mem, numops + 1)
    Backward  -> return (inp, backward mem, numops + 1)
    Increment -> return (inp, increment mem, numops + 1)
    Decrement -> return (inp, decrement mem, numops + 1)
    Output    -> do
      putChar $ chr $ output mem
      return (inp, mem, numops + 1)
    Input    -> return (tail inp, input (head inp) mem, numops + 1)
    Loop ins -> loop ins im
    -- Loop ins -> case mem of
    --   ((0 : _), _) -> return (inp, mem, numops + 2)
    --   _            -> do
    --     (inp', mem', numops') <- go ins (inp, mem, numops + 1)
    --     applyInst (Loop ins) (inp', mem', numops' + 1)

loop :: [Instruction] -> IM -> IO IM
loop ins im@(inp, mem, numops) = case mem of
  ((0 : _), _) -> return (inp, mem, numops + 2)
  _            -> do
    im'@(inp', mem', numops') <- go ins (inp, mem, numops + 1)
    if numops' >= op_limit
      then return im'
      else case mem' of
        ((0 : _), _) -> return (inp', mem', numops' + 1)
        _            -> loop ins (inp', mem', numops' + 1)

emptyMemory = ([0], [])
op_limit = 100000

interpret :: [Int] -> String -> IO IM
interpret input text =
  let ins = parseIntrutions text in go ins (input, emptyMemory, 0)

main :: IO ()
main = do
  getLine
  input          <- map ord . init <$> getLine
  text           <- getContents
  (_, _, numops) <- interpret input text
  when (numops >= op_limit) $ putStrLn "\nPROCESS TIME OUT. KILLED!!!"

