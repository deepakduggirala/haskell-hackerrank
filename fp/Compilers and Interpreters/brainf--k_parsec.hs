module BrainFuck2 where


import           Control.Monad
import           Data.Char
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy
import           Control.Monad.Except
import           Text.Parsec             hiding ( count )
import           Text.Parsec.String

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
  between comments comments
    $   forwardP
    <|> backwardP
    <|> incrementP
    <|> decrementP
    <|> outputP
    <|> inputP
    <|> loopP

comments :: Parser String
comments = many (noneOf "><+-.,[]")

bfToken :: Char -> Instruction -> Parser Instruction
bfToken c i = char c >> return i


forwardP = bfToken '>' Forward
backwardP = bfToken '<' Backward
incrementP = bfToken '+' Increment
decrementP = bfToken '-' Decrement
outputP = bfToken '.' Output
inputP = bfToken ',' Input
loopP = Loop <$> between (char '[') (char ']') (many instP)

parseIntrutions s = case parse (many instP) "" s of
  Left  _     -> []
  Right insts -> insts

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

