module IntuitiveLanguage where

import           Text.Parsec             hiding ( State )
import           Text.Parsec.Char
import           Text.Parsec.String             ( Parser )
import           Text.Parsec.Expr
import           Data.Functor.Identity
import           Control.Monad
import           Text.Parsec.Language           ( emptyDef )
import qualified Text.Parsec.Token             as Token
import           Control.Monad.State.Lazy
import           Control.Monad.Except
import qualified Data.Map.Strict               as M
import qualified Data.Either                   as E
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.List                      ( intercalate )
import           Data.Ratio

data Stmt = Seq [Stmt]
  | Declare String Integer [Expression]
  | Assign AssignExpr
  | Loop Expression AssignExpr
  | WhatIs [ApplyExpr]
  deriving (Show)

newtype AssignExpr = AssignExpr [(String,Expression)] deriving (Show)
data ApplyExpr = ApplyExpr String [Expression] deriving (Show)

data Expression = Var String
  | IntConst Integer
  | Neg Expression
  | ABinary ABinOp Expression Expression
  | FValue ApplyExpr
  deriving (Show)

data ABinOp = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

-- reserved names : is, of, assign, what, function, do, and, to

languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   = [ "is"
                            , "of"
                            , "assign"
                            , "what"
                            , "function"
                            , "do"
                            , "and"
                            , "to"
                            ]
  , Token.reservedOpNames = ["+", "-", "*", "/", "="]
  , Token.caseSensitive   = False
  }

lexer = Token.makeTokenParser languageDef


whiteSpace = Token.whiteSpace lexer
dot = Token.dot lexer
identifier = Token.identifier lexer
reserved = Token.reserved lexer
integer = Token.integer lexer
reservedOp = Token.reservedOp lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
comma = Token.comma lexer
colon = Token.colon lexer
symbol = Token.symbol lexer
parens = Token.parens lexer

iParser :: Parser Stmt
iParser = whiteSpace *> statement

statement :: Parser Stmt
statement = do
  list <- many1 statement'
  return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = declareStmt' <|> loopStmt <|> Assign <$> assignExpr <|> whatIs

-- %Variable% is function of %N%: %Expression1%, %Expression2%, ..., %ExpressionN%, %Expression0%.

declareStmt' = try declareStmt <|> simpleDeclareStmt


-- %Variable% is function of %N%: %Expression1%, %Expression2%, ..., %ExpressionN%, %Expression0%.

declareStmt :: Parser Stmt
declareStmt = do
  var <- identifier
  reserved "is"
  reserved "function"
  reserved "of"
  numArgs <- integer
  colon
  exprs <- sepBy1 expression comma
  dot
  return $ Declare var numArgs exprs

-- %Variable% is %Expression%. 

simpleDeclareStmt :: Parser Stmt
simpleDeclareStmt = do
  var <- identifier
  reserved "is"
  expr <- expression
  dot
  return $ Declare var 0 [expr]

-- do {Expression} [Assign]! 
-- [] - one or more, {} - literals

loopStmt :: Parser Stmt
loopStmt = do
  reserved "do"
  cond       <- braces expression
  assignment <- assignExpr
  return $ Loop cond assignment

-- Assign %Expression1% to %Variable1% [ AND %Expression2% to %Variable2% ... ]!
-- assign 3 to P AND (4+5) to Q AND (Q-15) to R!

assignExpr :: Parser AssignExpr
assignExpr = do
  reserved "assign"
  assignments <- sepBy1 (singleAssign) (reserved "AND")
  symbol "!"
  return $ AssignExpr assignments

singleAssign :: Parser (String, Expression)
singleAssign = do
  value <- expression
  reserved "to"
  var <- identifier
  return (var, value)

-- what is %Function1 call% [AND %Variable1% [ AND %Function2 call% ... ] ]?
-- Function call: %Name%[Expression1][Expression2]...[ExpressionM]
-- what is Sum[10] AND A?
-- what is Sum[20][10]?    

whatIs :: Parser Stmt
whatIs = do
  reserved "what"
  reserved "is"
  call1  <- funcCall
  others <- many $ reserved "AND" >> funcCall
  symbol "?"
  return $ WhatIs (call1 : others)

funcCall :: Parser ApplyExpr
funcCall = do
  name <- identifier
  args <- many (brackets expression)
  return $ ApplyExpr name args

expression :: Parser Expression
expression = buildExpressionParser operators term

operators :: OperatorTable String () Identity Expression
operators =
  [ [Prefix (Neg <$ reservedOp "-")]
  , [ Infix (ABinary Multiply <$ reservedOp "*") AssocLeft
    , Infix (ABinary Divide <$ reservedOp "/")   AssocLeft
    ]
  , [ Infix (ABinary Add <$ reservedOp "+")      AssocLeft
    , Infix (ABinary Subtract <$ reservedOp "-") AssocLeft
    ]
  ]

term :: Parser Expression
term =
  try (FValue <$> funcCall) <|> (IntConst <$> integer) <|> parens expression

parse' :: Parser a -> String -> Either String a
parse' p s = first show (parse p "" s)

parseIL = parse' iParser

showRat :: Rational -> String
showRat r = if denominator r == 1
  then show (numerator r)
  else show (numerator r) ++ "/" ++ show (denominator r)

data Value = C Rational | F [Rational]

data Env = Env { store :: M.Map String Value, output :: [String] }

type InterpM = ExceptT String (State Env)

lookupVar :: String -> InterpM Value
-- lookupVar = undefined
lookupVar name = do
  s <- store <$> get
  maybe (throwError $ "Variable " ++ name ++ " is not found in store")
        return
        (M.lookup name s)

lookupIntVar :: String -> InterpM Rational
lookupIntVar name = do
  val <- lookupVar name
  case val of
    C n -> return n
    F _ -> throwError $ "Variable " ++ name ++ " is not a variable"

lookupFunVar :: String -> InterpM [Rational]
lookupFunVar name = do
  val <- lookupVar name
  case val of
    C _  -> throwError $ "Variable " ++ name ++ " is not a function"
    F xs -> return xs

updateVar :: String -> InterpM Value -> InterpM ()
-- updateVar = undefined
updateVar name mx = do
  env <- get
  x   <- mx
  put $ env { store = M.insert name x (store env) }

outputStr :: String -> InterpM ()
outputStr out = do
  env <- get
  put $ env { output = out : output env }

ratToInt :: Rational -> Int
ratToInt r = fromInteger $ numerator r `div` denominator r

interp :: Stmt -> InterpM ()
interp (Seq stmts          ) = mapM_ interp stmts
interp (Declare name 0 args) = updateVar name (interpExpr' (head args))
interp (Declare name _ args) = updateVar name (F <$> mapM interpExpr args)
interp (Assign (AssignExpr kvs)) =
  mapM_ (\(name, expr) -> updateVar name $ interpExpr' expr) kvs
interp (Loop countExpr aexpr) = do
  count <- interpExpr countExpr
  replicateM_ (ratToInt count) $ interp (Assign aexpr)
interp (WhatIs appExprs) = mapM_ interpApply appExprs

interpApply :: ApplyExpr -> InterpM ()
interpApply (ApplyExpr name argExprs) = do
  val <- lookupVar name
  case val of
    C n   -> outputStr $ showRat n
    F fun -> do
      args <- mapM interpExpr argExprs
      outputStr $ apply fun args

apply :: [Rational] -> [Rational] -> String
apply fun args =
  let fun'      = rearrange fun
      evaled    = sum . map (uncurry (*)) $ zip fun' (1 : args)
      remaining = drop (min (length fun) (length args + 1)) fun'
  in  intercalate ", " . map showRat $ (remaining ++ [evaled])


rearrange []  = []
rearrange [x] = [x]
rearrange xs  = last xs : init xs

interpExpr' :: Expression -> InterpM Value
interpExpr' expr = C <$> interpExpr expr

interpExpr :: Expression -> InterpM Rational
interpExpr (Var      name   ) = lookupIntVar name
interpExpr (IntConst n      ) = return (n % 1)
interpExpr (Neg      expr   ) = ((-1) *) <$> interpExpr expr
interpExpr (ABinary op e1 e2) = case op of
  Add      -> (+) <$> x1 <*> x2
  Subtract -> (-) <$> x1 <*> x2
  Multiply -> (*) <$> x1 <*> x2
  Divide   -> (/) <$> x1 <*> x2
 where
  x1 = interpExpr e1
  x2 = interpExpr e2
interpExpr (FValue (ApplyExpr name argExprs)) = do
  val <- lookupVar name
  case val of
    C n   -> return n
    F fun -> do
      args <- mapM interpExpr argExprs
      if length fun /= 1 + length args
        then throwError $ "insufficient arguments for " ++ name
        else return $ sum . map (uncurry (*)) $ zip (rearrange fun) (1 : args)

runInterp :: Stmt -> Either String Env
runInterp stmt = f (runState (runExceptT (interp stmt)) (Env M.empty []))
  where f (es, st) = second (const st) es

eval :: String -> Either String Env
eval program = parse' iParser (replace '/' "/ " program) >>= runInterp

replace :: Eq a => a -> [a] -> [a] -> [a]
replace _     _           []       = []
replace match replacement (x : xs) = if x == match
  then replacement ++ replace match replacement xs
  else x : replace match replacement xs

main :: IO ()
main = interact (either id showEnv . eval)
  where showEnv env = unlines . reverse $ output env
