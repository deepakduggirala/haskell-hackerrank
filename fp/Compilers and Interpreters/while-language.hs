
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

data BExpr = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Show)

-- relational binary operators
data RBinOp = Greater | Less deriving (Show)


-- binary boolean operators
data BBinOp = And | Or deriving (Show)

-- arithmatic expressions
data AExpr = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)

-- arithmatic binary operator
data ABinOp = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

-- Statement
data Stmt = Seq [Stmt]
  | Assign String AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  deriving (Show)

languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   = [ "if"
                            , "then"
                            , "else"
                            , "while"
                            , "do"
                            , "true"
                            , "false"
                            , "not"
                            , "and"
                            , "or"
                            ]
  , Token.reservedOpNames = [ "+"
                            , "-"
                            , "*"
                            , "/"
                            , "<"
                            , ">"
                            , ":="
                            , "and"
                            , "or"
                            , "not"
                            , "{"
                            , "}"
                            ]
  }

lexer = Token.makeTokenParser languageDef

whiteSpace = Token.whiteSpace lexer
parens = Token.parens lexer
semi = Token.semi lexer
reserved = Token.reserved lexer
identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
integer = Token.integer lexer

-- while language parser
-- S  ::= x := a | S1; S2 | ( S ) | if b then S1 else S2 | while b do { S }
-- a  ::= x | n | - a | a opa a

-- b  ::= true | false | not b | b opb b | a opr a

-- opa ::= + | - | * | /

-- opb ::= and | or

-- opr ::= > | <


whileParser :: Parser Stmt
whileParser = whiteSpace *> statement

statement :: Parser Stmt
statement = parens statement <|> sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
  list <- (sepBy1 statement' semi)
  return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = ifStmt <|> whileStmt <|> assignStmt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- bExpression
  reserved "then"
  stmt1 <- between (reservedOp "{") (reservedOp "}") statement
  reserved "else"
  stmt2 <- between (reservedOp "{") (reservedOp "}") statement
  return $ If cond stmt1 stmt2


whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- bExpression
  reserved "do"
  stmt <- between (reservedOp "{") (reservedOp "}") statement
  return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  reservedOp ":="
  expr <- aExpression
  return $ Assign var expr

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators :: OperatorTable String () Identity AExpr
aOperators =
  [ [Prefix (Neg <$ reservedOp "-")]
  , [ Infix (ABinary Multiply <$ reservedOp "*") AssocLeft
    , Infix (ABinary Divide <$ reservedOp "/")   AssocLeft
    ]
  , [ Infix (ABinary Add <$ reservedOp "+")      AssocLeft
    , Infix (ABinary Subtract <$ reservedOp "-") AssocLeft
    ]
  ]

aTerm :: Parser AExpr
aTerm = Var <$> identifier <|> IntConst <$> integer <|> parens aExpression

bOperators :: OperatorTable String () Identity BExpr
bOperators =
  [ [Prefix (Not <$ reservedOp "not")]
  , [ Infix (BBinary And <$ reservedOp "and") AssocLeft
    , Infix (BBinary Or <$ reservedOp "or")   AssocLeft
    ]
  ]

bTerm :: Parser BExpr
bTerm = BoolConst <$> bool <|> parens bExpression <|> rExpression

bool :: Parser Bool
bool = True <$ reserved "true" <|> False <$ reserved "false"

rExpression :: Parser BExpr
rExpression = do
  a1 <- aExpression
  op <- relation
  a2 <- aExpression
  return $ RBinary op a1 a2

relation :: Parser RBinOp
relation = Greater <$ reservedOp ">" <|> Less <$ reserved "<"


type Store = M.Map String Integer

type InterpM = ExceptT String (State Store)

lookupVar :: String -> InterpM Integer
lookupVar name = do
  store <- get
  maybe (throwError $ "Variable " ++ name ++ " was not found in store")
        return
        (M.lookup name store)

updateVar :: String -> InterpM Integer -> InterpM ()
updateVar name mx = do
  store <- get
  x     <- mx
  put $ M.insert name x store

interp :: Stmt -> InterpM ()
interp (Seq stmts          ) = mapM_ interp stmts
interp (Assign name stmt   ) = updateVar name $ interpArith stmt
interp (If cond stmt1 stmt2) = do
  b <- interpBool cond
  if b then interp stmt1 else interp stmt2
interp (While cond stmt) = do
  b <- interpBool cond
  when b $ interp stmt >> interp (While cond stmt)

interpBool :: BExpr -> InterpM Bool
interpBool (BoolConst x     ) = return x
interpBool (Not       bexp  ) = not <$> interpBool bexp
interpBool (BBinary op b1 b2) = case op of
  And -> (&&) <$> x1 <*> x2
  Or  -> (||) <$> x1 <*> x2
  where
  x1 = interpBool b1
  x2 = interpBool b2
interpBool (RBinary op a1 a2) = case op of
  Greater -> (>) <$> x1 <*> x2
  Less    -> (<) <$> x1 <*> x2
  where
  x1 = interpArith a1
  x2 = interpArith a2

interpArith :: AExpr -> InterpM Integer
interpArith (Var      name   ) = lookupVar name
interpArith (IntConst x      ) = return x
interpArith (Neg      x      ) = (* (-1)) <$> interpArith x
interpArith (ABinary op a1 a2) = case op of
  Add      -> (+) <$> x1 <*> x2
  Subtract -> (-) <$> x1 <*> x2
  Multiply -> (*) <$> x1 <*> x2
  Divide   -> div <$> x1 <*> x2
  where
  x1 = interpArith a1
  x2 = interpArith a2

runInterp :: Stmt -> Either String Store
runInterp stmt = f $ runState (runExceptT (interp stmt)) M.empty
  where f (es, st) = second (const st) es

parse' :: Parser a -> String -> Either String a
parse' p s = first show (parse p "" s)

eval :: String -> Either String Store
eval program = parse' whileParser program >>= runInterp

main :: IO ()
main = interact (either id showStore . eval)
  where
  showStore m = unlines $ map (\(k, a) -> k ++ " " ++ show a) (M.toList m)
