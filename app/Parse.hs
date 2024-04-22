{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Parse (Expr (..), Stmt (..), Variable, Number, parseStmt) where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Number = Integer

type Variable = String

data Expr a where
  -- Arithmetic expressions
  NumLit :: Number -> Expr Number
  Var :: Variable -> Expr Number
  Add :: Expr Number -> Expr Number -> Expr Number
  Mul :: Expr Number -> Expr Number -> Expr Number
  Sub :: Expr Number -> Expr Number -> Expr Number
  -- Boolean expressions
  TrueLit :: Expr Bool
  FalseLit :: Expr Bool
  Eq :: Expr Number -> Expr Number -> Expr Bool
  LessEq :: Expr Number -> Expr Number -> Expr Bool
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Neg :: Expr Bool -> Expr Bool

deriving instance Show (Expr a) -- TODO: would be nice with a pretty printer

data Stmt
  = Assign Variable (Expr Number)
  | Skip
  | Sequence [Stmt]
  | If (Expr Bool) Stmt Stmt
  | While (Expr Bool) Stmt
  deriving (Show) -- TODO: would be nice to use a pretty printer

-- I've really not used (mega)parsec before,
-- so most stuff is taken from this intro tutorial, that also
-- happens to use the same language for the examples :)
-- https://github.com/mrkkrp/megaparsec-site/blob/master/tutorials/parsing-simple-imperative-language.md

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keyword :: Text -> Parser Text
keyword word = lexeme (string word <* notFollowedBy alphaNumChar)

keywords :: [String]
keywords = ["if", "then", "else", "while", "do", "skip", "true", "false"]

ident :: Parser String
ident = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` keywords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

unary :: Text -> (a -> a) -> Operator Parser a
unary name f = Prefix (f <$ symbol name)

pNumber :: Parser (Expr Number)
pNumber = NumLit <$> lexeme L.decimal

pVariable :: Parser (Expr Number)
pVariable = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pTermNum :: Parser (Expr Number)
pTermNum = parens pExprNum <|> pVariable <|> pNumber

operatorTableNum :: [[Operator Parser (Expr Number)]]
operatorTableNum =
  [ [binary "*" Mul],
    [ binary "+" Add,
      binary "-" Sub
    ]
  ]

pExprNum :: Parser (Expr Number)
pExprNum = makeExprParser pTermNum operatorTableNum

operatorTableBool :: [[Operator Parser (Expr Bool)]]
operatorTableBool = [[unary "!" Neg], [binary "&&" And]]

pTermBool :: Parser (Expr Bool)
pTermBool =
  parens pExprBool
    <|> (FalseLit <$ keyword "false")
    <|> (TrueLit <$ keyword "true")
    <|> booleanRelation

booleanRelation :: Parser (Expr Bool)
booleanRelation = do
  lhs <- pExprNum
  op <- Eq <$ symbol "=" <|> LessEq <$ symbol "<="
  rhs <- pExprNum
  return (op lhs rhs)

pExprBool :: Parser (Expr Bool)
pExprBool = makeExprParser pTermBool operatorTableBool

parseStmt :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Stmt
parseStmt = runParser parser
  where
    parser = between spaceConsumer eof pStmt

pStmt :: Parser Stmt
pStmt = parens pStmt <|> pStmtSeq

pStmtSeq :: Parser Stmt
pStmtSeq = f <$> sepBy1 pStmt' (symbol ";")
  where
    f l = if length l == 1 then head l else Sequence l

pStmt' :: Parser Stmt
pStmt' = pSkip <|> pIf <|> pIf <|> pAssign <|> pWhile

pSkip :: Parser Stmt
pSkip = Skip <$ keyword "skip"

pIf :: Parser Stmt
pIf = do
  let _ = trace "if"
  _ <- keyword "if"
  cond <- pExprBool
  _ <- keyword "then"
  yes <- pStmt
  _ <- keyword "else"
  no <- pStmt
  return (If cond yes no)

pWhile :: Parser Stmt
pWhile = do
  let _ = trace "while"
  _ <- keyword "while"
  cond <- pExprBool
  _ <- keyword "do"
  loop <- pStmt
  return (While cond loop)

pAssign :: Parser Stmt
pAssign = do
  name <- ident
  _ <- symbol ":="
  value <- pExprNum
  return (Assign name value)
