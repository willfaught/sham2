module MLParser (parseM, mexp) where

import HaskellParser
import Prelude hiding (exp)
import TypeParser
import Syntax
import Text.ParserCombinators.Parsec

parseM :: String -> Either ParseError MExp
parseM = parse mexp ""

funapp :: Parser MExp
funapp = do
  e1 <- mexp
  many1 space
  e2 <- mexp
  return $ MFunApp e1 e2

mexp :: Parser MExp
mexp = do
  char '('
  spaces
  e <- mexp'
  spaces
  char ')'
  return e

mexp' :: Parser MExp
mexp' = (add <?> "addition")
  <|> (sub <?> "subtraction")
  <|> (num <?> "number")
  <|> (mh <?> "haskell")
  <|> try (fix <?> "fixed-point operation")
  <|> try (if0 <?> "condition")
  <|> try (wrong <?> "wrong")
  <|> (var <?> "variable")
  <|> try (funabs <?> "function abstraction")
  <|> (tyabs <?> "type abstraction")
  <|> try (tyapp <?> "type application")
  <|> (funapp <?> "function application")

tyapp :: Parser MExp
tyapp = do
  e <- mexp
  many1 space
  char '{'
  spaces
  t <- stype
  spaces
  char '}'
  return $ MTyApp e t

add :: Parser MExp
add = do
  char '+'
  spaces
  e1 <- mexp
  many1 space
  e2 <- mexp
  return $ MAdd e1 e2

evar :: Parser EVar
evar = do
  c <- lower
  cs <- many (lower <|> digit)
  ps <- many (char '\'')
  return $ c : cs ++ ps

mh :: Parser MExp
mh = do
  string "MH"
  many1 space
  t <- stype
  many1 space
  e <- mexp
  return $ MM t e

fix :: Parser MExp
fix = do
  string "fix"
  many1 space
  e <- mexp
  return $ MFix e

funabs :: Parser MExp
funabs = do
  char '\\'
  spaces
  x <- evar
  spaces
  char ':'
  spaces
  t <- stype
  spaces
  char '.'
  spaces
  e <- mexp
  return $ MFunAbs x t e


if0 :: Parser MExp
if0 = do
  string "if0"
  many1 space
  c <- mexp
  many1 space
  t <- mexp
  many1 space
  f <- mexp
  return $ MIf0 c t f

num :: Parser MExp
num = do
  n <- many1 digit
  return $ MNum (read n)

parens :: Parser MExp -> Parser MExp
parens p = do
  char '('
  spaces
  e <- p
  spaces
  char ')'
  return e

sub :: Parser MExp
sub = do
  char '-'
  spaces
  e1 <- mexp
  many1 space
  e2 <- mexp
  return $ MSub e1 e2

tyabs :: Parser MExp
tyabs = do
  string "\\\\"
  spaces
  v <- tvar
  spaces
  char '.'
  e <- mexp
  return $ MTyAbs v e

var :: Parser MExp
var = do
  v <- evar
  return $ MVar v

wrong :: Parser MExp
wrong = do
  string "wrong"
  many1 space
  t <- stype
  many1 space
  char '\"'
  s <- many1 $ noneOf ['\"']
  char '\"'
  return $ MWrong t s
