module HaskellParserP (hexp) where

import Prelude hiding (exp)
import TypeParserP
import Syntax
import Text.ParserCombinators.Parsec

add :: Parser HExp
add = do
  char '+'
  spaces
  e1 <- exp
  many1 space
  e2 <- exp
  return $ HAdd e1 e2

evar :: Parser EVar
evar = do
  c <- lower
  cs <- many (lower <|> digit)
  ps <- many (char '\'')
  let v = c : cs ++ ps
  if v `elem` ["fix", "if0", "wrong"]
    then fail $ "the variable '" ++ v ++ "' is reserved"
    else return v

exp :: Parser HExp
exp = (add <?> "addition")
  <|> (num <?> "number")
  <|> (parens <?> "parentheses")
  <|> (sub <?> "subtraction")
  <|> try (var <?> "variable")
  <|> (fix <?> "fixed-point operation")
  <|> (if0 <?> "condition")
  <|> (wrong <?> "wrong")
  <|> try (funabs <?> "function abstraction")
  <|> (tyabs <?> "type abstraction")

fix :: Parser HExp
fix = do
  string "fix"
  many1 space
  e <- exp
  return $ HFix e

funabs :: Parser HExp
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
  e <- exp
  return $ HFunAbs x t e

funapp :: Parser (HExp -> HExp -> HExp)
funapp = do
  many1 space
  return HFunApp

hexp :: Parser HExp
hexp = chainl1 exp (funapp <?> "function application")

if0 :: Parser HExp
if0 = do
  string "if0"
  many1 space
  c <- exp
  many1 space
  t <- exp
  many1 space
  f <- exp
  return $ HIf0 c t f

num :: Parser HExp
num = do
  n <- many1 digit
  return $ HNum (read n)

parens :: Parser HExp
parens = do
  char '('
  spaces
  e <- exp
  spaces
  char ')'
  spaces
  return e

sub :: Parser HExp
sub = do
  char '-'
  spaces
  e1 <- exp
  many1 space
  e2 <- exp
  return $ HSub e1 e2

tyabs :: Parser HExp
tyabs = do
  string "\\\\"
  spaces
  v <- tvar
  spaces
  char '.'
  e <- exp
  return $ HTyAbs v e

tyapp :: HExp -> Parser HExp
tyapp e = do
  spaces
  char '{'
  spaces
  t <- stype
  spaces
  char '}'
  return $ HTyApp e t

var :: Parser HExp
var = do
  v <- evar
  return $ HVar v

wrong :: Parser HExp
wrong = do
  string "wrong"
  many1 space
  t <- stype
  many1 space
  char '\"'
  s <- many1 $ noneOf ['\"']
  char '\"'
  return $ HWrong t s
