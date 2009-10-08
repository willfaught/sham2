module HaskellParser (hexp) where

import Prelude hiding (exp)
import TypeParser
import Syntax
import Text.ParserCombinators.Parsec

hexp :: Parser HExp
hexp = chainl1 exp (funapp <?> "function application")

funapp :: Parser (HExp -> HExp -> HExp)
funapp = do
  many1 space
  return HFunApp

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

add :: Parser HExp
add = do
  char '+'
  spaces
  e1 <- hexp
  many1 space
  e2 <- hexp
  hexp' $ HAdd e1 e2

fix :: Parser HExp
fix = do
  string "fix"
  many1 space
  e <- hexp
  hexp' $ HFix e

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
  e <- hexp
  hexp' $ HFunAbs x t e

if0 :: Parser HExp
if0 = do
  string "if0"
  many1 space
  c <- hexp
  many1 space
  t <- hexp
  many1 space
  f <- hexp
  hexp' $ HIf0 c t f

num :: Parser HExp
num = do
  n <- many1 digit
  hexp' $ HNum (read n)

parens :: Parser HExp
parens = do
  char '('
  spaces
  e <- hexp
  spaces
  char ')'
  spaces
  hexp' e

sub :: Parser HExp
sub = do
  char '-'
  spaces
  e1 <- hexp
  many1 space
  e2 <- hexp
  hexp' $ HSub e1 e2

tyabs :: Parser HExp
tyabs = do
  string "\\\\"
  spaces
  v <- tvar
  spaces
  char '.'
  e <- hexp
  hexp' $ HTyAbs v e

evar :: Parser EVar
evar = do
  c <- lower
  cs <- many (lower <|> digit)
  ps <- many (char '\'')
  let v = c : cs ++ ps
  if v `elem` ["fix", "if0", "wrong"]
    then fail $ "the variable '" ++ v ++ "' is reserved"
    else return v

var :: Parser HExp
var = do
  v <- evar
  hexp' $ HVar v

wrong :: Parser HExp
wrong = do
  string "wrong"
  many1 space
  t <- stype
  many1 space
  char '\"'
  s <- many1 $ noneOf ['\"']
  char '\"'
  hexp' $ HWrong t s

hexp' :: HExp -> Parser HExp
hexp' e = (try (tyapp e <?> "type application")) <|> empty e

tyapp :: HExp -> Parser HExp
tyapp e = do
  spaces
  char '{'
  spaces
  t <- stype
  spaces
  char '}'
  hexp' $ HTyApp e t

empty :: HExp -> Parser HExp
empty = return
