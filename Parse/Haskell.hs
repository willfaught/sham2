module Parse.Haskell (parseH, hexp) where

import Parse.Types
import Prelude hiding (exp)
import Syntax
import Text.ParserCombinators.Parsec

parseH :: String -> Either ParseError HExp
parseH = parse hexp ""

funapp :: Parser HExp
funapp = do
  e1 <- hexp
  many1 space
  e2 <- hexp
  return $ HFunApp e1 e2

hexp :: Parser HExp
hexp = do
  char '('
  spaces
  e <- hexp'
  spaces
  char ')'
  return e

hexp' :: Parser HExp
hexp' = (add <?> "addition")
  <|> (sub <?> "subtraction")
  <|> (num <?> "number")
  <|> (hm <?> "ML")
  <|> try (fix <?> "fixed-point operation")
  <|> try (if0 <?> "condition")
  <|> try (wrong <?> "wrong")
  <|> (var <?> "variable")
  <|> try (funabs <?> "function abstraction")
  <|> (tyabs <?> "type abstraction")
  <|> try (tyapp <?> "type application")
  <|> (funapp <?> "function application")

tyapp :: Parser HExp
tyapp = do
  e <- hexp
  many1 space
  char '{'
  spaces
  t <- stype
  spaces
  char '}'
  return $ HTyApp e t

add :: Parser HExp
add = do
  char '+'
  spaces
  e1 <- hexp
  many1 space
  e2 <- hexp
  return $ HAdd e1 e2

evar :: Parser EVar
evar = do
  c <- lower
  cs <- many (lower <|> digit)
  ps <- many (char '\'')
  return $ c : cs ++ ps

hm :: Parser HExp
hm = do
  string "HM"
  many1 space
  t <- stype
  many1 space
  e <- char '*'--mexp
  return $ HM Nat (MNum 0)

fix :: Parser HExp
fix = do
  string "fix"
  many1 space
  e <- hexp
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
  e <- hexp
  return $ HFunAbs x t e


if0 :: Parser HExp
if0 = do
  string "if0"
  many1 space
  c <- hexp
  many1 space
  t <- hexp
  many1 space
  f <- hexp
  return $ HIf0 c t f

num :: Parser HExp
num = do
  n <- many1 digit
  return $ HNum (read n)

parens :: Parser HExp -> Parser HExp
parens p = do
  char '('
  spaces
  e <- p
  spaces
  char ')'
  return e

sub :: Parser HExp
sub = do
  char '-'
  spaces
  e1 <- hexp
  many1 space
  e2 <- hexp
  return $ HSub e1 e2

tyabs :: Parser HExp
tyabs = do
  string "\\\\"
  spaces
  v <- tvar
  spaces
  char '.'
  e <- hexp
  return $ HTyAbs v e

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
