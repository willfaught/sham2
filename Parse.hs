module Parse (parseH, parseM, parseS, parseT) where

import Prelude hiding (exp)
import Syntax
import Text.ParserCombinators.Parsec

-- Haskell

parseH :: String -> Either ParseError HExp
parseH = parse expH ""

funappH :: Parser HExp
funappH = do
  e1 <- expH
  many1 space
  e2 <- expH
  return $ HFunApp e1 e2

expH :: Parser HExp
expH = do
  char '('
  spaces
  e <- expH'
  spaces
  char ')'
  return e

expH' :: Parser HExp
expH' = (addH <?> "addHition")
  <|> (subH <?> "subHtraction")
  <|> (numH <?> "numHber")
  <|> (hm <?> "ML")
  <|> try (fixH <?> "fixHed-point operation")
  <|> try (if0H <?> "condition")
  <|> try (wrongH <?> "wrongH")
  <|> (varH <?> "varHiable")
  <|> try (funabsH <?> "function abstraction")
  <|> (tyabsH <?> "type abstraction")
  <|> try (tyappH <?> "type application")
  <|> (funappH <?> "function application")

tyappH :: Parser HExp
tyappH = do
  e <- expH
  many1 space
  char '{'
  spaces
  t <- stype
  spaces
  char '}'
  return $ HTyApp e t

addH :: Parser HExp
addH = do
  char '+'
  spaces
  e1 <- expH
  many1 space
  e2 <- expH
  return $ HAdd e1 e2

evarH :: Parser EVar
evarH = do
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
  e <- mexp
  return $ HM t e

fixH :: Parser HExp
fixH = do
  string "fixH"
  many1 space
  e <- expH
  return $ HFix e

funabsH :: Parser HExp
funabsH = do
  char '\\'
  spaces
  x <- evarH
  spaces
  char ':'
  spaces
  t <- stype
  spaces
  char '.'
  spaces
  e <- expH
  return $ HFunAbs x t e


if0H :: Parser HExp
if0H = do
  string "if0H"
  many1 space
  c <- expH
  many1 space
  t <- expH
  many1 space
  f <- expH
  return $ HIf0 c t f

numH :: Parser HExp
numH = do
  n <- many1 digit
  return $ HNum (read n)

parensH :: Parser HExp -> Parser HExp
parensH p = do
  char '('
  spaces
  e <- p
  spaces
  char ')'
  return e

subH :: Parser HExp
subH = do
  char '-'
  spaces
  e1 <- expH
  many1 space
  e2 <- expH
  return $ HSub e1 e2

tyabsH :: Parser HExp
tyabsH = do
  string "\\\\"
  spaces
  v <- tvarH
  spaces
  char '.'
  e <- expH
  return $ HTyAbs v e

varH :: Parser HExp
varH = do
  v <- evarH
  return $ HVar v

wrongH :: Parser HExp
wrongH = do
  string "wrongH"
  many1 space
  t <- stype
  many1 space
  char '\"'
  s <- many1 $ noneOf ['\"']
  char '\"'
  return $ HWrong t s

-- ML



-- Scheme

parseS = undefined
