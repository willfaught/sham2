module Parse.Haskell (hexp) where

import Parse.Types
import Prelude hiding (exp)
import Syntax
import Text.ParserCombinators.Parsec

parseH :: String -> Either ParseError HExp
parseH = parse hexp ""

hexp :: Parser HExp
hexp = hexp' False

hexp' :: Bool -> Parser HExp
hexp' nested = try (wrap add)
  <|> try (wrap fix)
  <|> try (wrap funabs)
  <|> try (wrap funapp)
  <|> try (wrap if0)
  <|> try (wrap hm)
  <|> try (wrap sub)
  <|> try (wrap tyabs)
  <|> try (wrap tyapp)
  <|> wrap wrong
  <|> num
  <|> var where
  wrap parser = if nested then wrapped else parser where
    wrapped = do
      char '('
      spaces
      t <- parser
      spaces
      char ')'
      return t

add :: Parser HExp
add = parser <?> "addition" where
  parser = do
    char '+'
    spaces
    e1 <- hexp' True
    many1 space
    e2 <- hexp' True
    return $ HAdd e1 e2

fix :: Parser HExp
fix = parser <?> "fixed-point operation" where
  parser = do
    string "fix"
    many1 space
    e <- hexp' True
    return $ HFix e

funabs :: Parser HExp
funabs = parser <?> "function abstraction" where
  parser = do
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
    e <- hexp' True
    return $ HFunAbs x t e

funapp :: Parser HExp
funapp = parser <?> "function application" where
  parser = do
    e1 <- hexp' True
    many1 space
    e2 <- hexp' True
    return $ HFunApp e1 e2

if0 :: Parser HExp
if0 = parser <?> "condition" where
  parser = do
    string "if0"
    many1 space
    c <- hexp' True
    many1 space
    t <- hexp' True
    many1 space
    f <- hexp' True
    return $ HIf0 c t f

hm :: Parser HExp
hm = parser <?> "HM guard" where
  parser = do
    string "HM"
    many1 space
    t <- stype
    many1 space
    e <- char '*'--mexp
    return $ HM Nat (MNum 0)

sub :: Parser HExp
sub = parser <?> "subtraction" where
  parser = do
    char '-'
    spaces
    e1 <- hexp' True
    many1 space
    e2 <- hexp' True
    return $ HSub e1 e2

tyabs :: Parser HExp
tyabs = parser <?> "type abstraction" where
  parser = do
    string "\\\\"
    spaces
    v <- tvar
    spaces
    char '.'
    e <- hexp' True
    return $ HTyAbs v e

tyapp :: Parser HExp
tyapp = parser <?> "type application" where
  parser = do
    e <- hexp' True
    many1 space
    char '{'
    spaces
    t <- stype
    spaces
    char '}'
    return $ HTyApp e t

wrong :: Parser HExp
wrong = parser <?> "wrong" where
  parser = do
    string "wrong"
    many1 space
    t <- stype
    many1 space
    char '\"'
    s <- many1 $ noneOf ['\"']
    char '\"'
    return $ HWrong t s

num :: Parser HExp
num = parser <?> "number" where
  parser = do
    n <- many1 digit
    return $ HNum (read n)

evar :: Parser EVar
evar = do
  c <- lower
  cs <- many (lower <|> digit)
  ps <- many (char '\'')
  return $ c : cs ++ ps

var :: Parser HExp
var = parser <?> "variable" where
  parser = do
    v <- evar
    return $ HVar v
