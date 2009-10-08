module TypeParser (stype, tvar) where

import Syntax
import Text.ParserCombinators.Parsec hiding (label)

stype :: Parser SType
stype = chainl1 stype' (fun <?> "function abstraction")

stype' = do
  t <- stype''
  let labels = do n <- many (label <?> "label") ; return $ foldl Label t n
  option t labels

stype'' :: Parser SType
stype'' = (forall <?> "type abstraction")
  <|> (lump <?> "lump")
  <|> (nat <?> "number")
  <|> (parens <?> "parentheses")
  <|> (tyvar <?> "type variable")

forall :: Parser SType
forall = do
  char 'A'
  spaces
  v <- tvar
  spaces
  char '.'
  spaces
  b <- stype
  return $ Forall v b

fun :: Parser (SType -> SType -> SType)
fun = do
  spaces
  string "->"
  spaces
  return Fun

label :: Parser Int
label = do
  spaces
  char '^'
  spaces
  n <- many1 digit
  return $ read n

lump :: Parser SType
lump = do
  char 'L'
  return Lump

nat :: Parser SType
nat = do
  char 'N'
  return Nat

parens :: Parser SType
parens = between (char '(') (char ')') stype

tvar :: Parser TVar
tvar = do
  c <- lower
  cs <- many (lower <|> digit)
  ps <- many (char '\'')
  return $ c : cs ++ ps

tyvar :: Parser SType
tyvar = do
  v <- tvar
  return $ TyVar v
