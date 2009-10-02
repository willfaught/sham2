module TypeParser (parserT) where

import Syntax
import Text.ParserCombinators.Parsec hiding (label)

parserT :: Parser SType
parserT = stype

stype :: Parser SType
stype = lump <|> nat <|> tyvar <|> forall <|> parens

lump :: Parser SType
lump = do
  char 'L'
  stype' Lump

nat :: Parser SType
nat = do
  char 'N'
  stype' Nat

tyvar :: Parser SType
tyvar = do
  c <- lower
  cs <- many (lower <|> digit)
  ps <- many (char '\'')
  stype' . TyVar $ c : cs ++ ps

forall :: Parser SType
forall = do
  char 'A'
  spaces
  TyVar v <- tyvar
  spaces
  char '.'
  spaces
  b <- stype
  stype' $ Forall v b

parens :: Parser SType
parens = do
  char '('
  spaces
  t <- stype
  spaces
  char ')'
  spaces
  stype' t

stype' :: SType -> Parser SType
stype' t = try (label t) <|> fun t <|> empty t

empty :: SType -> Parser SType
empty = return

label :: SType -> Parser SType
label t = do
  spaces
  char '^'
  spaces
  n <- many1 digit
  stype' $ Label t (read n)

fun :: SType -> Parser SType
fun t = do
  spaces
  string "->"
  spaces
  u <- stype
  stype' $ Fun t u
