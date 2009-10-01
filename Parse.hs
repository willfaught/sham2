module Parse where

import Syntax
import Text.ParserCombinators.Parsec hiding (label)

stype :: Parser SType
stype = lump <|> nat <|> tyvar <|> forall

lump :: Parser SType
lump = do
  char 'L'
  (stype' Lump <|> return Lump)

nat :: Parser SType
nat = do
  char 'N'
  (stype' Nat <|> return Nat)

tyvar :: Parser SType
tyvar = do
  c <- lower
  cs <- many (lower <|> digit)
  ps <- many (char '\'')
  let x = TyVar $ c : cs ++ ps
  ((stype' x) <|> return x)

forall :: Parser SType
forall = do
  char 'A'
  spaces
  TyVar v <- tyvar
  spaces
  char '.'
  spaces
  b <- stype
  let x = Forall v b
  ((stype' $ x) <|> return x)

stype' :: SType -> Parser SType
stype' t = label t <|> fun t

empty :: SType -> Parser SType
empty = return

label :: SType -> Parser SType
label t = do
  spaces
  char '^'
  spaces
  n <- many1 digit
  let x = Label t (read n)
  ((stype' $ x) <|> return x)

fun :: SType -> Parser SType
fun t = do
  spaces
  string "->"
  spaces
  u <- stype
  let x = Fun t u
  ((stype' $ x) <|> return x)

parseH :: String -> HExp
parseH = undefined
