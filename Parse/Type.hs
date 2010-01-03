module Parse.Types (stype, tvar) where

import Syntax
import Text.ParserCombinators.Parsec hiding (label)

stype :: Parser SType
stype = stype' False

stype' :: Bool -> Parser SType
stype' nested = try (wrap forall) <|> try (wrap label) <|> try (wrap fun) <|> lump <|> nat <|> tyvar where
  wrap parser = if nested then wrapped else parser where
    wrapped = do
      char '('
      spaces
      t <- parser
      spaces
      char ')'
      return t

forall :: Parser SType
forall = parser <?> "forall type" where
  parser = do
    char 'A'
    spaces
    TyVar v <- tyvar
    spaces
    char '.'
    spaces
    b <- stype' True
    return $ Forall v b

label :: Parser SType
label = parser <?> "label type" where
  parser = do
    t <- stype' True
    spaces
    char '^'
    spaces
    n <- many1 digit
    return $ Label t (read n)

fun :: Parser SType
fun = parser <?> "function type" where
  parser = do
    p <- stype' True
    spaces
    string "->"
    spaces
    r <- stype' True
    return $ Fun p r

lump :: Parser SType
lump = parser <?> "lump type" where
  parser = do
    char 'L'
    return Lump

nat :: Parser SType
nat = parser <?> "number type" where
  parser = do
    char 'N'
    return Nat

tvar :: Parser TVar
tvar = do
  c <- lower
  a <- many (lower <|> digit)
  p <- many (char '\'')
  return $ c : a ++ p

tyvar :: Parser SType
tyvar = parser <?> "variable type" where
  parser = do
    v <- tvar
    return $ TyVar v
