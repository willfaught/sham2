module Parse (stype, hexp, mexp, sexp) where

import Syntax
import Text.ParserCombinators.Parsec hiding (label)

evar :: Parser EVar
evar = do
  c <- lower
  cs <- many (lower <|> digit)
  ps <- many (char '\'')
  return $ c : cs ++ ps

tvar :: Parser TVar
tvar = do
  c <- lower
  a <- many (lower <|> digit)
  p <- many (char '\'')
  return $ c : a ++ p

-- Type

stype :: Parser SType
stype = stype' False

stype' :: Bool -> Parser SType
stype' nested = try (wrap forall) <|> try (wrap fun) <|> try (wrap label) <|> lump <|> nat <|> tyvar where
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

fun :: Parser SType
fun = parser <?> "function type" where
  parser = do
    p <- stype' True
    spaces
    string "->"
    spaces
    r <- stype' True
    return $ Fun p r

label :: Parser SType
label = parser <?> "label type" where
  parser = do
    t <- stype' True
    spaces
    char '^'
    spaces
    n <- many1 digit
    return $ Label t (read n)

lump :: Parser SType
lump = parser <?> "lump type" where
  parser = do
    char 'L'
    return Lump

nat :: Parser SType
nat = parser <?> "natural number type" where
  parser = do
    char 'N'
    return Nat

tyvar :: Parser SType
tyvar = parser <?> "variable type" where
  parser = do
    v <- tvar
    return $ TyVar v

-- Haskell
    
hexp :: Parser HExp
hexp = hexp' False

hexp' :: Bool -> Parser HExp
hexp' nested = try (wrap hfunapp)
  <|> try (wrap htyapp)
  <|> try (wrap hadd)
  <|> try (wrap hfix)
  <|> try (wrap hfunabs)
  <|> try (wrap hif0)
  <|> try (wrap hm)
  <|> try hnum
  <|> try (wrap hs)
  <|> try (wrap hsub)
  <|> try (wrap htyabs)
  <|> try hvar
  <|> try (wrap hwrong) where
  wrap parser = if nested then wrapped else parser where
    wrapped = do
      char '('
      spaces
      t <- parser
      spaces
      char ')'
      return t

hadd :: Parser HExp
hadd = parser <?> "addition" where
  parser = do
    char '+'
    spaces
    e1 <- hexp' True
    many1 space
    e2 <- hexp' True
    return $ HAdd e1 e2

hfix :: Parser HExp
hfix = parser <?> "hfixed-point operation" where
  parser = do
    string "hfix"
    many1 space
    e <- hexp' True
    return $ HFix e

hfunabs :: Parser HExp
hfunabs = parser <?> "function abstraction" where
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

hfunapp :: Parser HExp
hfunapp = parser <?> "function application" where
  parser = do
    e1 <- hexp' True
    many1 space
    e2 <- hexp' True
    return $ HFunApp e1 e2

hif0 :: Parser HExp
hif0 = parser <?> "condition" where
  parser = do
    string "hif0"
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
    e <- mexp
    return $ HM t e

hnum :: Parser HExp
hnum = parser <?> "natural number" where
  parser = do
    n <- many1 digit
    return $ HNum (read n)

hs :: Parser HExp
hs = parser <?> "HS guard" where
  parser = do
    string "HS"
    many1 space
    t <- stype
    many1 space
    e <- sexp
    return $ HS t e

hsub :: Parser HExp
hsub = parser <?> "subtraction" where
  parser = do
    char '-'
    spaces
    e1 <- hexp' True
    many1 space
    e2 <- hexp' True
    return $ HSub e1 e2

htyabs :: Parser HExp
htyabs = parser <?> "type abstraction" where
  parser = do
    string "\\\\"
    spaces
    v <- tvar
    spaces
    char '.'
    e <- hexp' True
    return $ HTyAbs v e

htyapp :: Parser HExp
htyapp = parser <?> "type application" where
  parser = do
    e <- hexp' True
    many1 space
    char '{'
    spaces
    t <- stype
    spaces
    char '}'
    return $ HTyApp e t

hvar :: Parser HExp
hvar = parser <?> "variable" where
  parser = do
    v <- evar
    return $ HVar v

hwrong :: Parser HExp
hwrong = parser <?> "wrong" where
  parser = do
    string "wrong"
    many1 space
    t <- stype
    many1 space
    char '\"'
    s <- many1 $ noneOf ['\"']
    char '\"'
    return $ HWrong t s

-- ML

mexp :: Parser MExp
mexp = mexp' False

mexp' :: Bool -> Parser MExp
mexp' nested = try (wrap mfunapp)
  <|> try (wrap mtyapp)
  <|> try (wrap madd)
  <|> try (wrap mfix)
  <|> try (wrap mfunabs)
  <|> try (wrap mh)
  <|> try (wrap mif0)
  <|> try mnum
  <|> try (wrap ms)
  <|> try (wrap msub)
  <|> try (wrap mtyabs)
  <|> try mvar
  <|> try (wrap mwrong) where
  wrap parser = if nested then wrapped else parser where
    wrapped = do
      char '('
      spaces
      t <- parser
      spaces
      char ')'
      return t

madd :: Parser MExp
madd = parser <?> "addition" where
  parser = do
    char '+'
    spaces
    e1 <- mexp' True
    many1 space
    e2 <- mexp' True
    return $ MAdd e1 e2

mfix :: Parser MExp
mfix = parser <?> "fixed-point operation" where
  parser = do
    string "mfix"
    many1 space
    e <- mexp' True
    return $ MFix e

mfunabs :: Parser MExp
mfunabs = parser <?> "function abstraction" where
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
    e <- mexp' True
    return $ MFunAbs x t e

mfunapp :: Parser MExp
mfunapp = parser <?> "function application" where
  parser = do
    e1 <- mexp' True
    many1 space
    e2 <- mexp' True
    return $ MFunApp e1 e2

mif0 :: Parser MExp
mif0 = parser <?> "condition" where
  parser = do
    string "mif0"
    many1 space
    c <- mexp' True
    many1 space
    t <- mexp' True
    many1 space
    f <- mexp' True
    return $ MIf0 c t f

mh :: Parser MExp
mh = parser <?> "MH guard" where
  parser = do
    string "MH"
    many1 space
    t <- stype
    many1 space
    e <- hexp
    return $ MH t e

mnum :: Parser MExp
mnum = parser <?> "natural number" where
  parser = do
    n <- many1 digit
    return $ MNum (read n)

ms :: Parser MExp
ms = parser <?> "MS guard" where
  parser = do
    string "MS"
    many1 space
    t <- stype
    many1 space
    e <- sexp
    return $ MS t e

msub :: Parser MExp
msub = parser <?> "subtraction" where
  parser = do
    char '-'
    spaces
    e1 <- mexp' True
    many1 space
    e2 <- mexp' True
    return $ MSub e1 e2

mtyabs :: Parser MExp
mtyabs = parser <?> "type abstraction" where
  parser = do
    string "\\\\"
    spaces
    v <- tvar
    spaces
    char '.'
    e <- mexp' True
    return $ MTyAbs v e

mtyapp :: Parser MExp
mtyapp = parser <?> "type application" where
  parser = do
    e <- mexp' True
    many1 space
    char '{'
    spaces
    t <- stype
    spaces
    char '}'
    return $ MTyApp e t

mvar :: Parser MExp
mvar = parser <?> "variable" where
  parser = do
    v <- evar
    return $ MVar v

mwrong :: Parser MExp
mwrong = parser <?> "wrong" where
  parser = do
    string "wrong"
    many1 space
    t <- stype
    many1 space
    char '\"'
    s <- many1 $ noneOf ['\"']
    char '\"'
    return $ MWrong t s

-- Scheme

sexp :: Parser SExp
sexp = sexp' False

sexp' :: Bool -> Parser SExp
sexp' nested = wrap sadd
  <|> wrap sfunabs
  <|> try (wrap sfunapp)
  <|> wrap sh
  <|> wrap sif0
  <|> wrap sm
  <|> snum
  <|> wrap ssub
  <|> svar
  <|> wrap swrong where
  wrap parser = if nested then wrapped else parser where
    wrapped = do
      char '('
      spaces
      t <- parser
      spaces
      char ')'
      return t

sadd :: Parser SExp
sadd = parser <?> "addition" where
  parser = do
    char '+'
    spaces
    e1 <- sexp' True
    many1 space
    e2 <- sexp' True
    return $ SAdd e1 e2

sfunabs :: Parser SExp
sfunabs = parser <?> "function abstraction" where
  parser = do
    char '\\'
    spaces
    x <- evar
    spaces
    char '.'
    spaces
    e <- sexp' True
    return $ SFunAbs x e

sfunapp :: Parser SExp
sfunapp = parser <?> "function application" where
  parser = do
    e1 <- sexp' True
    many1 space
    e2 <- sexp' True
    return $ SFunApp e1 e2

sh :: Parser SExp
sh = parser <?> "SH guard" where
  parser = do
    string "SH"
    many1 space
    t <- stype
    many1 space
    e <- hexp
    return $ SH t e

sif0 :: Parser SExp
sif0 = parser <?> "condition" where
  parser = do
    string "if0"
    many1 space
    c <- sexp' True
    many1 space
    t <- sexp' True
    many1 space
    f <- sexp' True
    return $ SIf0 c t f

sm :: Parser SExp
sm = parser <?> "SM guard" where
  parser = do
    string "SM"
    many1 space
    t <- stype
    many1 space
    e <- mexp
    return $ SM t e

snum :: Parser SExp
snum = parser <?> "natural number" where
  parser = do
    n <- many1 digit
    return $ SNum (read n)

ssub :: Parser SExp
ssub = parser <?> "subtraction" where
  parser = do
    char '-'
    spaces
    e1 <- sexp' True
    many1 space
    e2 <- sexp' True
    return $ SSub e1 e2

svar :: Parser SExp
svar = parser <?> "variable" where
  parser = do
    v <- evar
    return $ SVar v

swrong :: Parser SExp
swrong = parser <?> "wrong" where
  parser = do
    string "wrong"
    many1 space
    char '\"'
    s <- many1 $ noneOf ['\"']
    char '\"'
    return $ SWrong s
