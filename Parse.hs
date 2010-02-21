module Parse (parseT, parseH, parseM, parseS) where

import Control.Monad
import Syntax
import Text.ParserCombinators.Parsec as P hiding (label)

parseT :: String -> Either ParseError SType
parseT = P.parse p "" where p = do t <- stype ; eof ; return t

parseH :: String -> Either ParseError HExp
parseH = P.parse p "" where p = do e <- hexp ; eof ; return e

parseM :: String -> Either ParseError MExp
parseM = P.parse p "" where p = do e <- mexp ; eof ; return e

parseS :: String -> Either ParseError SExp
parseS = P.parse p "" where p = do e <- sexp ; eof ; return e

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
stype' nested = try (wrap forall)
  <|> try (wrap fun)
  <|> try (wrap label)
  <|> try lump
  <|> try nat
  <|> try tyvar where
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
hexp' nested = try (wrap hadd)
  <|> try (wrap hfix)
  <|> try (wrap hfunabs)
  <|> try (wrap hif0)
  <|> try (wrap hm)
  <|> try (wrap hs)
  <|> try (wrap hsub)
  <|> try (wrap htyabs)
  <|> try (wrap hwrong)
  <|> try (wrap hfunapp)
  <|> try (wrap htyapp)
  <|> try hnum
  <|> try hvar where
  wrap parser = if nested then wrapped else parser where
    wrapped = do
      char '('
      spaces
      t <- parser
      spaces
      char ')'
      return t

hadd :: Parser HExp
hadd = parser <?> "Haskell addition" where
  parser = do
    char '+'
    spaces
    e1 <- hexp' True
    many1 space
    e2 <- hexp' True
    return $ HAdd e1 e2

hfix :: Parser HExp
hfix = parser <?> "Haskell fixed-point operation" where
  parser = do
    string "fix"
    many1 space
    e <- hexp' True
    return $ HFix e

hfunabs :: Parser HExp
hfunabs = parser <?> "Haskell function abstraction" where
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
hfunapp = parser <?> "Haskell function application" where
  parser = do
    e1 <- hexp' True
    many1 space
    e2 <- hexp' True
    return $ HFunApp e1 e2

hif0 :: Parser HExp
hif0 = parser <?> "Haskell condition" where
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
hm = parser <?> "Haskell ML guard" where
  parser = do
    string "HM"
    many1 space
    t <- stype
    many1 space
    e <- mexp' True
    return $ HM t e

hnum :: Parser HExp
hnum = parser <?> "Haskell natural number" where
  parser = do
    n <- many1 digit
    return $ HNum (read n)

hs :: Parser HExp
hs = parser <?> "Haskell Scheme guard" where
  parser = do
    string "HS"
    many1 space
    t <- stype
    many1 space
    e <- sexp' True
    return $ HS t e

hsub :: Parser HExp
hsub = parser <?> "Haskell subtraction" where
  parser = do
    char '-'
    spaces
    e1 <- hexp' True
    many1 space
    e2 <- hexp' True
    return $ HSub e1 e2

htyabs :: Parser HExp
htyabs = parser <?> "Haskell type abstraction" where
  parser = do
    string "\\\\"
    spaces
    v <- tvar
    spaces
    char '.'
    spaces
    e <- hexp' True
    return $ HTyAbs v e

htyapp :: Parser HExp
htyapp = parser <?> "Haskell type application" where
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
hvar = parser <?> "Haskell variable" where
  parser = do
    v <- evar
    when (v `elem` ["fix", "if0", "wrong"]) (fail $ "Invalid variable: " ++ v)
    return $ HVar v

hwrong :: Parser HExp
hwrong = parser <?> "Haskell wrong" where
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
mexp' nested = try (wrap madd)
  <|> try (wrap mfix)
  <|> try (wrap mfunabs)
  <|> try (wrap mh)
  <|> try (wrap mif0)
  <|> try (wrap ms)
  <|> try (wrap msub)
  <|> try (wrap mtyabs)
  <|> try (wrap mwrong)
  <|> try (wrap mfunapp)
  <|> try (wrap mtyapp)
  <|> try mnum
  <|> try mvar where
  wrap parser = if nested then wrapped else parser where
    wrapped = do
      char '('
      spaces
      t <- parser
      spaces
      char ')'
      return t

madd :: Parser MExp
madd = parser <?> "ML addition" where
  parser = do
    char '+'
    spaces
    e1 <- mexp' True
    many1 space
    e2 <- mexp' True
    return $ MAdd e1 e2

mfix :: Parser MExp
mfix = parser <?> "ML fixed-point operation" where
  parser = do
    string "fix"
    many1 space
    e <- mexp' True
    return $ MFix e

mfunabs :: Parser MExp
mfunabs = parser <?> "ML function abstraction" where
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
mfunapp = parser <?> "ML function application" where
  parser = do
    e1 <- mexp' True
    many1 space
    e2 <- mexp' True
    return $ MFunApp e1 e2

mif0 :: Parser MExp
mif0 = parser <?> "ML condition" where
  parser = do
    string "if0"
    many1 space
    c <- mexp' True
    many1 space
    t <- mexp' True
    many1 space
    f <- mexp' True
    return $ MIf0 c t f

mh :: Parser MExp
mh = parser <?> "ML Haskell guard" where
  parser = do
    string "MH"
    many1 space
    t <- stype
    many1 space
    e <- hexp' True
    return $ MH t e

mnum :: Parser MExp
mnum = parser <?> "ML natural number" where
  parser = do
    n <- many1 digit
    return $ MNum (read n)

ms :: Parser MExp
ms = parser <?> "ML Scheme guard" where
  parser = do
    string "MS"
    many1 space
    t <- stype
    many1 space
    e <- sexp' True
    return $ MS t e

msub :: Parser MExp
msub = parser <?> "ML subtraction" where
  parser = do
    char '-'
    spaces
    e1 <- mexp' True
    many1 space
    e2 <- mexp' True
    return $ MSub e1 e2

mtyabs :: Parser MExp
mtyabs = parser <?> "ML type abstraction" where
  parser = do
    string "\\\\"
    spaces
    v <- tvar
    spaces
    char '.'
    e <- mexp' True
    return $ MTyAbs v e

mtyapp :: Parser MExp
mtyapp = parser <?> "ML type application" where
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
mvar = parser <?> "ML variable" where
  parser = do
    v <- evar
    when (v `elem` ["fix", "if0", "wrong"]) (fail $ "Invalid variable: " ++ v)
    return $ MVar v

mwrong :: Parser MExp
mwrong = parser <?> "ML wrong" where
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
sexp' nested = try (wrap sadd)
  <|> try (wrap sfunabs)
  <|> try (wrap sfunpred)
  <|> try (wrap sh)
  <|> try (wrap sif0)
  <|> try (wrap sm)
  <|> try (wrap snumpred)
  <|> try (wrap ssub)
  <|> try (wrap swrong)
  <|> try (wrap sfunapp)
  <|> try snum
  <|> try svar where
  wrap parser = if nested then wrapped else parser where
    wrapped = do
      char '('
      spaces
      t <- parser
      spaces
      char ')'
      return t

sadd :: Parser SExp
sadd = parser <?> "Scheme addition" where
  parser = do
    char '+'
    spaces
    e1 <- sexp' True
    many1 space
    e2 <- sexp' True
    return $ SAdd e1 e2

sfunabs :: Parser SExp
sfunabs = parser <?> "Scheme function abstraction" where
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
sfunapp = parser <?> "Scheme function application" where
  parser = do
    e1 <- sexp' True
    many1 space
    e2 <- sexp' True
    return $ SFunApp e1 e2

sfunpred :: Parser SExp
sfunpred = parser <?> "Scheme function predicate" where
  parser = do
    string "fun?"
    many1 space
    e <- sexp
    return $ SFunPred e

sh :: Parser SExp
sh = parser <?> "Scheme Haskell guard" where
  parser = do
    string "SH"
    many1 space
    t <- stype
    many1 space
    e <- hexp' True
    return $ SH t e

sif0 :: Parser SExp
sif0 = parser <?> "Scheme condition" where
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
sm = parser <?> "Scheme ML guard" where
  parser = do
    string "SM"
    many1 space
    t <- stype
    many1 space
    e <- mexp' True
    return $ SM t e

snum :: Parser SExp
snum = parser <?> "Scheme natural number" where
  parser = do
    n <- many1 digit
    return $ SNum (read n)

snumpred :: Parser SExp
snumpred = parser <?> "Scheme natural number predicate" where
  parser = do
    string "num?"
    many1 space
    e <- sexp
    return $ SFunPred e

ssub :: Parser SExp
ssub = parser <?> "Scheme subtraction" where
  parser = do
    char '-'
    spaces
    e1 <- sexp' True
    many1 space
    e2 <- sexp' True
    return $ SSub e1 e2

svar :: Parser SExp
svar = parser <?> "Scheme variable" where
  parser = do
    v <- evar
    when (v `elem` ["if0", "wrong"]) (fail $ "Invalid variable: " ++ v)
    return $ SVar v

swrong :: Parser SExp
swrong = parser <?> "Scheme wrong" where
  parser = do
    string "wrong"
    many1 space
    char '\"'
    s <- many1 $ noneOf ['\"']
    char '\"'
    return $ SWrong s
