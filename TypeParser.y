{
module TypeParser2 (parseT) where

import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (fromJust, isJust)
import Syntax

parseT :: String -> SType
parseT = stype . lexer
}

%name stype
%tokentype { Token }
%error { parseError }

%token
  'A' { TokenForall }
  'L' { TokenLump }
  'N' { TokenNumber }
  '.' { TokenPeriod }
  '^' { TokenCaret }
  '->' { TokenArrow }
  v { TokenVariable $$ }
  n { TokenLabel $$ }

%%

T : 'A' v '.' T { Forall $2 $4 }
  | 'L' { Lump }
  | 'N' { Nat }
  | v { TyVar $1 }
  | T '^' n { Label $1 $3 }
  | T '->' T { Fun $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token = TokenArrow
  | TokenCaret
  | TokenForall
  | TokenLabel Int
  | TokenLump
  | TokenNumber
  | TokenPeriod
  | TokenVariable String
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer cs | "->" `isPrefixOf` cs = TokenArrow : lexer (drop 2 cs)
lexer (c : cs) | isJust $ lookup c tokens = fromJust (lookup c tokens) : lexer cs
  where tokens = [('A', TokenForall), ('L', TokenLump), ('N', TokenNumber), ('.', TokenPeriod), ('^', TokenCaret)]
lexer s @ (c : cs)
  | isSpace c = lexer cs
  | isAlpha c = lexAlpha s
  | isDigit c = lexDigit s

lexAlpha (c : cs) = TokenVariable (c : both ++ primes) : lexer primes'
  where
    (both, both') = span (\x -> isAlpha x || isDigit x) cs
    (primes, primes') = span ((==) '\'') both'

lexDigit cs = (TokenLabel (read num)) : lexer rest
  where (num, rest) = span isDigit cs
}
