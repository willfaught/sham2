{
module HaskellParser2 (parseH) where

import Data.Maybe (fromJust)
import Syntax
}

%name parseH
%tokentype { Char }
%error { parseError }

%token
  '+' { TokenPlus }
  'fix' { TokenFix }
  '\' { TokenBackSlash }
  ':' { TokenColon }
  '.' { TokenPeriod }
  'if0' { TokenIf0 }
  n { TokenNumber $$ }
  '-' { TokenMinus }
  v { TokenVariable $$ }
  '{' { TokenCurlyBracketLeft }
  '}' { TokenCurlyBracketRight }
  'wrong' { TokenWrong }
  '"' { TokenQuote }
  '(' { TokenParenLeft }
  ')' { TokenParenRight }
  'N' { TokenType }
  string { TokenString $$ }

%%

AppMaybe : AppMaybe E { HApp $1 $2 }
         | E { $1 }

E : '+' E E { HAdd $2 $3 }
  | 'fix' E { HFIx $2 }
  | '\' v ':' 'N' '.' E { HFunAbs $2 Nat $6 }
  | v { HVar $1 }
  | 'if0' E E E { HIf0 $2 $3 $4 }
  | n { HNum $1 }
  | '-' E E { HSub $2 $3 }
  | '\' '\' v '.' E { HTyAbs $2 $4 }
  | E '{' 'N' '}' { HTyApp $1 Nat }
  | 'wrong' 'N' '"' string '"' { HWrong Nat $4 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token = TokenBackSlash
  | TokenColon
  | TokenCurlyBracketLeft
  | TokenCurlyBracketRight
  | TokenFix
  | TokenIf0
  | TokenMinus
  | TokenNumber Int
  | TokenParenLeft
  | TokenParenRight
  | TokenPeriod
  | TokenPlus
  | TokenQuote
  | TokenString String
  | TokenType
  | TokenVariable String
  | TokenWrong
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer s @ (c : cs)
  | isSpace c = lexer cs
  | isAlpha c = lexAlpha s
  | isDigit c = lexDigit s
lexer (c : cs) = fromJust . lookup c tokens : lexer cs
  where tokens =
    [('\', TokenBackSlash),
     (':', TokenColon),
     ('{', TokenCurlyBracketLeft),
     ('}', TokenCurlyBracketRight),
     ('(', TokenParenLeft),
     (')', TokenParenRight),
     ('.', TokenPeriod),
     ('+', TokenPlus),
     ('"', TokenQuote)]

lexAlpha cs = case match of
  "fix" -> TokenFix : lexer primesrest
  "if0" -> TokenIf0 : lexer primesrest
  "wrong" -> TokenWrong : lexer primesrest
  _ -> TokenVariable match : lexer primesrest
  where
    (both, bothrest) = span (isAlpha || isDigit) cs
    (primes, primesrest) = span ((==) '\'') bothrest
    match = c : both : primes

lexDigit cs = TokenNumber (read num) : lexer rest
  where (num, rest) = span isDigit cs
}
