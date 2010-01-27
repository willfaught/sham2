module Test (runTests) where

import Check
import Context
import Parse
import Reduce
import Substitute
import Syntax
import Test.HUnit hiding (Label)
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Error as E

runTests :: IO Counts
runTests = runTestTT tests

tests :: Test
tests = test [{-check, context,i-} parse{-, reduce, substitute, syntax-}]

-- Check

check :: Test
check = undefined

-- Context



-- Parse

instance Eq P.ParseError where
  x == y = E.errorPos x == E.errorPos y && E.errorMessages x == E.errorMessages y

instance Eq E.Message where
  (==) = E.messageEq

badType :: String -> String -> Test
badType n s = n ~: r ~? "" where
  r = case parseT s of
    Left _ -> True
    Right _ -> False

goodType :: String -> SType -> String -> Test
goodType n e s = n ~: Right e ~=? parseT s

parse :: Test
parse = "parse" ~: test [parseTypes{-, parseHaskell, parseML, parseScheme-}]

parseTypes = "types" ~: test [parseLump, parseNat, parseTyVar]

parseLump = goodType "lump" Lump "L"

parseNat = goodType "nat" Nat "N"

parseTyVar = "tyvar" ~: test [goodType "letter" (TyVar "x") "x",
  goodType "letters" (TyVar "xy") "xy",
  goodType "letter, digit" (TyVar "x2") "x2",
  goodType "letters, digit" (TyVar "xy2") "xy2",
  goodType "letter, digit, letter" (TyVar "x2y") "x2y",
  goodType "letter, prime" (TyVar "x'") "x'",
  goodType "letter, primes" (TyVar "x''") "x''",
  goodType "letters, prime" (TyVar "xy'") "xy'",
  goodType "letters, primes" (TyVar "xy''") "xy''",
  badType "digit" "1",
  badType "digit, letter" "1x",
  badType "prime, letter" "'x",
  badType "letter, prime, letter" "x'y"]

parseLabel = "label" ~: test [goodType "zero, zero spaces" (Label Nat 0) "N^0",
  goodType "zero, space before" (Label Nat 0) "N ^0",
  goodType "zero, space after" (Label Nat 0) "N^ 0",
  goodType "zero, two spaces" (Label Nat 0) "N ^ 0"
           ]

parseHaskell = undefined

parseML = undefined

parseScheme = undefined

-- Reduce



-- Substitute



--Syntax


