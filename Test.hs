module Test (runTests) where

import Check
import Context
import Parse
import Reduce
import Substitute
import Syntax
import Test.HUnit
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

parse :: Test
parse = "parse" ~: test [parseTypes{-, parseHaskell, parseML, parseScheme-}]

parseTypes = "types" ~: test [
  "lu1" ~: Right Lump ~=? parseT "L",
  "na1" ~: Right Nat ~=? parseT "N",
  "va1" ~: Right (TyVar "x") ~=? parseT "x",
  "va2" ~: Right (TyVar "xy") ~=? parseT "xy",
  "va3" ~: Right (TyVar "x2") ~=? parseT "x2",
  "va4" ~: Right (TyVar "xy2") ~=? parseT "xy2",
  "va5" ~: Right (TyVar "xy2z") ~=? parseT "xy2z",
  "va6" ~: Right (TyVar "x'") ~=? parseT "x'",
  "va7" ~: Right (TyVar "x''") ~=? parseT "x''",
  "va8" ~: Right (TyVar "xy'") ~=? parseT "xy'",
  "va9" ~: Right (TyVar "xy'") ~=? parseT "xy'",
  "va10" ~: let p = case parseT "N" of Left _ -> True ; Right _ -> False in p ~? ""
  ]

parseHaskell = undefined

parseML = undefined

parseScheme = undefined

-- Reduce



-- Substitute



--Syntax


