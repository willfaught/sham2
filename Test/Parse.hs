module Test.Parse (parseTests) where

import Parse
import Syntax
import Test.HUnit hiding (Label)
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Error as E

instance Eq P.ParseError where
  x == y = E.errorPos x == E.errorPos y && E.errorMessages x == E.errorMessages y

instance Eq E.Message where
  (==) = E.messageEq

parseTests :: Test
parseTests = "parse" ~: test [
    parseType{-,
    parseHaskell,
    parseML,
    parseScheme-}
  ]

-- Types

badType :: String -> String -> Test
badType n s = n ~: r ~? "" where
  r = case parseT s of
    Left _ -> True
    Right _ -> False

goodType :: String -> SType -> String -> Test
goodType n e s = n ~: Right e ~=? parseT s

parseType :: Test
parseType = "type" ~: test [
    parseForall,
    parseFun,
    parseLabel,
    parseLump,
    parseNat,
    parseTyVar
  ]

parseForall :: Test
parseForall = "forall" ~: test [
    goodType "no nested" (Forall "x" Nat) "A x . N",
    goodType "one nested" (Forall "x" (Forall "y" Nat)) "A x . (A y . N)",
    badType "bad A" "B x . N",
    badType "bad var" "A B . N",
    badType "bad dot" "A x , N",
    badType "bad body" "A x . B",
    badType "truncated body" "A x .",
    badType "truncated dot" "A x",
    badType "truncated var" "A"
  ]

parseFun :: Test
parseFun = "fun" ~: test [
    goodType "no nested" (Fun Nat Nat) "N -> N",
    goodType "left nested" (Fun (Fun Nat Nat) Nat) "(N -> N) -> N",
    goodType "right nested" (Fun Nat (Fun Nat Nat)) "N -> (N -> N)",
    goodType "both nested" (Fun (Fun Nat Nat) (Fun Nat Nat)) "(N -> N) -> (N -> N)",
    badType "bad arrow" "N --> N",
    badType "two arrows" "N ->-> N",
    badType "truncated right" "N ->"
  ]

parseLabel :: Test
parseLabel = "label" ~: test [
    goodType "zero nested" (Label Nat 0) "N ^ 0",
    goodType "one nested" (Label (Label Lump 0) 1) "(L ^ 0) ^ 1",
    badType "bad caret" "N * 0",
    badType "two carets" "N ^^ 0",
    badType "bad num" "N ^ N",
    badType "truncated num" "N ^"
  ]

parseLump :: Test
parseLump = "lump" ~: goodType "lump" Lump "L"

parseNat :: Test
parseNat = "nat" ~: goodType "nat" Nat "N"

parseTyVar :: Test
parseTyVar = "tyvar" ~: test [
    goodType "letter" (TyVar "x") "x",
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
    badType "letter, prime, letter" "x'y"
  ]

-- Haskell

badHExp :: String -> String -> Test
badHExp n s = n ~: r ~? "" where
  r = case parseH s of
    Left _ -> True
    Right _ -> False

goodHExp :: String -> HExp -> String -> Test
goodHExp n e s = n ~: Right e ~=? parseH s

parseHaskell :: Test
parseHaskell = "haskell" ~: test [
    parseHAdd{-,
    parseHFix,
    parseHFunAbs,
    parseHFunApp,
    parseHIf0,
    parseHM,
    parseHNum,
    parseHS,
    parseHSub,
    parseHTyAbs,
    parseHTyApp,
    parseHVar,
    parseHWrong-}
  ]

parseHAdd :: Test
parseHAdd = "hadd" ~: test [
    goodHExp "1, 1" (HAdd (HNum 1) (HNum 1)) "+ 1 1"
  ]

-- ML

parseML :: Test
parseML = undefined

-- Scheme

parseScheme :: Test
parseScheme = undefined
