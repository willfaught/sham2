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
    parseType,
    parseHaskell{-,
    parseML,
    parseScheme-}
  ]

-- Type

badType :: String -> String -> Test
badType n s = n ~: r ~? "" where
  r = case parseT s of
    Left _ -> True
    Right _ -> False

goodType :: String -> SType -> String -> Test
goodType n e s = n ~: Right e ~=? parseT s

parseType :: Test
parseType = "type" ~: test [
    forall,
    fun,
    label,
    lump,
    nat,
    tyvar,
    empty
  ]

forall :: Test
forall = "forall" ~: test [
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

fun :: Test
fun = "fun" ~: test [
    goodType "no nested" (Fun Nat Nat) "N -> N",
    goodType "left nested" (Fun (Fun Nat Nat) Nat) "(N -> N) -> N",
    goodType "right nested" (Fun Nat (Fun Nat Nat)) "N -> (N -> N)",
    goodType "both nested" (Fun (Fun Nat Nat) (Fun Nat Nat)) "(N -> N) -> (N -> N)",
    badType "bad arrow" "N --> N",
    badType "two arrows" "N ->-> N",
    badType "truncated right" "N ->"
  ]

label :: Test
label = "label" ~: test [
    goodType "zero nested" (Label Nat 0) "N ^ 0",
    goodType "one nested" (Label (Label Lump 0) 1) "(L ^ 0) ^ 1",
    badType "bad caret" "N * 0",
    badType "two carets" "N ^^ 0",
    badType "bad num" "N ^ N",
    badType "truncated num" "N ^"
  ]

lump :: Test
lump = "lump" ~: goodType "lump" Lump "L"

nat :: Test
nat = "nat" ~: goodType "nat" Nat "N"

tyvar :: Test
tyvar = "tyvar" ~: test [
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

empty :: Test
empty = badType "empty" ""

-- Haskell

badH :: String -> String -> Test
badH n s = n ~: r ~? "" where
  r = case parseH s of
    Left _ -> True
    Right _ -> False

goodH :: String -> HExp -> String -> Test
goodH n e s = n ~: Right e ~=? parseH s

parseHaskell :: Test
parseHaskell = "haskell" ~: test [
    hadd,
    hempty,
    hfix,
    hfunabs,
    hfunapp,
    hif0,
    hm,
    hnum,
    hs,
    hsub,
    htyabs,
    htyapp,
    hvar,
    hwrong
  ]

h1 = HNum 1

h2 = HNum 2

h3 = HNum 3

h4 = HNum 4

hid = HFunAbs "x" Nat (HVar "x")

hadd :: Test
hadd = "hadd" ~: test [
    goodH "none nested" (HAdd h1 h2) "+ 1 2",
    goodH "left nested" (HAdd (HAdd h1 h2) h3) "+ (+ 1 2) 3",
    goodH "right nested" (HAdd h1 (HAdd h2 h3)) "+ 1 (+ 2 3)",
    goodH "both nested" (HAdd (HAdd h1 h2) (HAdd h3 h4)) "+ (+ 1 2) (+ 3 4)",
    badH "missing right" "+ 1",
    badH "missing both" "+",
    badH "wrong plus" "* 1 2"
  ]

hempty :: Test
hempty = badH "hempty" ""

hfix :: Test
hfix = "hfix" ~: test [
    goodH "fun" (HFix hid) "fix (\\ x : N . x)",
    goodH "nested, fun" (HFix (HFix hid)) "fix (fix (\\ x : N . x))",
    badH "missing operand" "fix"
  ]

hfunabs :: Test
hfunabs = "hfunabs" ~: test [
    goodH "id" hid "\\ x : N . x",
    goodH "nested" (HFunAbs "x" Nat (HFunAbs "y" (Fun Lump Nat) (HVar "x"))) "\\ x : N . (\\ y : L -> N . x)",
    badH "bad period" "\\ x : N , x",
    badH "bad colon" "\\ x ; N . x",
    badH "bad backslash" "/ x : N . x",
    badH "missing body" "\\ x : N .",
    badH "missing period and body" "\\ x : N",
    badH "missing colon, period, and body" "\\ x",
    badH "missing param, colon, period, and body" "\\"
  ]

hfunapp :: Test
hfunapp = "hfunapp" ~: test [
    goodH "basic" (HFunApp hid hid) "(\\ x : N . x) (\\ x : N . x)",
    goodH "nested left" (HFunApp (HFunApp hid hid) hid) "((\\ x : N . x) (\\ x : N . x)) (\\ x : N . x)",
    goodH "nested right"(HFunApp hid (HFunApp hid hid)) "(\\ x : N . x) ((\\ x : N . x) (\\ x : N . x))",
    goodH "nested both" (HFunApp (HFunApp hid hid) (HFunApp hid hid)) "((\\ x : N . x) (\\ x : N . x)) ((\\ x : N . x) (\\ x : N . x))",
    badH "missing one" "(\\ x : N . x) "
  ]

hif0 :: Test
hif0 = "hif0" ~: test [
    goodH "basic" (HIf0 h1 h2 h3) "if0 1 2 3",
    goodH "nested test" (HIf0 (HIf0 h1 h2 h3) h2 h3) "if0 (if0 1 2 3) 2 3",
    goodH "nested then" (HIf0 h1 (HIf0 h1 h2 h3) h3) "if0 1 (if0 1 2 3) 3",
    goodH "nested else" (HIf0 h1 h2 (HIf0 h1 h2 h3)) "if0 1 2 (if0 1 2 3)",
    goodH "nested all" (HIf0 (HIf0 h1 h2 h3) (HIf0 h1 h2 h3) (HIf0 h1 h2 h3)) "if0 (if0 1 2 3) (if0 1 2 3) (if0 1 2 3)",
    badH "missing else" "if0 1 2",
    badH "missing then, else" "if0 1",
    badH "missing test, then, else" "if0",
    badH "misspelled" "if1 1 2 3",
    badH "missing if0" "1 2 3"
  ]

hm :: Test
hm = "hm" ~: test [
    goodH "basic" (HM Nat (MNum 1)) "HM N 1",
    goodH "nested" (HM Nat (MH Nat h1)) "HM N (MH N 1)",
    badH "missing exp" "HM N",
    badH "missing type, exp" "HM",
    badH "misspelled" "HN N 1"
  ]

hnum :: Test
hnum = "hnum" ~: test [
    goodH "single digit" h1 "1",
    goodH "many digits" (HNum 123) "123",
    badH "digit and letter" "1a",
    badH "prime" "1'"
  ]

hs :: Test
hs = "hs" ~: test [
    goodH "basic" (HS Nat (SNum 1)) "HS N 1",
    goodH "nested" (HS Nat (SH Nat h1)) "HS N (SH N 1)",
    badH "missing exp" "HS N",
    badH "missign type" "HS",
    badH "misspelled" "HD N 1"
  ]

hsub :: Test
hsub = "hsub" ~: test [
    goodH "none nested" (HSub h1 h2) "- 1 2",
    goodH "left nested" (HSub (HSub h1 h2) h3) "- (- 1 2) 3",
    goodH "right nested" (HSub h1 (HSub h2 h3)) "- 1 (- 2 3)",
    goodH "both nested" (HSub (HSub h1 h2) (HSub h3 h4)) "- (- 1 2) (- 3 4)",
    badH "missing right" "- 1",
    badH "missing both" "-",
    badH "wrong plus" "* 1 2"
  ]

htyabs :: Test
htyabs = "htyabs" ~: test [
    goodH "basic" (HTyAbs "x" h1) "\\\\ x . 1",
    goodH "nested" (HTyAbs "x" (HTyAbs "y" h1)) "\\\\ x . (\\\\ y . 1)",
    badH "missing body" "\\\\ x .",
    badH "missing period, body" "\\\\ x",
    badH "missing var, period, body" "\\\\",
    badH "bad period" "\\\\ x , 1",
    badH "bad first backslash" "/\\ x . 1",
    badH "bad second backslash" "\\/ x . 1",
    badH "bad both backslashes" "// x . 1",
    badH "bad var" "\\\\ N . 1"
  ]

htyapp :: Test
htyapp = "htyapp" ~: test [
    goodH "basic" (HTyApp (HTyAbs "x" h1) Nat) "(\\\\ x . 1) { N }",
    goodH "nested" (HTyApp (HTyApp (HTyAbs "x" (HTyAbs "y" h1)) Nat) Lump) "((\\\\ x . (\\\\ y . 1)) { N }) { L }",
    badH "missing left curly bracket" "1 N }",
    badH "missing type" "1 {   }",
    badH "missing right curly bracket" "1 { N",
    badH "missing type, right curly bracket" "1 {",
    badH "missing exp" "{ N }",
    badH "bad left curly bracket" "1 [ N }",
    badH "bad type" "1 { 2 }",
    badH "bad right curly bracket" "1 { N ]",
    badH "bad exp" "N { N }"
  ]

hvar :: Test
hvar = "hvar" ~: test [
    goodH "letter" (HVar "x") "x",
    goodH "letters" (HVar "xy") "xy",
    goodH "letter, digit" (HVar "x2") "x2",
    goodH "letters, digit" (HVar "xy2") "xy2",
    goodH "letter, digit, letter" (HVar "x2y") "x2y",
    goodH "letter, prime" (HVar "x'") "x'",
    goodH "letter, primes" (HVar "x''") "x''",
    goodH "letters, prime" (HVar "xy'") "xy'",
    goodH "letters, primes" (HVar "xy''") "xy''",
    badH "digit, letter" "1x",
    badH "prime, letter" "'x",
    badH "letter, prime, letter" "x'y",
    badH "fix" "fix",
    badH "if0" "if0",
    badH "wrong" "wrong"
  ]

hwrong :: Test
hwrong = "hwrong" ~: test [
    goodH "basic" (HWrong Nat "test") "wrong N \"test\"",
    goodH "chars" (HWrong (Fun Nat Nat) "Foo Bar Baz ,.:;/?\\[]{}()-_=+`~!@#$%^&*1234567890") "wrong N -> N \"Foo Bar Baz ,.:;/?\\[]{}()-_=+`~!@#$%^&*1234567890\""
  ]

-- ML

parseML :: Test
parseML = undefined

-- Scheme

parseScheme :: Test
parseScheme = undefined
