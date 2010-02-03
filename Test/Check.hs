module Test.Check (checkTests) where

import Check
import Context
import Parse
import Syntax
import Test.HUnit hiding (Label)

checkTests :: Test
checkTests = "check" ~: test [
    checkType,
    checkHaskell,
    checkML,
    checkScheme
  ]

-- Type

goodT :: String -> String -> Test
goodT = goodT' empty

goodT' :: Context -> String -> String -> Test
goodT' c n s = n ~: case parseT s of
  Right x -> test . assert $ checkT c x
  Left x -> test . assertFailure $ show x

badT :: String -> String -> Test
badT = badT' empty

badT' :: Context -> String -> String -> Test
badT' c n s = n ~: case parseT s of
  Right x -> test . assert . not $ checkT c x
  Left x -> test . assertFailure $ show x

checkType :: Test
checkType = "type" ~: test [
    checkForall,
    checkFun,
    checkLabel,
    checkLump,
    checkNat,
    checkTyVar
  ]

checkForall :: Test
checkForall = "forall" ~: test [
    goodT "forall, no var" "A x . N",
    goodT "forall, good var" "A x . x",
    goodT "forall, good nested var" "A x . (A x . x)",
    goodT "forall, good outer var" "A x . (A y . x)",
    goodT "forall, good inner var" "A x . (A y . y)",
    badT "forall, bad var" "A x . y"
  ]

checkFun :: Test
checkFun = "fun" ~: test [
    goodT "fun, no vars" "N -> N",
    goodT' (stbind "x" empty) "fun, good var" "x -> x",
    goodT' (stbind "y" (stbind "x" empty)) "fun, good vars" "x -> y",
    badT "fun, left var bad" "x -> N",
    badT "fun, right var bad" "N -> x",
    badT "fun, both vars bad" "x -> x"
  ]

checkLabel :: Test
checkLabel = "label" ~: test [
    goodT "label, nat" "N ^ 0",
    goodT' (stbind "x" empty) "label, good var" "x ^ 0",
    goodT' (stbind "x" empty) "label, good nested var" "(x ^ 0) ^ 1",
    badT' (stbind "x" empty) "label, bad var" "y ^ 0"
  ]

checkLump :: Test
checkLump = "lump" ~: goodT "lump" "L"

checkNat :: Test
checkNat = "nat" ~: goodT "nat" "N"

checkTyVar :: Test
checkTyVar = "tyvar" ~: test [
    goodT' (stbind "x" empty) "tyvar, good" "x",
    goodT' (stbind "x" (stbind "y" empty)) "tyvar, good, first" "x",
    goodT' (stbind "x" (stbind "y" empty)) "tyvar, good, second" "y",
    badT "tyvar, bad" "x",
    badT' (stbind "x" empty) "tyvar, bad, one bound" "y",
    badT' (stbind "x" (stbind "y" empty)) "tyvar, bad, two bound" "z"
  ]

-- Haskell

goodH :: String -> SType -> String -> Test
goodH = goodH' empty

goodH' :: Context -> String -> SType -> String -> Test
goodH' c n e s = case parseH s of
  Right x -> case checkH c x of
    Just y -> n ~: e ~=? y
    Nothing -> test . assertFailure $ "Ill-typed expression: " ++ show x
  Left x -> test . assertFailure $ show x

checkHaskell :: Test
checkHaskell = "haskell" ~: test [
    checkHAdd,
    checkHFix,
    checkHFunAbs,
    checkHFunApp{-,
    checkHIf0...-}
  ]

checkHAdd :: Test
checkHAdd = "hadd" ~: test [
    assert True
  ]

checkHFix = "hfix" ~: test [
    assert True
  ]

checkHFunAbs = "hfunabs" ~: test [
    assert True
  ]

checkHFunApp = "hfunapp" ~: test [
    assert True
  ]

-- ML

checkML :: Test
checkML = undefined

-- Scheme

checkScheme :: Test
checkScheme = undefined
