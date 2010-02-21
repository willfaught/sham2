module Test.Reduce (reduceTests) where

import Check
import Context
import Parse
import Reduce
import Syntax
import Test.HUnit
import Text.ParserCombinators.Parsec

reduceTests :: Test
reduceTests = "reduce" ~: test [
    reduceHaskell,
    reduceML,
    reduceScheme
  ]

transform :: (Show a, Reduce a) => (String -> Either ParseError a) -> (Context -> a -> Maybe b) -> String -> IO a
transform parse check syntax = do
  expectedExp <- case parse syntax of
    Right exp -> return exp
    Left error -> fail . show $ error
  case check empty expectedExp of
    Just type_ -> return type_
    Nothing -> fail $ "Ill-typed expression: " ++ show expectedExp
  case reduce expectedExp of
    Right exp' -> return exp'
    Left message -> fail message

-- Haskell

transformH = transform parseH checkH

goodH :: String -> String -> String -> Test
goodH name expected actual = name ~: test assertion where
  assertion = do
    expected' <- case parseH expected of
      Right x -> return x
      Left error -> fail . show $ error
    actual' <- transformH actual
    expected' @=? actual'

reduceHaskell :: Test
reduceHaskell = "haskell" ~: test [
    hadd
  ]

hadd :: Test
hadd = "hadd" ~: test [
    goodH "full" "2" "+ 1 1",
    goodH "left" "+ 3 3" "+ (+ 1 2) 3",
    goodH "right" "+ 1 5" "+ 1 (+ 2 3)"
  ]

hfix :: Test
hfix = "hfix" ~: test [
    goodH "inner" "fix ((\\ x : N . (\\ y : N . y)) 0)" "fix (\\ x : N . (\\ y : N . y))",
    goodH "outer" "fix (\\x:N.x)" "fix (\\x:N.x)"
  ]

-- ML

reduceML = undefined

-- Scheme

reduceScheme = undefined
