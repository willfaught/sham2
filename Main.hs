module Main (evalH) where

import Check
import Context
import Data.Maybe (isJust)
import HaskellParser
import Reduce
import Syntax

evalH :: String -> Maybe HExp
evalH s = case parseH s of
  Left x -> Nothing
  Right x -> if isJust $ checkH empty x then Just $ reduceFullH x else Nothing
