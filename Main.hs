module Main where

import Check
import Context
import Parse
import Reduce
import Syntax
import System.Environment
import System.FilePath.Posix
import System.IO
import Text.ParserCombinators.Parsec as P

usage :: IO ()
usage = putStrLn "usage: sham [FILE.haskell | FILE.ml | FILE.scheme]"

main :: IO ()
main = do
  args <- getArgs
  let path = args !! 0
  let language = drop 1 . takeExtension $ path
  handle <- openFile path ReadMode
  if language == "haskell"
    then haskell handle
    else if language == "ml"
      then ml handle
      else scheme handle

haskell :: Handle -> IO ()
haskell handle = do
  contents <- hGetContents handle
  let Right exp = parseH contents
  let ty = checkH empty exp
  let Right reduction = reduceFully exp
  putStrLn . show $ reduction

ml = undefined

scheme = undefined
