module Main where

import Check
import Parse
import Reduce
import Syntax
import System.Environment
import System.FilePath.Posix
import System.IO
import Text.ParserCombinators.Parsec as P

handler ext = case lookup ext pairs of
  Just x -> x

usage :: IO ()
usage = putStrLn "usage: sham [FILE.haskell | FILE.ml | FILE.scheme]"

run :: IO ()
run file = do
  let handle = openFile file ReadMode
  if hIsEOF handle
    then hPutStrLn stderr "sham: unexpected end of file"
    else return ()
  if not (hasExtension file)
    then hPutStrLn stderr "sham: file extension required to determine the language"
    else return ()
  let ext = takeExtension $ getArgs !! 0
  if not (ext `elem` [".haskell", ".ml", ".scheme"])
    then hPutStrLn stderr "sham: invalid file extension"
  let p = parse
  hClose stdout
  return ()

main :: IO ()
main = do
  if length getArgs > 1
    then hPutStrLn stderr "sham: invalid number of arguments"
    else if length getArgs == 0
      then usage
      else do
        let arg = getArgs !! 0
        if arg == "--help"
          then usage
          else run arg
