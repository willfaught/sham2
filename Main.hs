module Main where

import Check
import Context
import Parse
import Reduce hiding (reduce)
import System.Environment
import System.IO
import qualified Text.ParserCombinators.Parsec as P

main :: IO ()
main = do
  arguments <- getArgs
  case length arguments of
    0 -> usage
    1 -> language
    _ -> fail "too many arguments"

usage :: IO ()
usage = putStrLn "usage: sham haskell | sham ml | sham scheme"

language :: IO ()
language = do
  args <- getArgs
  let arg = args !! 0
  case lookup arg [("haskell", haskell), ("ml", ml), ("scheme", scheme)] of
    Just x -> x
    Nothing -> fail "invalid language"

haskell :: IO ()
haskell = do
  contents <- getContents
  expression <- parse hexp contents
  check checkH expression
  reduction <- reduce expression
  putStrLn $ show reduction where

ml :: IO ()
ml = do
  contents <- getContents
  expression <- parse mexp contents
  check checkM expression
  reduction <- reduce expression
  putStrLn $ show reduction where

scheme :: IO ()
scheme = do
  contents <- getContents
  expression <- parse sexp contents
  check checkS expression
  reduction <- reduce expression
  putStrLn $ show reduction where

parse :: P.Parser a -> String -> IO a
parse p s = case P.parse p "standard input" s of
  Right x -> return x
  Left x -> fail $ "ill-formed expression: " ++ show x

check :: Show a => (Context -> a -> Maybe b) -> a -> IO b
check c e = case c empty e of
  Just x -> return x
  Nothing -> fail $ "ill-typed expression: " ++ show e

reduce :: Reduce a => a -> IO a
reduce e = case reduceFully e of
  Right x -> return x
  Left x -> fail $ "reduction error: " ++ x
