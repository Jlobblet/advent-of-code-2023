module Main (main) where

import System.Environment (getEnv)
import Lib (parseState, part1)

main :: IO ()
main = do
  -- read environment variable INPUT
  input <- getEnv "INPUT"
  -- parse input
  parsed <- parseState <$> readFile input
  print $ part1 parsed
