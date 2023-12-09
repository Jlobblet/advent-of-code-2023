module Main (main) where

import Lib (parseDocument, part1, part2)
import System.Environment (getEnv)

main :: IO ()
main = do
  -- read environment variable INPUT
  input <- getEnv "INPUT"
  -- parse input
  parsed <- parseDocument <$> readFile input
  print $ part1 parsed
  print $ part2 parsed
