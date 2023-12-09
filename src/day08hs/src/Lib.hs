module Lib
  ( stateParser,
    parseState,
    part1,
  )
where

import qualified Data.CircularList as C
import Data.Either (fromRight)
import Data.List (unfoldr)
import qualified Data.Map.Strict as M
import Text.Parsec (Parsec, count, many1, parse, sepEndBy, (<|>))
import Text.Parsec.Char (char, newline, spaces, string, upper)

newtype Element = Element String deriving (Show, Eq, Ord)

data Node = Node Element Element deriving (Show, Eq, Ord)

newtype Network = Network (M.Map Element Node) deriving (Show)

data Direction = L | R deriving (Show, Eq)

data State = State
  { network :: Network,
    current :: Element,
    directions :: C.CList Direction
  }

-- Replicate the format of the problem statement, with the current element prepended
instance Show State where
  show (State (Network m) e ds) =
    concat
      [ "Current: ",
        show e,
        "\n\n",
        concatMap show ds,
        "\n\n",
        unlines (map showNode $ M.toList m)
      ]
    where
      showNode (Element k, Node (Element l) (Element r)) =
        k ++ " = (" ++ l ++ ", " ++ r ++ ")"

stateParser :: Parsec String () State
stateParser = do
  ds <- directionParser
  spaces
  n <- networkParser
  return $ State n (Element "AAA") ds
  where
    directionParser = C.fromList <$> many1 ((L <$ char 'L') <|> (R <$ char 'R'))
    networkParser = Network . M.fromList <$> (nodeParser `sepEndBy` newline)
    nodeParser = do
      source <- elementParser
      _ <- string " = ("
      left <- elementParser
      _ <- string ", "
      right <- elementParser
      _ <- string ")"
      return (source, Node left right)
    elementParser = Element <$> count 3 upper

parseState :: String -> State
parseState input = fromRight undefined (parse stateParser "" input)

part1 :: State -> Int
part1 = length . unfoldr f
  where
    f (State _ (Element "ZZZ") _) = Nothing
    f (State (Network n) e ds) =
      Just (e', State (Network n) e' (C.rotR ds))
      where
        (Node l r) = n M.! e
        e' = case C.focus ds of
          Just L -> l
          Just R -> r
          Nothing -> undefined
