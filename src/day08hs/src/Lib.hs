module Lib
  ( documentParser,
    parseDocument,
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

data Document = Document Network (C.CList Direction)

data State = State Document Element

-- Replicate the format of the problem statement, with the current element prepended
instance Show Document where
  show (Document (Network m) ds) =
    concat
      [ concatMap show ds,
        "\n\n",
        unlines (map showNode $ M.toList m)
      ]
    where
      showNode (Element k, Node (Element l) (Element r)) =
        k ++ " = (" ++ l ++ ", " ++ r ++ ")"

documentParser :: Parsec String () Document
documentParser = do
  ds <- directionParser
  spaces
  n <- networkParser
  return $ Document n ds
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

parseDocument :: String -> Document
parseDocument input = fromRight undefined (parse documentParser "" input)

part1 :: Document -> Int
part1 d = length $ unfoldr f initial
  where
    initial = State d (Element "AAA")
    f (State _ (Element "ZZZ")) = Nothing
    f (State (Document (Network n) ds) e) =
      Just (e', State (Document (Network n) (C.rotR ds)) e')
      where
        (Node l r) = n M.! e
        e' = case C.focus ds of
          Just L -> l
          Just R -> r
          Nothing -> undefined
