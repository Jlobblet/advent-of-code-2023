module Lib
  ( Document,
    documentParser,
    parseDocument,
    part1,
    part2,
  )
where

import qualified Data.CircularList as C
import Data.Either (fromRight)
import Data.List (isSuffixOf, unfoldr)
import qualified Data.Map.Strict as M
import Text.Parsec (Parsec, count, many1, parse, sepEndBy, (<|>))
import Text.Parsec.Char (alphaNum, char, newline, spaces, string)

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
    elementParser = Element <$> count 3 alphaNum

parseDocument :: String -> Document
parseDocument input = fromRight undefined (parse documentParser "" input)

nSteps :: Element -> (Element -> Bool) -> Document -> Int
nSteps start isZ d = length $ unfoldr f $ State d start
  where
    f (State (Document (Network n) ds) e) =
      if isZ e
        then Nothing
        else Just (e', State (Document (Network n) (C.rotR ds)) e')
      where
        (Node l r) = n M.! e
        e' = case C.focus ds of
          Just L -> l
          Just R -> r
          Nothing -> undefined

part1 :: Document -> Int
part1 = nSteps (Element "AAA") (== Element "ZZZ")

part2 :: Document -> Int
part2 d@(Document (Network n) _) = k
  where
    isZ (Element e) = "Z" `isSuffixOf` e
    isA (Element e) = "A" `isSuffixOf` e
    k = foldr1 lcm $ fmap (\i -> nSteps i isZ d) $ filter isA $ M.keys n
