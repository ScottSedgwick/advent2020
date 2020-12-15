module Advent.Day7
  ( day7pt1
  , day7pt2
  , day7parser
  ) where

import Control.Monad (void)
import Data.List (nub)
import Text.Megaparsec
import Text.Megaparsec.Char
import Advent.ParseUtils

data Bag = Bag
  { colour :: String
  , contents :: [(Int, String)]
  } deriving stock (Show, Eq)

day7parser :: Parser [Bag]
day7parser = many bagParser

day7pt1 :: [Bag] -> Int
day7pt1 xs = length $ nub $ findAll ["shiny gold"] xs

findAll :: [String] -> [Bag] -> [String]
findAll []     _  = []
findAll (c:cs) cm = xs <> findAll (cs <> xs) cm 
  where
    xs = find c cm 

find :: String -> [Bag] -> [String]
find fromColor colorMap = map colour cs 
  where
    cs = filter (hasColor fromColor) colorMap

hasColor :: String -> Bag -> Bool
hasColor c b = any (\(_,c') -> c == c') (contents b)

day7pt2 :: [Bag] -> Int
day7pt2 ls = countBags "shiny gold" ls - 1
  where
    countBags c xs = 1 + n
      where
        ys = filter (\x -> colour x == c) xs
        n = if null ys then 0 else sum (map (\(a,b) -> a * countBags b xs) (contents (head ys)))

bagParser :: Parser Bag
bagParser = do
  c1 <- some letter
  void $ space
  c2 <- some letter
  void $ chunk " bags contain "
  cs <- many cp1
  void $ optional (chunk "no other bags.")
  void $ eol
  pure $ Bag 
    { colour = c1 <> " " <> c2
    , contents = cs
    }
  where
    cp1 = do
      n <- some digit
      void $ space
      c1 <- some letter
      void $ space
      c2 <- some letter
      void $ chunk " bag"
      void $ optional (single 's')
      void $ chunk ", " <|> chunk "."
      pure (read n :: Int, c1 <> " " <> c2)
    
