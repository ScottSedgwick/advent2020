{-# LANGUAGE DerivingStrategies #-}
module Advent.Day7
  ( day7pt1
  , day7pt2
  ) where

import Control.Monad (void)
import Data.List (isInfixOf, nub)
import Data.List.Split (splitOn)
import Text.Parsec
import Text.Parsec.String ( Parser )

day7pt1 :: String -> Int
day7pt1 xs = length $ nub $ findAll ["shiny gold"] zs
  where
    ys = map (splitOn " bags contain ") (lines xs)
    zs = map (\y -> (head y, head (tail y))) ys

findAll :: [String] -> [(String, String)] -> [String]
findAll []     _  = []
findAll (c:cs) cm = xs <> findAll (cs <> xs) cm 
  where
    xs = find c cm 

find :: String -> [(String, String)] -> [String]
find fromColor colorMap = map fst cs 
  where
    cs = filter (hasColor fromColor) colorMap

hasColor :: String -> (String, String) -> Bool
hasColor c (_, b) = c `isInfixOf` b

day7pt2 :: String -> Int
day7pt2 ls = 
  case parse (many bagParser) "" ls of
    (Left _)   -> -1
    (Right ys) -> countBags "shiny gold" ys - 1
  where
    countBags c xs = 1 + n
      where
        ys = filter (\x -> colour x == c) xs
        n = if null ys then 0 else sum (map (\(a,b) -> a * countBags b xs) (contents (head ys)))

data Bag = Bag
  { colour :: String
  , contents :: [(Int, String)]
  } deriving stock (Show, Eq)

bagParser :: Parser Bag
bagParser = do
  c1 <- many1 letter
  void $ space
  c2 <- many1 letter
  void $ string " bags contain "
  cs <- many contentParser
  void $ newline
  pure $ Bag 
    { colour = c1 <> " " <> c2
    , contents = cs
    }
  where
    contentParser = cp1 <|> cp2
    cp1 = do
      n <- many1 digit
      void $ space
      c1 <- many1 letter
      void $ space
      c2 <- many1 letter
      void $ string " bag"
      void $ string ", " <|> string "." <|> skipEnd
      pure (read n :: Int, c1 <> " " <> c2)
    cp2 = do
      void $ string "no other bags."
      pure (0, "")
    skipEnd = do
      void $ char 's'
      string ", " <|> string "."
    
