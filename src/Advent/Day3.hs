module Advent.Day3
  ( day3parser
  , day3pt1
  , day3pt2
  ) where

import Advent.ParseUtils
import Text.Megaparsec
import Text.Megaparsec.Char

day3parser :: Parser [[Int]]
day3parser = some p1

p1 :: Parser [Int]
p1 = do
  xs <- some p2
  _ <- eol
  pure xs

p2 :: Parser Int
p2 = do
  c <- char '#' <|> char '.'
  pure $ if c == '#' then 1 else 0

runSlope :: [[Int]] -> (Int, Int) -> Int
runSlope xs (dx, dy) = f 0 0
  where
    f x y = if y >= length xs 
            then 0
            else g x (xs !! y) + f (x + dx) (y + dy) 
    g x ys = ys !! (x `mod` length ys) 

day3pt1 :: [[Int]] -> Int
day3pt1 xs = runSlope xs (3, 1)

day3pt2 :: [[Int]] -> Int
day3pt2 xs = product $ map (runSlope xs) [(1,1), (3,1), (5,1), (7,1), (1,2)]
