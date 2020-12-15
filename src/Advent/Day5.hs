module Advent.Day5
  ( day5pt1
  , day5pt2
  , day5parser
  ) where

import Data.Char (digitToInt)
import Data.List (foldl', sort)
import Advent.ParseUtils
import Text.Megaparsec
import Text.Megaparsec.Char

data Seat = Seat
  { row :: Int
  , col :: Int
  } deriving stock (Eq, Show)

day5parser :: Parser [Int]
day5parser = some seatParser

seatParser :: Parser Int
seatParser = do
  rs <- some (char 'F' <|> char 'B')
  cs <- some (char 'R' <|> char 'L')
  _ <- eol
  let a = binToDec $ map (\c -> if c == 'B' then '1' else '0') rs
  let b = binToDec $ map (\c -> if c == 'R' then '1' else '0') cs
  pure $ a * 8 + b
  where
    binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

day5pt1 :: [Int] -> Int
day5pt1 xs = head $ reverse $ sort xs

day5pt2 :: [Int] -> Int
day5pt2 xs = findGap ys (tail ys)
  where
    ys = reverse $ sort xs

findGap :: [Int] -> [Int] -> Int
findGap (x:xs) (y:ys) = 
  if x - y == 1 
    then findGap xs ys 
    else x - 1
findGap _ _ = -1