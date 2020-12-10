{-# LANGUAGE DerivingStrategies #-}
module Advent.Day9
  ( day9pt1
  , day9pt2
  , day9parser
  ) where

import Advent.ParseUtils
import Data.List (find, inits, sort)
import Text.Parsec
import Text.Parsec.String ( Parser )

day9pt1 :: [Integer] -> Integer
day9pt1 xs = firstBad (take 25 xs) (drop 25 xs)

firstBad :: [Integer] -> [Integer] -> Integer
firstBad _ [] = -1
firstBad xs (y:ys) =
  if isGood y xs
    then firstBad (tail xs <> [y]) ys
    else y

isGood :: Integer -> [Integer] -> Bool
isGood c cs = any (\(a,b) -> a+b == c) (pairs cs)

pairs :: [Integer] -> [(Integer,Integer)]
pairs [] = []
pairs (d:ds) = zip (repeat d) ds <> pairs ds

day9pt2 :: Integer -> [Integer] -> Integer
day9pt2 x xs = head ys + last ys
  where
    ys = sort $ day9pt2' x xs

day9pt2' :: Integer -> [Integer] -> [Integer]
day9pt2' _ [] = []
day9pt2' x xs = 
  case f x (inits xs) of
    Nothing -> day9pt2' x (tail xs)
    (Just ys) -> ys
  
f :: Integer -> [[Integer]] -> Maybe [Integer]
f x xs = find (\y -> x == sum y) xs

day9parser :: Parser [Integer]
day9parser = many numberline

numberline :: Parser Integer
numberline = do
  x <- integer
  _ <- eol
  pure x
