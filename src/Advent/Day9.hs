{-# LANGUAGE DerivingStrategies #-}
module Advent.Day9
  ( day9pt1
  , day9pt2
  , day9parser
  ) where

import Advent.ParseUtils
import Data.List (inits, tails)
import Text.Megaparsec

day9pt1 :: [Int] -> Int
day9pt1 xs = firstBad (take 25 xs) (drop 25 xs)

firstBad :: [Int] -> [Int] -> Int
firstBad _ [] = -1
firstBad xs (y:ys) =
  if isGood y xs
    then firstBad (tail xs <> [y]) ys
    else y

isGood :: Int -> [Int] -> Bool
isGood c cs = any (\(a,b) -> a+b == c) (pairs cs)

pairs :: [Int] -> [(Int,Int)]
pairs [] = []
pairs (d:ds) = zip (repeat d) ds <> pairs ds

day9pt2 :: Int -> [Int] -> Int
day9pt2 x xs = head ys + last ys
  where
    ys = head $ filter (\zs -> sum zs == x) $ concatMap tails $ inits xs

day9parser :: FilePath -> IO [Int]
day9parser f = parseFile (some intline) f

