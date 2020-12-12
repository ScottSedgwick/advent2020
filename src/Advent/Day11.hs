module Advent.Day11 
  ( day11parser
  , day11pt1
  , day11pt2
  , cycleOnce
  , countAdjacent
  ) where

import qualified Data.Map as M
import Text.Megaparsec
import Advent.MParseUtils

data Space = Empty
           | Occupied
           | Floor
           deriving stock Eq

instance Show Space where
  show Empty = "L"
  show Occupied = "#"
  show Floor = "."

toSpace :: Char -> Space
toSpace 'L' = Empty
toSpace '#' = Occupied
toSpace _   = Floor

spaceValue :: Space -> Int
spaceValue Occupied = 1
spaceValue _        = 0

spaceParser :: Parser Space
spaceParser = do
  c <- oneOf ['L', '#', '.'] 
  pure $ toSpace c

type Room = M.Map (Int, Int) Space

day11parser :: FilePath -> IO Room
day11parser f = parseFile (parseMap spaceParser) M.empty f

day11pt1 :: Room -> Int
day11pt1 r0 = countOccupied r
  where
    r = cycleUntilStable countAdjacent 4 r0

countOccupied :: Room -> Int
countOccupied = M.foldr (\a -> (+) (spaceValue a)) 0 

cycleUntilStable :: ((Int, Int) -> Room -> Int) -> Int -> Room -> Room
cycleUntilStable f n r | r == r' = r
                       | otherwise = cycleUntilStable f n r'
  where
    r' = cycleOnce f n r

cycleOnce :: ((Int, Int) -> Room -> Int) -> Int -> Room -> Room 
cycleOnce f n r = M.mapWithKey g r
  where
    g _     Floor    = Floor
    g (i,j) Empty    = if f (i,j) r == 0 then Occupied else Empty
    g (i,j) Occupied = if f (i,j) r >= n then Empty else Occupied

countAdjacent :: (Int, Int) -> Room -> Int
countAdjacent (i,j) r = sum $ map (isAdjacentOccupied r) [(i-1,j-1), (i,j-1), (i+1,j-1), (i-1,j), (i+1,j), (i-1,j+1), (i,j+1), (i+1,j+1)]

isAdjacentOccupied :: Room -> (Int, Int) -> Int
isAdjacentOccupied r k = 
  case M.lookup k r of
    Just Occupied -> 1
    _             -> 0

day11pt2 :: Room -> Int
day11pt2 r0 = countOccupied (cycleUntilStable countLine 5 r0)

countLine :: (Int, Int) -> Room -> Int
countLine (i,j) r = sum $ map (isLineOccupied r (i,j)) [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]

isLineOccupied :: Room -> (Int, Int) -> (Int, Int) -> Int
isLineOccupied r (i,j) (di,dj) = 
  case M.lookup (i+di, j+dj) r of
    Just Occupied -> 1
    Just Floor    -> isLineOccupied r (i+di, j+dj) (di,dj)
    _             -> 0