module Advent.Day17
  ( day17pt1,
    day17pt2,
    day17parser,
  )
where

import Advent.ParseUtils (Parser, parseMap)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char (char)

type CSpace a = M.Map a Int

type Point2D = (Int, Int)

type Point3D = (Int, Int, Int)

ptArea3 :: [Point3D] -> [Point3D]
ptArea3 ps = [(x, y, z) | x <- [minimum xs -1 .. maximum xs + 1], y <- [minimum ys -1 .. maximum ys + 1], z <- [minimum zs -1 .. maximum zs + 1]]
  where
    xs = map (\(x, _, _) -> x) ps
    ys = map (\(_, y, _) -> y) ps
    zs = map (\(_, _, z) -> z) ps

type Point4D = (Int, Int, Int, Int)

ptArea4 :: [Point4D] -> [Point4D]
ptArea4 ps = [(x, y, z, w) | x <- [minimum xs -1 .. maximum xs + 1], y <- [minimum ys -1 .. maximum ys + 1], z <- [minimum zs -1 .. maximum zs + 1], w <- [minimum ws -1 .. maximum ws + 1]]
  where
    xs = map (\(x, _, _, _) -> x) ps
    ys = map (\(_, y, _, _) -> y) ps
    zs = map (\(_, _, z, _) -> z) ps
    ws = map (\(_, _, _, w) -> w) ps

day17parser :: Parser (CSpace Point2D)
day17parser = parseMap hashParser

hashParser :: Parser Int
hashParser = do
  c <- char '#' <|> char '.'
  pure $ if c == '#' then 1 else 0

day17pt1 :: CSpace Point2D -> Int
day17pt1 = conway ptArea3 . mapSpace (\(x, y) -> (x, y, 0))

day17pt2 :: CSpace Point2D -> Int
day17pt2 = conway ptArea4 . mapSpace (\(x, y) -> (x, y, 0, 0))

mapSpace :: Ord k => (Point2D -> k) -> CSpace Point2D -> CSpace k
mapSpace f = M.foldrWithKey (M.insert . f) M.empty

conway :: Ord k => ([k] -> [k]) -> CSpace k -> Int
conway ptgen = M.foldr (+) 0 . congen 6 ptgen

congen :: Ord k => Int -> ([k] -> [k]) -> CSpace k -> CSpace k
congen 0 _ cs = cs
congen n f cs = congen (n - 1) f $ foldr (csgen f cs) M.empty (f $ M.keys cs)

csgen :: Ord k => ([k] -> [k]) -> CSpace k -> k -> CSpace k -> CSpace k
csgen f cs p = M.insert p v'
  where
    v = fromMaybe 0 (M.lookup p cs)
    v'
      | v == 1 = if ca == 3 || ca == 4 then 1 else 0
      | ca == 3 = 1
      | otherwise = 0
    ca = sumSpace p f cs

sumSpace :: Ord k => k -> ([k] -> [k]) -> CSpace k -> Int
sumSpace p f cs = sum (mapMaybe (`M.lookup` cs) (f [p]))
