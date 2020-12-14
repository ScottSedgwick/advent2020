module Advent.Day14
  ( day14parser
  , day14pt1
  , day14pt2
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Int
import Data.Bits
import Data.List (foldl')
import Advent.ParseUtils
import Text.Megaparsec
import Text.Megaparsec.Char

data Program = Program 
  { mask :: [(Int, Char)]
  , cmds :: [(Int64, Int64)]
  } deriving stock Show

data Docker = Docker
  { locn :: Map Int64 Int64
  , progs :: [Program]
  } deriving stock Show

instance Semigroup Docker where
  (<>) a b = Docker
    { locn = locn a <> locn b
    , progs = progs a <> progs b
    }

instance Monoid Docker where
  mempty = Docker
    { locn = M.empty
    , progs = []
    }

day14parser :: Parser Docker
day14parser = do
  p <- many programParser
  pure $ Docker 
    { locn = M.empty
    , progs = p
    }

programParser :: Parser Program
programParser = do
  m <- maskParser
  c <- some cmdParser
  pure $ Program { mask = m, cmds = c }

maskParser :: Parser [(Int, Char)]
maskParser = do
  _ <- chunk "mask = "
  cs <- many alphaNumChar
  _ <- newline
  pure $ zip [0..] $ reverse cs

cmdParser :: Parser (Int64, Int64)
cmdParser = do
  _ <- chunk "mem["
  i <- some digitChar
  _ <- chunk "] = "
  v <- some digitChar
  _ <- newline
  pure (read i :: Int64, read v :: Int64)

day14pt1 :: Docker -> Int64
day14pt1 d = M.foldr (+) 0 $ execDocker1 (locn d) (progs d)

execDocker1 :: Map Int64 Int64 -> [Program] -> Map Int64 Int64
execDocker1 mem ps = foldl' (\m p -> execProgram1 m (mask p) (cmds p)) mem ps

execProgram1 :: Map Int64 Int64 -> [(Int, Char)] -> [(Int64, Int64)] -> Map Int64 Int64
execProgram1 mem msk cs = foldl' (\m c -> execCommand1 msk m c) mem cs

execCommand1 :: [(Int, Char)] -> Map Int64 Int64 -> (Int64, Int64) -> Map Int64 Int64
execCommand1 m d (i,v) = M.insert i (foldr f v m) d
  where
    f (j,'1') a = setBit a j
    f (j,'0') a = clearBit a j
    f _       a = a 

day14pt2 :: Docker -> Int64
day14pt2 d = M.foldr (+) 0 $ execPrograms2 (locn d) (progs d)

execPrograms2 :: Map Int64 Int64 -> [Program] -> Map Int64 Int64
execPrograms2 mem ps = foldl' execProgram2 mem ps

execProgram2 :: Map Int64 Int64 -> Program -> Map Int64 Int64
execProgram2 mem p = foldl' (execCommand2 (mask p)) mem (cmds p)

execCommand2 :: [(Int, Char)] -> Map Int64 Int64 -> (Int64, Int64) -> Map Int64 Int64
execCommand2 msk mem (addr, val) = foldl' (\m a -> M.insert a val m) mem (mkAddrs [addr] msk)

mkAddrs :: [Int64] -> [(Int,Char)] -> [Int64]
mkAddrs addrs cs = foldl' f addrs cs
  where
    f [] _       = []
    f xs (i,'1') = map (\x -> setBit x i) xs
    f xs (i,'X') = concatMap (\x -> [setBit x i, clearBit x i]) xs
    f xs _       = xs