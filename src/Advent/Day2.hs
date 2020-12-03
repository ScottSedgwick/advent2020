{-# LANGUAGE DerivingStrategies #-}
module Advent.Day2
  ( Pwd
  , day2IsValid1
  , day2IsValid2
  , day2ValidCount
  , day2Parser
  )
 where

import Data.Algebra.Boolean ( xor )
import Safe ( headMay )
import Text.Parsec ( char, digit, letter, newline, space, many ) 
import Text.Parsec.String ( Parser )

data Pwd = Pwd
  { minC :: Int
  , maxC :: Int
  , ch :: Char
  , pwd :: String
  } deriving stock (Show)

day2Parser :: Parser [Pwd]
day2Parser = many pwdParser

pwdParser :: Parser Pwd
pwdParser = do
  minc <- many digit
  _ <- char '-'
  maxc <- many digit
  _ <- space
  c <- letter
  _ <- char ':'
  _ <- space
  p <- many letter
  _ <- newline
  pure $ Pwd 
    { minC = read minc :: Int
    , maxC = read maxc :: Int
    , ch = c
    , pwd = p
    }

day2ValidCount :: (Pwd -> Bool) -> [Pwd] -> Int
day2ValidCount f = length . filter f

day2IsValid1 :: Pwd -> Bool
day2IsValid1 p = l >= minC p && l <= maxC p
  where
    l = length (filter (== ch p) (pwd p))

day2IsValid2 :: Pwd -> Bool
day2IsValid2 p = a == c `xor` b == c
  where
    a = headMay (drop (minC p - 1) (pwd p))
    b = headMay (drop (maxC p - 1) (pwd p))
    c = Just (ch p)
