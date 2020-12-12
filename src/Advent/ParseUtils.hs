module Advent.ParseUtils
  ( eol
  , parseFile
  , process
  , integer
  , integers
  , int
  , ints
  , intset
  ) where

import Control.Monad (void)
import qualified Data.IntSet as IS
import Text.Parsec
import Text.Parsec.String ( Parser )

parseFile :: Parser  a -> FilePath -> IO (Either ParseError a)
parseFile p fname = do
  input <- readFile fname
  return (parse p "" input)

process :: (a -> b) -> b -> Either ParseError a -> IO b
process f d exs =
  case exs of
    (Left e) -> print e >> pure d
    (Right xs) -> pure $ f xs

eol :: Parser ()
eol = void (char '\n') <|> eof

integer :: Parser Integer
integer = rd <$> many1 digit
  where rd = read :: String -> Integer

integers :: Parser [Integer]
integers = many f
  where
    f = do
      x <- integer
      eol
      pure x

int :: Parser Int
int = rd <$> many1 digit
  where rd = read :: String -> Int

ints :: Parser [Int]
ints = many f
  where
    f = do
      x <- int
      eol
      pure x

intset :: Parser IS.IntSet
intset = do
  xs <- ints
  pure (IS.fromList xs)