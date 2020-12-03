module Advent.ParseUtils 
  ( parseFromFile
  ) where

import Text.Parsec

parseFromFile :: Parsec String () a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname = do
  input <- readFile fname
  return (runParser p () fname input)