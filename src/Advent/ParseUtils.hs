module Advent.ParseUtils 
  ( parseFile
  , process
  ) where

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