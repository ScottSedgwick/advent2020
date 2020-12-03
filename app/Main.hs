module Main (main) where

import Advent
import Text.Parsec.String (Parser)

datafile :: FilePath
datafile = "data/Day2Actual.txt"
--datafile = "data/Day2Test.txt"

type T = [Pwd]

parser :: Parser T
parser = day2Parser

action :: T -> IO()
action = print . day2ValidCount day2IsValid1

main :: IO ()
main = do
  epwd <- parseFromFile parser datafile
  case epwd of
    Left e -> print $ "Error: " ++ show e
    Right ps -> action ps
