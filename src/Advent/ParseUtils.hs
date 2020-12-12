module Advent.ParseUtils 
  ( Parser
  , digit
  , eol
  , intline
  , letter
  , parseFile
  , parseMap
  , space
  ) where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.Void
import qualified Data.Map as M
import Text.Megaparsec

type Parser = Parsec Void String

parseFile :: Parser a -> a -> FilePath -> IO a
parseFile p d f = do
  xs <- readFile f 
  case parse p f xs of
    (Left e)   -> print e >> pure d
    (Right ys) -> pure ys

parseMap :: Parser a -> Parser (M.Map (Int, Int) a)
parseMap p = some (lineParser p) >>= pure . M.fromList . concat

lineParser :: Parser a -> Parser [((Int, Int), a)]
lineParser p = some (locnParser p) >>= \xs -> eol <|> eof >> pure xs

locnParser :: Parser a -> Parser ((Int, Int), a)
locnParser p = do
  c <- p
  pos <- getSourcePos
  pure ((unPos (sourceColumn pos), unPos (sourceLine pos)), c)

eol :: Parser ()
eol = chunk "\n" >> pure ()

intline :: Parser Int
intline = do
  x <- some digit                  -- consume the digits we actually want
  _ <- many (oneOf [' ', '\t'])    -- consume any trailing spaces
  _ <- eol <|> eof                 -- consume the end-of-line character, or end-of-file
  pure (read x :: Int)             -- return the number we want, converted to an Int

digit :: Parser Char
digit = satisfy isDigit <?> "digit" -- oneOf ['0'..'9'] <?> "digit"

letter :: Parser Char
letter = satisfy isAlpha <?> "letter"

space :: Parser Char
space = satisfy isSpace <?> "space"