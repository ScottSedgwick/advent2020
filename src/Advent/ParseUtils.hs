module Advent.ParseUtils 
  ( Parser
  , digit
  , int
  , integer
  , intline
  , letter
  , parseFile
  , parseMap
  ) where

import Data.Char (isAlpha, isDigit)
import Data.Void
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseFile :: Monoid a => Parser a -> FilePath -> IO a
parseFile p f = do
  xs <- readFile f 
  case parse p f xs of
    (Left e)   -> print e >> pure mempty
    (Right ys) -> pure ys

parseMap :: Parser a -> Parser (M.Map (Int, Int) a)
parseMap p = some (lineParser p) >>= pure . M.fromList . concat

lineParser :: Parser a -> Parser [((Int, Int), a)]
lineParser p = some (locnParser p) >>= \xs -> eolv <|> eof >> pure xs

locnParser :: Parser a -> Parser ((Int, Int), a)
locnParser p = do
  c <- p
  pos <- getSourcePos
  pure ((unPos (sourceColumn pos), unPos (sourceLine pos)), c)

intline :: Parser Int
intline = do
  x <- some digit                  -- consume the digits we actually want
  _ <- many (oneOf [' ', '\t'])    -- consume any trailing spaces
  _ <- eolv <|> eof                -- consume the end-of-line character, or end-of-file
  pure (read x :: Int)             -- return the number we want, converted to an Int

eolv :: Parser ()
eolv = do
  _ <- eol
  pure ()

digit :: Parser Char
digit = satisfy isDigit <?> "digit" -- oneOf ['0'..'9'] <?> "digit"

int :: Parser Int
int = do
  x <- some digit
  pure (read x :: Int)

integer :: Parser Integer
integer = do
  x <- some digit
  pure (read x :: Integer)

letter :: Parser Char
letter = satisfy isAlpha <?> "letter"