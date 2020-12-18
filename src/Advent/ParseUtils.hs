module Advent.ParseUtils 
  ( Parser
  , digit
  , int
  , integer
  , intline
  , letter
  , lexeme
  , parens
  , parseFile
  , parseMap
  , parseString
  , symbol
  ) where

import Data.Char (isAlpha, isDigit)
import Data.Default (Default, def)
import Data.Functor ((<&>))
import Data.Void ( Void )
import qualified Data.Map as M
import Text.Megaparsec
    ( (<|>),
      Parsec,
      (<?>),
      getSourcePos,
      oneOf,
      parse,
      satisfy,
      unPos,
      between,
      many,
      some,
      MonadParsec(hidden, eof),
      SourcePos(sourceLine, sourceColumn) )
import Text.Megaparsec.Char ( digitChar, eol, space, string )

type Parser = Parsec Void String

parseFile :: Default a => Parser a -> FilePath -> IO a
parseFile p f = do
  xs <- readFile f 
  case parse p f xs of
    (Left e)   -> print e >> pure def
    (Right ys) -> pure ys

parseString :: Default a => Parser a -> String -> a
parseString p xs = 
  case parse p "" xs of
    (Left _)   -> def
    (Right ys) -> ys

parseMap :: Parser a -> Parser (M.Map (Int, Int) a)
parseMap p = some (lineParser p) <&> M.fromList . concat

lineParser :: Parser a -> Parser [((Int, Int), a)]
lineParser p = some (locnParser p) >>= \xs -> eolv <|> eof >> pure xs

locnParser :: Parser a -> Parser ((Int, Int), a)
locnParser p = do
  c <- p
  pos <- getSourcePos
  pure ((unPos (sourceLine pos), unPos (sourceColumn pos)), c)

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
digit = satisfy isDigit <?> "digit"

int :: Parser Int
int = do
  x <- some digit
  pure (read x :: Int)

lexeme :: Parser a -> Parser a
lexeme p = p <* hidden space

integer :: Parser Integer
integer = lexeme (read <$> some digitChar <?> "integer")

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

letter :: Parser Char
letter = satisfy isAlpha <?> "letter"