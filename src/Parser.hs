module Parser (parse, parseExpr, symbols) where

-- import Control.Monad
import LispVal
import Text.ParserCombinators.Parsec hiding (spaces)

-- import qualified ParserLib as P
-- import System.Environment

symbols :: String
symbols = "!#$%&|*+-/:<=>?@^_~"

symbol :: Parser Char
symbol = oneOf symbols


spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

-- An atom is a letter or symbol, followed by any number of letters, digits, or symbols:
parseAtom :: Parser LispVal
parseAtom = do
  x <- letter <|> symbol
  y <- many (letter  <|> digit <|> symbol)
  let atom = x : y
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = fmap (Number . read) (many1 digit)

parseList :: Parser LispVal
parseList = fmap List (sepBy parseExpr spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseNumber
  <|> parseString
  <|> parseQuoted
  <|> do char '('
         x <- try parseList <|> parseDottedList
         char ')'
         return x
