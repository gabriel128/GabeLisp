module Parser (readExpr, symbols) where

import Control.Monad.Except
import LispVal
import Text.ParserCombinators.Parsec hiding (spaces)

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
  y <- many (letter <|> digit <|> symbol)
  let atom = x : y
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _ -> Atom atom

singleAtom :: Parser LispVal
singleAtom = do
  x <- letter <|> symbol
  y <- many (letter <|> digit <|> symbol)
  eof
  let atom = x : y
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _ -> Atom atom

parseParensList :: Parser LispVal
parseParensList = do
  char '('
  x <- try parseList
  char ')'
  return x

parseCond :: Parser LispVal
parseCond = do
  string "if"
  spaces
  p <- parseAtom <|> parseParensList
  spaces
  conseq <- (parseSimpleExpr <|> parseParensList)
  spaces
  alt <- (parseSimpleExpr <|> parseParensList)
  return $ Cond p conseq alt

parseNumber :: Parser LispVal
parseNumber = fmap (Number . read) (many1 digit)

parseList :: Parser LispVal
parseList = fmap List (sepBy parseExpr spaces)

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseSimpleExpr :: Parser LispVal
parseSimpleExpr = parseAtom
  <|> parseNumber
  <|> parseString
  <|> parseQuoted

parseExpr :: Parser LispVal
parseExpr = parseSimpleExpr
  <|> do char '('
         x <- try (parseCond <|> parseList)
         char ')'
         return x

initParser :: Parser LispVal
initParser = singleAtom <|> parseExpr

checkBalanceParens :: String -> Either LispError String
checkBalanceParens expr =
  let lParens = filter (=='(') expr
      rParens = filter (== ')') expr
  in if length lParens == length rParens
  then Right expr
  else Left UnbalancedParens

readExpr :: String -> ThrowsError LispVal
readExpr input = do
  expr <- checkBalanceParens input
  case parse initParser "lisp" expr of
    Left err -> throwError $ Parser err
    Right val -> return val
