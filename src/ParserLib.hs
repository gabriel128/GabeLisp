module ParserLib where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing  -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  fmap f (Parser parserF) =
    Parser (fmap (first f) . parserF)

instance Applicative Parser where
  pure a = Parser (\input -> Just (a, input))
  p1 <*> p2 =
    Parser (\input ->
              case runParser p1 input of
                Nothing -> Nothing
                Just (f, rest) -> (runParser $ fmap f p2) rest
              )

instance Monad Parser where
  return = pure
  -- f :: a -> Parser b
  p1 >>= f = Parser (\input ->
                 case runParser p1 input of
                   Nothing -> Nothing
                   Just (a, rest) -> runParser (f a) rest
                 )

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser f
    where
      f input = runParser p1 input <|> runParser p2 input


-- Examples --

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser' :: Parser (Char, Char)
abParser' = do
  a <- char 'a'
  b <- char 'b'
  return (a, b)

intPair :: Parser [Integer]
intPair = f <$> posInt <*> char ' ' <*> posInt
  where
    f x _ y = [x, y]

-- Ex 4 --


intOrUppercase :: Parser ()
intOrUppercase = p1 <|> p2
  where
    p1 = void posInt
    p2 = void (satisfy isUpper)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)
