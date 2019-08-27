module LispVal where

type Tail = LispVal
type Head = [LispVal]

data LispVal = Atom String
             | List [LispVal]
             | DottedList Head Tail
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
   show (Atom x) = show x
   show (List vals) = "(" <> unwords (fmap show vals) <> ")"
   show (DottedList h t) = "(" <> unwords (fmap show h) <> " . " <> show t <> ")"
   show (Number x) = show x
   show (String x) = "\"" <> x <> "\""
   show (Bool True) = "#t"
   show (Bool False) = "#f"
