module LispVal where

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Cond LispVal LispVal LispVal
             | Bool Bool
             deriving Eq

instance Show LispVal where
   show (Atom x) = show x
   show (List vals) = "(" <> unwords (fmap show vals) <> ")"
   show (Number x) = show x
   show (String x) = "\"" <> x <> "\""
   show (Cond p x y) = "(if " <> show p <> " " <> show x <> " " <> show y <> ")"
   show (Bool True) = "#t"
   show (Bool False) = "#f"
