{-# LANGUAGE InstanceSigs #-}

module LispError where

import Text.ParserCombinators.Parsec
import LispVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | DefaultError String
               | UnbalancedParens
               deriving Eq

type ThrowsError = Either LispError

instance Show LispError where
  show :: LispError -> String
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show UnbalancedParens = "Unbalanced Parens"
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
    where
      unwordsList = unwords . map show
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show _ = "Unknown Error"
