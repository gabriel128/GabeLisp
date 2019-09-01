{-# LANGUAGE InstanceSigs #-}
module LispVal where

import Data.IORef
import Text.ParserCombinators.Parsec
import Control.Monad.Except

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Cond LispVal LispVal LispVal
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String],
                      body :: [LispVal],
                      closure :: Env }
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | DefaultError String
               | UnbalancedParens
               | NotMutableLanguage
               deriving Eq

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

instance Eq LispVal where
  (PrimitiveFunc _) == (PrimitiveFunc _) = False
  x == y = x == y

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
  show NotMutableLanguage = "This is not a mutable language mate"
  show _ = "Unknown Error"

instance Show LispVal where
   show (Atom x) = show x
   show (List vals) = "(" <> unwords (fmap show vals) <> ")"
   show (Number x) = show x
   show (String x) = "\"" <> x <> "\""
   show (Cond p x y) = "(if " <> show p <> " " <> show x <> " " <> show y <> ")"
   show (Bool True) = "#t"
   show (Bool False) = "#f"
   show (PrimitiveFunc _) = "<Primitive func>"
   show Func{params = args, body = body} = "(lambda (" <> unwords (map show args) <> ")"  <>  show body <> ")"

type Env = IORef [(String, IORef LispVal)]

makeFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc env params body = return $ Func (fmap show params) body env

nullEnv :: IO Env
nullEnv = newIORef []

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right a) = a
extractValue (Left _) = error "[Fatal Error]"

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
