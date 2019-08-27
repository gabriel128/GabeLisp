{-# LANGUAGE InstanceSigs #-}
module Eval where

import LispVal
import Parser
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom f : args)) = mapM eval args >>= apply f
eval badform = throwError $ BadSpecialForm "Bad special form" badform

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "Unrecognized primitive function" f) ($ args) (lookup f primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop f params = mapM unpackNum params >>= return . Number . foldl1 f

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                         if null parsed
                         then throwError $ TypeMismatch "number" (String n)
                         else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom [a]] = return $ Bool $ a `elem` symbols
isSymbol _ = return $ Bool False


--- Error Handling ---

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

instance Show LispError where
  show :: LispError -> String
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
    where
      unwordsList = unwords . map show
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show _ = "Unknown Error"


extractValue :: ThrowsError a -> a
extractValue (Right a) = a
extractValue (Left _) = undefined

-- trapError :: a -> String
-- trapError action = catchError action (return . show)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
