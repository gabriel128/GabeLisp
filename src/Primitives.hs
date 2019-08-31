module Primitives (primitives) where

import LispVal
import LispError
import Control.Monad.Except
import Parser

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("==", numBoolOp (==)),
              ("<=", numBoolOp (<=)),
              (">=", numBoolOp (>=)),
              ("!=", numBoolOp (/=)),
              (">", numBoolOp (>)),
              ("||", boolBoolOp (||)),
              ("&&", boolBoolOp (&&)),
              ("<", numBoolOp (<)),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

binOp :: (LispVal -> ThrowsError a) -> (b -> LispVal) -> (a -> a -> b) -> [LispVal] -> ThrowsError LispVal
binOp _ _ _ singleVal@[_] = throwError $ NumArgs 2 singleVal
binOp unpacker constructor f [a, b] = fmap constructor $ f <$> unpacker a <*> unpacker b
binOp _ _ _ _ = throwError $ NumArgs 2 []

numBoolOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolOp = binOp unpackNum Bool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop = binOp unpackNum Number

boolBoolOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolOp = binOp unpackBool Bool

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool a) = return a
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x: _)] = return x
car [badArg] = throwError $ TypeMismatch "not a list" badArg
car badArg = throwError $ NumArgs 1 badArg

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return (List xs)
cdr [badArg] = throwError $ TypeMismatch "not a list" badArg
cdr badArg = throwError $ NumArgs 1 badArg

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom [a]] = return $ Bool $ a `elem` symbols
isSymbol _ = return $ Bool False
