module Eval (eval) where

import LispVal
import LispError
import Primitives
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (Cond pred conseq alt) = do
  p <- eval pred
  case p of
    Bool True -> eval conseq
    _ -> eval alt
eval (List (Atom f : args)) = mapM eval args >>= apply f
eval badform = throwError $ BadSpecialForm "Bad special form" badform

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args =
  -- maybe (throwError $ NotFunction "Unrecognized primitive function" f) ($ args) (lookup f primitives)
  case lookup f primitives of
    Just function -> function args
    Nothing -> throwError $ NotFunction "Unrecognized primitive unction" f
