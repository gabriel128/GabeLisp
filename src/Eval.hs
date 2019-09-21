module Eval (eval, setVar, bindVars) where

import Data.IORef
import LispVal
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Parser

eval :: LispVal -> IOThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (Atom id) = do
  env <- ask
  getVar env id
eval (List [Atom "quote", val]) = return val
eval (List (Atom "defo" : List (Atom var : params) : body)) = do
  env <- ask
  makeFunc env params body >>= define env var
eval (List [Atom "defo", Atom var, val]) = do
  env <- ask
  eval val >>= define env var
eval (List (Atom "lambo" : List params : body)) = do
  env <- ask
  makeFunc env params body
eval (List [Atom "load", String filename]) = do
  a <- load filename
  eval a
eval (Cond pred conseq alt) = do
  p <- eval pred
  case p of
    Bool True -> eval conseq
    _ -> eval alt
eval (List (f : args)) = do
  func <- eval f
  argVals <- mapM eval args
  apply func argVals
eval badform = throwError $ BadSpecialForm "Bad special form" badform

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func params [body] closure) args
  | num params /= num args = throwError $ NumArgs (num params) args
  | otherwise = do
      env <- liftIO $ bindVars closure $ zip params args
      local (const env) evalBody
  where
    num = toInteger . length
    evalBody = eval body
apply _ _ = throwError $ DefaultError "Error applying function"

load :: String -> IOThrowsError LispVal
load filename = liftIO (readFile filename) >>= liftThrows . readExpr

--- ENV Handling ----

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Unbound variable"  var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  valueRef <- liftIO $ newIORef value
  env <- liftIO $ readIORef envRef
  liftIO $ writeIORef envRef ((var, valueRef) : env)
  return value

define :: Env -> String -> LispVal -> IOThrowsError LispVal
define envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
  then throwError NotMutableLanguage
  else setVar envRef var value >> return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
  env <- readIORef envRef
  env' <- extendEnv bindings env
  newIORef env'
     where
       extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
       addBinding (var, value) = do ref <- newIORef value
                                    return (var, ref)
