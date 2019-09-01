module Eval (eval, setVar, bindVars) where

import Data.IORef
import LispVal
import Control.Monad.Except
import Data.Maybe

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval _ (List [Atom "quoto", val]) = return val
eval env (List (Atom "defo" : List (Atom var : params) : body)) = makeFunc env params body >>= define env var
eval env (List [Atom "defo", Atom var, val]) = eval env val >>= define env var
eval env (List (Atom "lambda" : List params : body)) = makeFunc env params body
-- eval env (List (Atom "lambda" : arg@(Atom _) : body)) = makeFunc env [arg] body
eval env (Cond pred conseq alt) = do
  p <- eval env pred
  case p of
    Bool True -> eval env conseq
    _ -> eval env alt
eval env (List (f : args)) = do
  func <- eval env f
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badform = throwError $ BadSpecialForm "Bad special form" badform

{-
(defo (f x) (+ x 2))
(f 1)

((lambda (x) (+ x 2)) 3)
-}
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func params [body] closure) args
  | num params /= num args = throwError $ NumArgs (num params) args
  | otherwise = do
      env <- liftIO $ bindVars closure $ zip params args
      evalBody env
  where
    num = toInteger . length
    evalBody env = eval env body
apply _ _ = throwError $ DefaultError "Error applying function"

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
                                    return (filter (/='"') var, ref)
