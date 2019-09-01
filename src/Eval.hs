module Eval (eval) where

import Data.IORef
import LispVal
import Primitives
import Control.Monad.Except
import Data.Maybe

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval _ (List [Atom "quote", val]) = return val
eval _ (List [Atom "quoto", val]) = return val
eval env (List [Atom "define", Atom var, val]) = eval env val >>= define env var
eval env (List [Atom "defo", Atom var, val]) = eval env val >>= define env var
eval env (Cond pred conseq alt) = do
  p <- eval env pred
  case p of
    Bool True -> eval env conseq
    _ -> eval env alt
eval env (List (Atom f : args)) = mapM (eval env) args >>= apply f
eval _ badform = throwError $ BadSpecialForm "Bad special form" badform

apply :: String -> [LispVal] -> IOThrowsError LispVal
apply f args =
  -- maybe (throwError $ NotFunction "Unrecognized primitive function" f) ($ args) (lookup f primitives)
  case lookup f primitives of
    Just function -> liftThrows $ function args
    Nothing -> throwError $ NotFunction "Unrecognized primitive unction" f

--- ENV Handling ----

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Unbound variable" var) (liftIO . readIORef) (lookup var env)

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
  else setVar envRef var value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
