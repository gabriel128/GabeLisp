module Main where

import Eval
import LispVal
import Parser
import System.IO
import Data.Char

evalString :: Env -> String -> IO String
evalString env expr =
  if all isSpace expr
  then return ""
  else runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== ":q") (readPrompt "GabeLisp> ") . evalAndPrint

main :: IO ()
main = runRepl
