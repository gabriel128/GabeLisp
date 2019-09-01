module Main where

import Eval
import LispVal
import Parser
import Primitives
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
runRepl = loadPrimitiveBindings >>= until_ (== ":q") (readPrompt "Schemo> ") . evalAndPrint

loadPrimitiveBindings :: IO Env
loadPrimitiveBindings = nullEnv >>= (flip bindVars $ fmap makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

main :: IO ()
main = runRepl

{-
(defo (f x) (+ x 2))
(f 1)

(defo (g y) (+ 1 y))
(defo (f x y) (+ x y))
(defo (h inc) (lambdo (x) (+ 3 4)))
(f)


((lambda (x) (+ x 2)) 3)
-}
