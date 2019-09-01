module Main where

import System.Environment
import Eval
import LispVal
import Parser
import Primitives
import System.IO
import Data.Char

evalString :: Env -> String -> IO String
evalString env expr
  | all isSpace expr = return ""
  | otherwise = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

runRepl :: IO ()
runRepl = loadPrimitiveBindings >>= until_ (== ":q") (readPrompt "Schemo> ") . evalAndPrint

runFile :: [String] -> IO ()
runFile (headArgs:_) = do
  env <- loadPrimitiveBindings
  y <- runIOThrows (show <$> eval env (List [Atom "load", String headArgs]))
  hPutStrLn stderr y
runFile _ = error "Unexpected error reading file"

loadPrimitiveBindings :: IO Env
loadPrimitiveBindings = nullEnv >>= (flip bindVars $ fmap makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runFile args

{-
(defo (f x y) (+ x y))
(f 1 8)

(defo (g y) (+ 1 y))
(defo (f x y) (+ x y))
(defo (h inc) (lambdo (x) (+ 3 4)))
(f)


(defo (counter inc) (lambda (x) (+ x inc)))
-}
