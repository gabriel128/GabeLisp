module Main where

import Control.Monad.Except
import System.Environment
import Eval
import LispError
import Parser

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right a) = a
extractValue (Left _) = undefined

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
