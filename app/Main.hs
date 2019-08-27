module Main where

import System.Environment
import Eval
import Control.Monad.Except

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ fmap show $ readExpr (head args) >>= eval
     putStrLn $ extractValue $ trapError evaled
