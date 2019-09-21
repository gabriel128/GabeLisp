{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispValEx where

import Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader

data LispVal =
  Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  -- deriving (Typeable)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal}

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving ( Monad, Functor, Applicative, MonadReader EnvCtx, MonadIO)


instance Show LispVal where
  show = T.unpack . showLispVal

showLispVal :: LispVal -> T.Text
showLispVal (Atom text) = text
showLispVal (List lispvals) = T.concat ["(", T.unwords $ showLispVal <$> lispvals, ")"]
showLispVal (Number number) =  T.pack $ show number
showLispVal (String text) = T.concat ["\"", text, "\""]
showLispVal (Fun _) = "(internal fn)"
showLispVal (Lambda _ _) = "(lambda function)"
showLispVal Nil = "nil"
showLispVal (Bool True) = "#t"
showLispVal (Bool False) = "#f"
