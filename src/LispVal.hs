{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispVal where

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
