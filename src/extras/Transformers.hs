module Transformers where


import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as Map

type Name = String -- variable names

data Exp =
  Lit Integer -- expressions
  | Var Name
  | Plus Exp Exp
  | Lambda Name Exp
  | App Exp Exp
  deriving (Show)

data Value =
  IntVal Integer -- values
  | FunVal Env Name Exp
  deriving (Show)

extractVal :: Value -> Integer
extractVal (IntVal a) = a
extractVal (FunVal _ _ _) = undefined

type Env = Map.Map Name Value -- mapping from names to values

eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) = IntVal i
eval0 env (Var name) = fromJust (Map.lookup name env)
eval0 env (Plus exp1 exp2) =  IntVal $ extractVal (eval0 env exp1) + extractVal (eval0 env exp2)
eval0 env (Lambda name expr) = FunVal env name expr
eval0 env (App e1 e2) =
  let val1 = eval0 env e1
      val2 = eval0 env e2
  in case val1 of
       FunVal env0 n body -> eval0 (Map.insert n val2 env0) body
-- 12 + ((λx → x)(4 + 2))
exampleExp :: Exp
exampleExp = Lit 12 `Plus` (App (Lambda "x" (Var "x")) (Lit 4 `Plus` Lit 2))

exampleExp1 = (App (Lambda "x" (Var "x")) (Lit 4))

invExp = (App (Lambda "y" (Var "x")) (Lit 4))

invExp2 = (Plus (Lit 1) (Lambda "x" (Var "x")))

--- Monad Transformers 2 --

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Exp -> Eval1 Value
eval1 _ (Lit i) = return $ IntVal i
eval1 env (Var n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval1 env (Plus e1 e2) = do
  e1' <- eval1 env e1
  e2' <- eval1 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
eval1 env (Lambda name expr) = return $ FunVal env name expr
eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body -> eval1 (Map.insert n val2 env') body
    -- _ -> throwError "type Error"

--- MT 3 ---

type Eval2 a = ExceptT String Identity a

runEval2 :: ExceptT String Identity a -> Either String a
runEval2 a = runIdentity (runExceptT a)

eval2 :: Env -> Exp -> Eval2 Value
eval2 _ (Lit i) = return $ IntVal i
eval2 env (Var n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval2 env (Plus e1 e2) = do
  e1' <- eval2 env e1
  e2' <- eval2 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type Error"
eval2 env (Lambda name expr) = return $ FunVal env name expr
eval2 env (App e1 e2) = do
  val1 <- eval2 env e1
  val2 <- eval2 env e2
  case val1 of
    FunVal env' n body ->
      eval2 (Map.insert n val2 env') body
    _ -> catchError (throwError "type Error") (\_ -> return $ IntVal 0)

-- eval2c :: Env -> Exp -> Eval2 Value
-- eval2c _ (Lit i) = return $ IntVal i
-- eval2c env (Var n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
-- eval2c env (Plus e1 e2) = do
--     ~(IntVal i1) <- eval2c env e1
--     ~(IntVal i2) <- eval2c env e2
--     return $ IntVal (i1 + i2 )
-- eval2c env (Lambda n e) = return $ FunVal env n e
-- eval2c env (App e1 e2) = do
--   ~(FunVal env' n body) <- eval2c env e1
--   val2 <- eval2c env e2
--   eval2c (Map.insert n val2 env') body

-- MT 4 --

type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env a = runIdentity (runExceptT (runReaderT a env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
  env <- ask
  case Map.lookup n env of
    (Just a) -> return a
    Nothing -> throwError ("unbound variable: " ++ n)
  -- maybe (fail ("undefined variable: " ++ n)) return $
eval3 (Plus e1 e2) = do
  e1' <- eval3 e1
  e2' <- eval3 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type Error in addition"
eval3 (Lambda name expr) = do
  env <- ask
  return $ FunVal env name expr
eval3 (App e1 e2) = do
  val1 <- eval3 e1
  val2 <- eval3 e2
  case val1 of
    FunVal env' n body ->
      local (const (Map.insert n val2 env')) (eval3 body)
    _ -> catchError (throwError "type Error in application") (\_ -> return $ IntVal 0)

-- MT 5 --


type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env initState a = runIdentity (runStateT (runExceptT (runReaderT a env)) initState)

tick :: (Num s, MonadState s m) => m ()
tick = do
  state <- get
  put (state + 1)

eval4 :: Exp -> Eval4' Value
eval4 (Lit i) = do
  tick
  return $ IntVal i
eval4 (Var n) = do
  tick
  env <- ask
  case Map.lookup n env of
    (Just a) -> return a
    Nothing -> throwError ("unbound variable: " ++ n)
  -- maybe (fail ("undefined variable: " ++ n)) return $
eval4 (Plus e1 e2) = do
  tick
  e1' <- eval4 e1
  e2' <- eval4 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type Error in addition"
eval4 (Lambda name expr) = do
  tick
  env <- ask
  return $ FunVal env name expr
eval4 (App e1 e2) = do
  tick
  val1 <- eval4 e1
  val2 <- eval4 e2
  case val1 of
    FunVal env' n body ->
      local (const (Map.insert n val2 env')) (eval4 body)
    _ -> catchError (throwError "type Error in application") (\_ -> return $ IntVal 0)

type Eval4' alpha = ReaderT Env (StateT Integer (ExceptT String Identity)) alpha

runEval4' :: Env -> Integer -> Eval4' a -> Either String (a, Integer)
runEval4' env initState a = runIdentity (runExceptT (runStateT (runReaderT a env) initState))

-- MT 6 --

type Eval5 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env initState a = runIdentity (runStateT (runWriterT (runExceptT (runReaderT a env))) initState)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
  tick
  tell ["Literal ran"]
  return $ IntVal i
eval5 (Var n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    (Just a) -> return a
    Nothing -> throwError ("unbound variable: " ++ n)
  -- maybe (fail ("undefined variable: " ++ n)) return $
eval5 (Plus e1 e2) = do
  tick
  tell ["plus here"]
  e1' <- eval5 e1
  e2' <- eval5 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type Error in addition"
eval5 (Lambda name expr) = do
  tick
  env <- ask
  return $ FunVal env name expr
eval5 (App e1 e2) = do
  tick
  val1 <- eval5 e1
  val2 <- eval5 e2
  case val1 of
    FunVal env' n body ->
      local (const (Map.insert n val2 env')) (eval5 body)
    _ -> catchError (throwError "type Error in application") (\_ -> return $ IntVal 0)

-- MT 7 --

type Eval6 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env initState a = runStateT (runWriterT (runExceptT (runReaderT a env))) initState

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do
  tick
  liftIO $ print i
  tell ["Literal ran"]
  return $ IntVal i
eval6 (Var n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    (Just a) -> return a
    Nothing -> throwError ("unbound variable: " ++ n)
  -- maybe (fail ("undefined variable: " ++ n)) return $
eval6 (Plus e1 e2) = do
  tick
  tell ["plus here"]
  e1' <- eval6 e1
  e2' <- eval6 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type Error in addition"
eval6 (Lambda name expr) = do
  tick
  env <- ask
  return $ FunVal env name expr
eval6 (App e1 e2) = do
  tick
  val1 <- eval6 e1
  val2 <- eval6 e2
  case val1 of
    FunVal env' n body ->
      local (const (Map.insert n val2 env')) (eval6 body)
    _ -> catchError (throwError "type Error in application") (\_ -> return $ IntVal 0)

-- EitherIO def --
-- import qualified Data.Text.IO as T

exampleEIO :: EitherIO String String
exampleEIO = liftEither' (Right "blah")

example2EIO :: EitherIO String String
example2EIO = liftIO' getLine

data EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }

instance Functor (EitherIO e) where
  fmap f ex = EitherIO fmapped
    where
      unwrapped = runEitherIO ex
      fmapped = (fmap . fmap) f unwrapped

instance Applicative (EitherIO e) where
  pure a = EitherIO $ return (Right a)
  -- f :: IO (Either e (a -> b))
  -- unwrapped :: IO (Either e a)
  -- (<*>) :: EitherIO e (a -> b) -> EitherIO e a -> EitherIO e b
  (EitherIO f) <*> (EitherIO unwrapped) = EitherIO $ liftA2 (<*>) f unwrapped

instance Monad (EitherIO e) where
  return = pure
  -- f :: a -> EitherIO e b
  EitherIO unwrapped >>= f = EitherIO $ do
    eitherA <- unwrapped
    case eitherA of
      (Right a) -> runEitherIO (f a)
      (Left a) -> return (Left a)

liftEither' :: Either e a -> EitherIO e a
liftEither' x = EitherIO (return x)

liftIO' :: IO a -> EitherIO e a
liftIO' x = EitherIO (fmap Right x)
