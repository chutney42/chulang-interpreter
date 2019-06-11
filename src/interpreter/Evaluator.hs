module Evaluator( evaluate, declare, Value(..)) where

import Data.Either

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity

import AbsGrammar
import Environment


data Value
  = VInt Integer
  | VBool Bool
  | VClosure [Arg] Expr Env
  | VConstr String [LazyValue]
  deriving (Eq, Ord, Read)


instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
--  show (VList l) = show l
  show (VClosure _ _ _) = "function"
  show (VConstr s lvals) =
    let x = runIdentity $ runErrorT $ runReaderT (mapM evaluateLazyValue lvals) initEnv
    in case x of
      Right r -> s ++ show r
      Left e -> show e


evaluateLazyValue :: LazyValue -> ReaderT Env (ErrorT InterpretError Identity) Value
evaluateLazyValue (env, expr) = local (\_->env) $ evaluate expr

declare :: Decl -> StateT Env (ErrorT InterpretError Identity) ()

declare (DVar (LIdent x) expr) = do
  env <- get
  let nenv = insertLazyValue x (nenv, expr) env
  put nenv

declare (DFunc (LIdent f) args expr) = do
  env <- get
  let nenv = insertLazyValue f (nenv, ELambda args expr) env
  put nenv


--defineType :: TypeDef -> StateT Env (ErrorT InterpretError Identity) ()
--defineType (TypeDef (UIdent t) params constrs) = do
--  let nconstrs = map (prepareConstr) constrs
--  env <- get
--  put $ foldr (\(key, expr) acc -> insertLazyValue key (env, expr) acc) env nconstrs


evaluate :: Expr -> ReaderT Env (ErrorT InterpretError Identity) Value

evaluate (EVar (LIdent x)) = lookupByName x lookupLazyValue ("Variable not in scope: " ++) >>= evaluateLazyValue

evaluate (EConstr (UIdent x)) = lookupByName x lookupLazyValue ("Constructor not in scope: " ++) >>= evaluateLazyValue

evaluate (EData (UIdent c) exprs) = do
  env <- ask
  return $ VConstr c $ map ((,) env) exprs

evaluate (EBool b) =
  case b of
    BTrue -> return (VBool True)
    BFalse -> return (VBool False)

evaluate (EInt a) = return (VInt a)

evaluate (ELambda args expr) = do
  env <- ask
  return $ VClosure args expr env

evaluate (EApply lexpr rexpr) = do
  VClosure args@((Arg (LIdent x)):xs) fexpr fenv <- evaluate lexpr
  fenv' <- asks (\env -> insertLazyValue x (env, rexpr) fenv)
  case xs of
    [] -> local (\_ -> fenv') $ evaluate fexpr
    _ -> return $ VClosure args fexpr fenv'

evaluate (ELet decls expr) = do
  env <- ReaderT (execStateT (forM_ decls declare))
  local (\_->env) (evaluate expr)

evaluate (EIfte bexpr lexpr rexpr) = do
  vb <- evaluate bexpr
  case vb of
    VBool b -> if b then evaluate lexpr else evaluate rexpr

evaluate (EOr lexpr rexpr) = do
  vl <- evaluate lexpr
  case vl of
    VBool True -> return $ VBool True
    VBool False -> evaluate rexpr

evaluate (EAnd lexpr rexpr) = do
  vl <- evaluate lexpr
  case vl of
    VBool False -> return $ VBool False
    VBool True -> evaluate rexpr

evaluate (EEq lexpr rexpr) = do
  vl <- evaluate lexpr
  vr <- evaluate rexpr
  return $ VBool $ vl == vr

evaluate (ENeq lexpr rexpr) = do
  vl <- evaluate lexpr
  vr <- evaluate rexpr
  return $ VBool $ vl /= vr

evaluate (ELeq lexpr rexpr) = evalBinOp (\x y -> VBool $ x <= y) lexpr rexpr
evaluate (ELes lexpr rexpr) = evalBinOp (\x y -> VBool $ x < y) lexpr rexpr
evaluate (EGre lexpr rexpr) = evalBinOp (\x y -> VBool $ x > y) lexpr rexpr
evaluate (EGeq lexpr rexpr) = evalBinOp (\x y -> VBool $ x >= y) lexpr rexpr
evaluate (EAdd lexpr rexpr) = evalBinOp (\x y -> VInt $ x + y) lexpr rexpr
evaluate (ESub lexpr rexpr) = evalBinOp (\x y -> VInt $ x - y) lexpr rexpr
evaluate (EMul lexpr rexpr) = evalBinOp (\x y -> VInt $ x * y) lexpr rexpr
evaluate (EDiv lexpr rexpr) = do
  vl <- evaluate lexpr
  vr <- evaluate rexpr
  case (vl, vr) of
    (_, VInt 0) -> lift $ throwError "Divide by zero"
    (VInt l, VInt r) -> return $ VInt (div l r)

evaluate (EMatch expr matchings) = do
  case matchings of
    (Matching pat mexpr):t -> do
      (b, nenv) <- ReaderT (\env -> runStateT (matchPattern (env, expr) pat) env)
      if b then local (\_->nenv) (evaluate mexpr) else evaluate (EMatch expr t)
    [] -> lift $ throwError "Non-exhaustive patterns"


matchPattern :: LazyValue -> Pattern -> StateT Env (ErrorT InterpretError Identity) Bool

matchPattern _ PWildcard = return True

matchPattern lv (PInt p) = do
  val <- StateT (\e -> fmap (\x -> (x, e)) $ runReaderT (evaluateLazyValue lv) e)
  return $ val == (VInt p)

matchPattern lv (PBool p) = do
  val <- StateT (\e -> fmap (\x -> (x, e)) $ runReaderT (evaluateLazyValue lv) e)
  let b = case p of BTrue -> True; BFalse -> False
  return $ val == (VBool b)

matchPattern lv (PVar (LIdent p)) = do
  modify (insertLazyValue p lv)
  return True

matchPattern lv (PConstr (UIdent p) pats) = do
  val <- StateT (\e -> fmap (\x -> (x, e)) $ runReaderT (evaluateLazyValue lv) e)
  case val of
    VConstr v vs -> if v == p
      then foldM (\b (av, ap) -> if b then matchPattern av ap else return False) True (zip vs pats)
      else return False
    _ -> return False

evalBinOp :: (Integer -> Integer -> Value) -> Expr -> Expr -> ReaderT Env (ErrorT InterpretError Identity) Value
evalBinOp binOp lexpr rexpr = do
  vl <- evaluate lexpr
  vr <- evaluate rexpr
  case (vl, vr) of
    (VInt l, VInt r) -> return $ binOp l r
