module Evaluator( evaluate, declare, defineType, Value(..)) where

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
  | VFunc String LazyValue
  | VData String [LazyValue]
  deriving (Eq, Ord, Read)


instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
--  show (VList l) = show l
  show (VFunc s (_, e)) = "<function>"
  show (VData s lvals) =
    let x = runIdentity $ runErrorT $ runReaderT (mapM evaluateLazyValue lvals) initEnv
    in case x of
      Right r -> s ++ show r
      Left e -> show e


evaluateLazyValue :: LazyValue -> ReaderT Env (ErrorT InterpretError Identity) Value
evaluateLazyValue (env, expr) = local (\_->env) $ evaluate expr


declare :: Decl -> StateT Env (ErrorT InterpretError Identity) ()
declare (Decl (LIdent f) args expr _) = do
  env <- get
  let nenv = insertLazyValue f (nenv, prepareFunc f args expr) env
  put nenv


defineType :: TypeDef -> StateT Env (ErrorT InterpretError Identity) ()
defineType (TypeDef (UIdent t) params constrs) = do
  let nconstrs = map (prepareConstr) constrs
  env <- get
  put $ foldr (\(key, expr) acc -> insertLazyValue key (env, expr) acc) env nconstrs


evaluate :: Expr -> ReaderT Env (ErrorT InterpretError Identity) Value

evaluate (EVar (LIdent x)) = lookupByName x lookupLazyValue ("Variable not in scope: " ++) >>= evaluateLazyValue

evaluate (EConstr (UIdent x)) = lookupByName x lookupLazyValue ("Constructor not in scope: " ++) >>= evaluateLazyValue

evaluate (EData (UIdent c) exprs) = do
  env <- ask
  return $ VData c $ map ((,) env) exprs

evaluate (EBool b) =
  case b of
    BTrue -> return (VBool True)
    BFalse -> return (VBool False)

evaluate (EInt a) = return (VInt a)

evaluate (ELambda (Arg (LIdent x) _) l expr) = do
  env <- ask
  case l of
    [] -> return $ VFunc x (env, expr)
    (h:t) -> return $ VFunc x (env, (ELambda h t expr))

evaluate (EApply lexpr rexpr) = do
  f <- evaluate lexpr
  case f of
    VFunc x (fenv, fexpr) -> local (\env -> insertLazyValue x (env, rexpr) fenv)
                             (evaluate fexpr)
    _ -> lift $ throwError "Cannot apply expression. It must be a function."

evaluate (ELet decls expr) = do
  env <- ReaderT (execStateT (forM_ decls declare))
  local (\_->env) (evaluate expr)

evaluate (EIfte bexpr lexpr rexpr) = do
  vb <- evaluate bexpr
  case vb of
    VBool b -> if b then evaluate lexpr else evaluate rexpr
    _ -> lift $ throwError ("Condition must be of type bool")

evaluate (EOr lexpr rexpr) = do
  vl <- evaluate lexpr
  case vl of
    VBool True -> return $ VBool True
    VBool False -> evaluate rexpr
    _ -> lift $ throwError "Expressions must be of type bool"

evaluate (EAnd lexpr rexpr) = do
  vl <- evaluate lexpr
  case vl of
    VBool False -> return $ VBool False
    VBool True -> evaluate rexpr
    _ -> lift $ throwError "Expressions must be of type bool"

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
    _ -> lift $ throwError "Incorrect type for arithmetic expression"

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
    VData v vs -> if v == p
      then foldM (\b (av, ap) -> if b then matchPattern av ap else return False) True (zip vs pats)
      else return False
    _ -> return False


evalBinOp :: (Integer -> Integer -> Value) -> Expr -> Expr -> ReaderT Env (ErrorT InterpretError Identity) Value
evalBinOp binOp lexpr rexpr = do
  vl <- evaluate lexpr
  vr <- evaluate rexpr
  case (vl, vr) of
    (VInt l, VInt r) -> return $ binOp l r
    _ -> lift $ throwError "Incorrect type for arithmetic expression"


