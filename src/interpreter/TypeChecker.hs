module TypeChecker where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity

import AbsGrammar
import Environment


typeInt :: Type
typeInt = TConstr $ Constr (UIdent "Int") []

typeBool :: Type
typeBool = TConstr $ Constr (UIdent "Bool") []

{-
defineType :: TypeDef -> StateT Env (ErrorT InterpretError Identity) ()
defineType (TypeDef (UIdent t) params constrs) = do
  let nconstrs = map (prepareConstr) constrs
  env <- get
  put $ foldr (\(key, expr) acc -> insertType key (case runIdentity $ runErrorT $ runReaderT (typeOf expr) env of Right a -> a) acc) env nconstrs
-}

declare :: Decl -> StateT Env (ErrorT InterpretError Identity) ()
declare (DFunc (LIdent f) args expr t) = do
  let tf = foldr (TArr) t $ map (\(Arg _ ta) -> ta) args
  nenv <- gets (insertType f tf)
  StateT (\_ -> fmap (\_ -> ((), nenv)) (runReaderT (checkType (prepareFunc f args expr) tf) nenv))

checkType :: Expr -> Type -> ReaderT Env (ErrorT InterpretError Identity) Type
checkType e t = do
  t' <- typeOf e
  if t == t' then return t else throwError "Type does not match"

typeOf :: Expr -> ReaderT Env (ErrorT InterpretError Identity) Type

typeOf (EInt _) = return typeInt

typeOf (EBool _) = return typeBool

typeOf (EVar (LIdent x)) = lookupByName x lookupType ("Variable not in scope: " ++)

typeOf (EConstr (UIdent c)) = lookupByName c lookupType ("Constructor not in scope: " ++)

typeOf (EIfte bexpr lexpr rexpr) = do
  checkType bexpr typeBool
  tl <- typeOf lexpr
  checkType rexpr tl

typeOf (ELambda (Arg (LIdent x) tx) args expr) = do
  let nexpr = case args of [] -> expr ; h:t -> ELambda h t expr
  te <- local (insertType x tx) $ typeOf nexpr
  return $ TArr tx te

typeOf (EApply lexpr rexpr) = do
  tl <- typeOf lexpr
  case tl of
    TArr ta t -> checkType rexpr ta >> return t
    _ -> throwError "Incorrect type in function application"

typeOf (ELet decls expr) = do
  env <- ReaderT (execStateT (forM_ decls declare))
  local (\_->env) (typeOf expr)

typeOf (EMatch expr matchings) = do
  let (h:t) = map (\(Matching _ e) -> e) matchings
  th <- typeOf h
  foldM (flip checkType) th t

--typeOf (EData (UIdent c) exprs) = return $ TData (Constr (UIdent "TODO") [])

typeOf (EEq lexpr rexpr) = do
  tl <- typeOf lexpr
  tr <- typeOf rexpr
  unless (tl == tr) $ throwError "Incorrect type for equality expression"
  return typeBool
typeOf (ENeq lexpr rexpr) = typeOf (EEq lexpr rexpr)

typeOf (EOr lexpr rexpr) = typeOfBinOp typeBool typeBool lexpr rexpr
typeOf (EAnd lexpr rexpr) = typeOfBinOp typeBool typeBool lexpr rexpr
typeOf (ELeq lexpr rexpr) = typeOfBinOp typeInt typeBool lexpr rexpr
typeOf (ELes lexpr rexpr) = typeOfBinOp typeInt typeBool lexpr rexpr
typeOf (EGre lexpr rexpr) = typeOfBinOp typeInt typeBool lexpr rexpr
typeOf (EEq lexpr rexpr) = typeOfBinOp typeInt typeBool lexpr rexpr
typeOf (EGeq lexpr rexpr) = typeOfBinOp typeInt typeBool lexpr rexpr
typeOf (EAdd lexpr rexpr) = typeOfBinOp typeInt typeInt lexpr rexpr
typeOf (ESub lexpr rexpr) = typeOfBinOp typeInt typeInt lexpr rexpr
typeOf (EMul lexpr rexpr) = typeOfBinOp typeInt typeInt lexpr rexpr
typeOf (EDiv lexpr rexpr) = typeOfBinOp typeInt typeInt lexpr rexpr

typeOfBinOp :: Type -> Type -> Expr -> Expr -> ReaderT Env (ErrorT InterpretError Identity) Type
typeOfBinOp targ tres lexpr rexpr = do
  tl <- typeOf lexpr
  tr <- typeOf rexpr
  if (tl, tr) == (targ, targ)
    then return $ tres
    else lift $ throwError "Incorrect type for infix expression"

