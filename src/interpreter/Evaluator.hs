module Evaluator( execute, Value(..), EvalError(..), ValueEnv, initVEnv ) where

import qualified Data.Map as Map
import Data.Either

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import AbsGrammar


--type EvalError = String -- TODO

data EvalError
  = DivByZero
  | NoPattern
  | UnknownError String

instance Show EvalError where
  show DivByZero = "Divide by zero"
  show NoPattern = "Non-exhaustive patterns"

type ValueEnv = Map.Map String LazyValue

data LazyValue = LazyValue { venv :: ValueEnv, expr :: Expr } deriving (Eq, Ord, Read, Show)

data Value
  = VInt Integer
  | VBool Bool
  | VClosure [Arg] Expr ValueEnv
  | VConstr String [LazyValue]
  deriving (Eq, Ord, Read)

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VClosure _ _ _) = "<function>"
  show (VConstr s lvals) =
    let x = runIdentity $ runExceptT $ runReaderT (mapM evaluateLazyValue lvals) initVEnv
    in case x of
      Right r -> s ++ (unwords $ ("":(map show r)))
      Left e -> show e

type SEM a = StateT ValueEnv (ExceptT EvalError Identity) a
type REM a = ReaderT ValueEnv (ExceptT EvalError Identity) a

initVEnv :: ValueEnv
initVEnv = Map.empty

evaluateLazyValue :: LazyValue -> REM Value
evaluateLazyValue (LazyValue env expr) = local (const env) $ evaluate expr

evaluate :: Expr -> REM Value
evaluate exp = case exp of
  EBool b ->
    case b of
      BTrue -> return (VBool True)
      BFalse -> return (VBool False)

  EInt a -> return (VInt a)
  
  EVar (LIdent x) -> do
    found <- asks $ Map.lookup x
    case found of
      Just lv -> evaluateLazyValue lv
      Nothing -> throwError $ UnknownError $ "Variable not in scope: " ++ x
  
  ECVar (UIdent x) -> do
    found <- asks $ Map.lookup x
    case found of
      Just lv -> evaluateLazyValue lv
      Nothing -> throwError $ UnknownError $ "Constructor not in scope: " ++ x

  EConstr (UIdent c) exprs -> do
    env <- ask
    return $ VConstr c $ map (\expr-> LazyValue env expr) exprs 
    --vals <- mapM evaluate exprs 
    --return $ VConstr c vals

  ELambda args expr -> do
    env <- ask
    return $ VClosure args expr env

  EApply lexpr rexpr -> do
    ele <- evaluate lexpr
    case ele of
      VClosure args@((Arg (LIdent x)):xs) fexpr fenv -> do
        fenv' <- asks (\env -> Map.insert x (LazyValue env rexpr) fenv)
        case xs of
          [] -> local (const fenv') $ evaluate fexpr
          _ -> return $ VClosure xs fexpr fenv'
      _ -> throwError $ UnknownError $ "Type mismatch"

  ELet decls expr -> do
    env <- ReaderT (execStateT (forM_ decls declare))
    local (const env) (evaluate expr)

  EIfte bexpr lexpr rexpr -> do
    val <- evaluate bexpr
    case val of
      VBool b -> if b then evaluate lexpr else evaluate rexpr
      _ -> throwError $ UnknownError $ "Type mismatch"

  EOr lexpr rexpr -> do
    vl <- evaluate lexpr
    case vl of
      VBool True -> return $ VBool True
      VBool False -> evaluate rexpr

  EAnd lexpr rexpr -> do
    vl <- evaluate lexpr
    case vl of
      VBool False -> return $ VBool False
      VBool True -> evaluate rexpr

  EEq le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) -> return $ VBool $ x == y
  ENeq le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) -> return $ VBool $ x /= y
  ELeq le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) -> return $ VBool $ x <= y
  ELes le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) -> return $ VBool $ x < y
  EGre le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) -> return $ VBool $ x > y
  EGeq le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) -> return $ VBool $ x >= y
  EAdd le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) -> return $ VInt $ x + y
  ESub le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) -> return $ VInt $ x - y
  EMul le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) -> return $ VInt $ x * y
  EDiv le re -> evaluate le >>= \(VInt x) -> evaluate re >>= \(VInt y) ->
    if y == 0 then throwError DivByZero else return $ VInt $ div x y

  EMatch expr matchings -> do
    case matchings of
      (Matching pat mexpr):t -> do
        (b, nenv) <- ReaderT (\env -> runStateT (matchPattern (LazyValue env expr) pat) env)
        if b then local (const nenv) (evaluate mexpr) else evaluate (EMatch expr t)
      [] -> lift $ throwError NoPattern

matchPattern :: LazyValue -> Pattern -> SEM Bool
matchPattern lv patt = case patt of
  PWildcard -> return True
  PInt p -> do
    val <- remToSem $ evaluateLazyValue lv
    return $ val == (VInt p)
  PBool p -> do
    val <- remToSem $ evaluateLazyValue lv
    let b = case p of BTrue -> True; BFalse -> False
    return $ val == (VBool b)
  PVar (LIdent p) -> do
    modify (Map.insert p lv)
    return True
  PConstr (UIdent p) pats -> do
    val <- remToSem $ evaluateLazyValue lv
    case val of
      VConstr v vs -> if v == p
        then foldM (\b (av, ap) -> if b then matchPattern av ap else return False) True (zip vs pats)
        else return False
      _ -> return False

declare :: Decl -> SEM ()
declare decl = case decl of
  DVar (LIdent x) expr -> do
    env <- get
    let nenv = Map.insert x (LazyValue nenv expr) env
    put nenv
  DFunc (LIdent f) args expr -> do
    env <- get
    let nenv = Map.insert f (LazyValue nenv (ELambda args expr)) env
    put nenv

defineType :: TypeDef -> SEM ()
defineType (TypeDef _ vars constrs) = do
  let prepare (Constr ic@(UIdent c) ts) = case ts of
        [] -> (c, EConstr ic [])
        _ -> (c, ELambda args expr) where
          pom = map (\a-> LIdent ('x':(show a))) [0..(length ts - 1)]
          args = map Arg pom
          expr = EConstr ic $ map EVar pom
  let constrs' = map prepare constrs
  env <- get
  put $ foldr (\(c, expr) a -> Map.insert c (LazyValue env expr) a) env constrs'


execInstr :: Instr -> SEM (Maybe Value)
execInstr instr = case instr of
  IType tdef -> do
    defineType tdef
    return Nothing
              
  IDecl decl -> do
    declare decl
    return Nothing

  IExpr expr -> do
    v <- remToSem $ evaluate expr
    return $ Just v

execProgram :: Program -> SEM [Maybe Value]
execProgram (Prog instrs) = forM instrs execInstr

execute :: Program -> ValueEnv -> Either EvalError ([Maybe Value], ValueEnv)
execute p e = runIdentity $ runExceptT $ runStateT (execProgram p) e

remToSem :: REM a -> SEM a
remToSem x = StateT $ \e -> fmap (\x -> (x, e)) $ runReaderT x e
