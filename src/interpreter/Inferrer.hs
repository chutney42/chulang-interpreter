module Inferrer where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Char(chr, intToDigit)

import Control.Monad.State
import Control.Monad.Reader
--import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Except

import AbsGrammar
--import Environment


type Env = M.Map String Scheme

initEnv :: Env
initEnv = M.empty

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope String
  deriving (Eq, Show)

data Scheme = ForAll [String] Type

--type TCM a = ExceptT TypeError (StateT TcState (Reader Env)) a
type TCM a = StateT TcState (ReaderT Env (ExceptT TypeError (Identity))) a
data TcState = TcState {
  tcsNS :: NameSupply, -- dostawca nazw
  tcsSubst :: Subst, -- podstawienie
  constraints :: Constraints -- równania
}

initTcState :: TcState
initTcState = TcState {
  tcsNS = NameSupply { counter = 0 },
  tcsSubst = emptySubst,
  constraints = []
}

type Constraint = (Type, Type)
type Constraints = [Constraint]

data NameSupply = NameSupply { counter :: Int }

type Subst = M.Map String Type

runTCM :: TCM a -> Either TypeError a
runTCM tcm = runIdentity$ runExceptT $ runReaderT (evalStateT tcm initTcState) initEnv
--runTCM tcm =
--  case runExcept tcm of
--    Left err -> 
--  runReader $ evalStateT t initTcState)

addConstraint :: Type -> Type -> TCM ()
addConstraint t1 t2 = do
  let c = (t1, t2)
  cs <- gets constraints
  modify (\state -> state { constraints = (c:cs) } )
  return ()

emptySubst :: Subst
emptySubst = M.empty

composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = M.map (substitute s1) s2 `M.union` s1

substitute :: Subst -> Type -> Type
substitute s t = case t of
  TConstr c -> TConstr c
  TVar (LIdent x) -> M.findWithDefault t x s
  TArr l r -> TArr (substitute s l) (substitute s l)

ftv :: Type -> S.Set String
ftv t = case t of
  TConstr c -> S.empty
  TVar (LIdent x) -> S.singleton x
  TArr l r -> S.union (ftv l) (ftv r)

ftvScheme :: Scheme -> S.Set String
ftvScheme (ForAll xs t) = S.difference (ftv t) (S.fromList xs) 
--substituteForAll :: Subst -> Scheme -> Scheme
--substituteForAll s (ForAll xs t) =
--  let s' = foldr M.delete s xs in ForAll xs $ apply s' t
  
int :: Type
int = TConstr $ Constr (UIdent "Int") []

bool :: Type
bool = TConstr $ Constr (UIdent "Bool") []

freshName :: TCM Type
freshName = do
  state <- get
  let c = counter $ tcsNS state
  put state { tcsNS = NameSupply { counter = c + 1 } } 
  let f = chr $ 97 + (mod c 25)
  let s = intToDigit $ div c 25
  return $ TVar $ LIdent [f, s]

instantiate :: Scheme -> TCM Type
instantiate (ForAll xs t) = do
  xs' <- mapM (\_ -> freshName) xs
  let s = M.fromList $ zip xs xs'
  return $ substitute s t

generalize :: Type -> TCM Scheme
generalize t = do
  env <- ask
  let ftvEnv = foldr (S.union . ftvScheme) S.empty (M.elems env)
  let xs = S.toList $ S.difference (ftv t) ftvEnv
  return $ ForAll xs t  

occursCheck :: String -> Type -> Bool
occursCheck x t = S.member x (ftv t)

--unify :: Type -> Type -> TCM Subst

--bind :: String -> Type -> TCM Subst

infer :: Expr -> TCM Type
infer expr = case expr of
  EInt _ -> return int
  EBool _ -> return bool
  
  EVar (LIdent x) -> do
    found <- asks $ M.lookup x
    case found of
      Nothing -> throwError $ NotInScope x
      Just s -> instantiate s

  ELambda ((Arg (LIdent x)):xs) expr -> do
    tv <- freshName
    let new env = M.insert x (ForAll [] tv) env
    let expr' = case xs of
          [] -> expr
          _ -> ELambda xs expr
    t <- local new $ infer expr'
    return (TArr tv t)

  EApply lexpr rexpr -> do
    ltype <- infer lexpr
    rtype <- infer rexpr
    tv <- freshName
    addConstraint ltype (TArr rtype tv)
    return tv

  ELet decls rexpr -> case decls of
    [] -> infer rexpr
    d:ds -> do
      let (x, lexpr) = case d of
            DVar (LIdent v) expr -> (v, expr)
            DFunc (LIdent f) args expr -> (f, ELambda args expr)
      env <- ask
      ltype <- infer lexpr
      s <- generalize ltype
      let new = M.insert x s
      let rexpr' = case ds of
            [] -> rexpr
            _ -> ELet ds rexpr
      rtype <- local new $ infer rexpr'
      return rtype

  EIfte bexpr lexpr rexpr -> do
    bt <- infer bexpr
    lt <- infer lexpr
    rt <- infer rexpr
    addConstraint bt bool
    addConstraint lt rt
    return lt

  EOr lexpr rexpr -> inferBinOp bool bool lexpr rexpr
  EAnd lexpr rexpr -> inferBinOp bool bool lexpr rexpr
  EEq lexpr rexpr -> inferBinOp int bool lexpr rexpr
  ENeq lexpr rexpr -> inferBinOp int bool lexpr rexpr
  ELeq lexpr rexpr -> inferBinOp int bool lexpr rexpr
  ELes lexpr rexpr -> inferBinOp int bool lexpr rexpr
  EGre lexpr rexpr -> inferBinOp int bool lexpr rexpr
  EGeq lexpr rexpr -> inferBinOp int bool lexpr rexpr
  EAdd lexpr rexpr -> inferBinOp int int lexpr rexpr
  ESub lexpr rexpr -> inferBinOp int int lexpr rexpr
  EMul lexpr rexpr -> inferBinOp int int lexpr rexpr
  EDiv lexpr rexpr -> inferBinOp int int lexpr rexpr
  
  
inferBinOp :: Type -> Type -> Expr -> Expr -> TCM Type
inferBinOp targ tres lexpr rexpr = do
  lt <- infer lexpr
  rt <- infer rexpr
  tv <- freshName
  let t1 = TArr lt (TArr rt tv)
  let t2 = TArr targ (TArr targ tres)
  return tv
