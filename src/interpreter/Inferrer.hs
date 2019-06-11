module Inferrer where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Either
import Data.Char(chr, intToDigit)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except

import AbsGrammar
--import Environment


type Env = Map.Map String Scheme

initEnv :: Env
initEnv = Map.empty

data TypeError
  = Mismatch Type Type
  | InfiniteType String Type
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
} deriving Show

--type TCM s a = StateT s (ReaderT Env (ExceptT TypeError (Identity))) a

initTcState :: TcState
initTcState = TcState {
  tcsNS = NameSupply { counter = 0 },
  tcsSubst = emptySubst,
  constraints = Seq.empty
}

type Constraint = (Type, Type)
type Constraints = Seq.Seq Constraint

type Unifier = (Subst, Constraints)

data NameSupply = NameSupply { counter :: Int } deriving Show

type Subst = Map.Map String Type


infix 4 |->
class Substitutable a where
  (|->) :: Subst -> a -> a
  ftv :: a -> Set.Set String

instance Substitutable Type where
  (|->) s t = case t of
    TConstr c -> TConstr c
    TVar (LIdent x) -> Map.findWithDefault t x s
    TArr l r -> TArr (s |-> l) (s |-> l)
  ftv t = case t of
    TConstr c -> Set.empty
    TVar (LIdent x) -> Set.singleton x
    TArr l r -> Set.union (ftv l) (ftv r)

instance Substitutable Scheme where
  (|->) s (ForAll xs t) = let s' = foldr Map.delete s xs in ForAll xs $ s' |-> t
  ftv (ForAll xs t) = Set.difference (ftv t) (Set.fromList xs) 

instance Substitutable a => Substitutable [a] where
  (|->) = fmap . (|->)
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable a => Substitutable (Seq.Seq a) where
  (|->) = fmap . (|->)
  ftv = foldr (Set.union . ftv) Set.empty

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  (|->) s (t1, t2) = (s |-> t1, s |-> t2)
  ftv (t1, t2) = Set.union (ftv t1) (ftv t2)

emptySubst :: Subst
emptySubst = Map.empty

infix 4 <@>
(<@>) :: Subst -> Subst -> Subst
s1 <@> s2 = Map.map (s1 |->) s2 `Map.union` s1

evalTCM :: TCM a -> Either TypeError a
evalTCM tcm = runIdentity$ runExceptT $ runReaderT (evalStateT tcm initTcState) initEnv

runTCM :: TCM a -> Either TypeError (a, TcState)
runTCM tcm = runIdentity$ runExceptT $ runReaderT (runStateT tcm initTcState) initEnv

addConstraint :: Type -> Type -> TCM ()
addConstraint t1 t2 = do
  let c = (t1, t2)
  cs <- gets constraints
  modify (\state -> state { constraints = (cs Seq.|> c) } )
  return ()

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
  let s = Map.fromList $ zip xs xs'
  return $ s |-> t

generalize :: Type -> TCM Scheme
generalize t = do
  env <- ask
  let ftvEnv = ftv $ Map.elems env
  let xs = Set.toList $ Set.difference (ftv t) ftvEnv
  return $ ForAll xs t  

infer :: Expr -> TCM Type
infer expr = case expr of
  EInt _ -> return int
  EBool _ -> return bool
  
  EVar (LIdent x) -> do
    found <- asks $ Map.lookup x
    case found of
      Nothing -> throwError $ NotInScope x
      Just s -> instantiate s

  ELambda ((Arg (LIdent x)):xs) expr -> do
    tv <- freshName
    let new env = Map.insert x (ForAll [] tv) env
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
      let new = Map.insert x s
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
  addConstraint t1 t2
  return tv

occursCheck :: String -> Type -> Bool
occursCheck x t = Set.member x (ftv t)

bind :: String -> Type -> TCM Subst
bind x t | t == TVar (LIdent x) = return emptySubst
         | occursCheck x t = throwError $ InfiniteType x t
         | otherwise = return $ Map.singleton x t

unify :: Type -> Type -> TCM Subst
unify t1 t2 | t1 == t2 = return emptySubst
unify (TVar (LIdent x)) t = bind x t
unify t (TVar (LIdent x)) = bind x t
unify (TArr ll lr) (TArr rl rr) = do
  sl <- unify ll rl
  sr <- unify (sl |-> lr) (sl |-> rr)
  return $ sr <@> sl
unify t1 t2 = throwError $ Mismatch t1 t2

solve :: TCM Subst
solve = do
  subs <- gets tcsSubst
  cons <- gets constraints
  case cons of
    Seq.Empty -> return subs
    (t1, t2) Seq.:<| cs -> do
      subs' <- unify t1 t2
      state <- get
      put state { tcsSubst = subs' <@> subs, constraints = subs' |-> cs }
--      solve
      return $ subs' <@> subs

typeOf :: Expr -> TCM Type
typeOf expr = do
  t <- infer expr
  s <- solve
  return $ s |-> t
  --return t
