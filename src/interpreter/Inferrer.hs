module Inferrer ( TypeEnv(..), typing, prettyType, TypeError(..), initTypeEnv ) where

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


type Arity = Int

data TypeEnv = TEnv { varTypes :: Map.Map String Scheme,
                      typeConstrs :: Map.Map String Arity }

data TypeError
  = Mismatch Type Type
  | InfiniteType String Type
  | VarNotInScope String
  | ValConstrNotInScope String
  | TypeConstrNotInScope String
  | TypeVarNotInScope String
  | ConflictingVars String
  | ConstrutorWrongArity String Int Int
  
instance Show TypeError where
  show (Mismatch t1 t2) = "Type mismatch: " ++ (prettyType t1) ++ " with " ++ (prettyType t2)
  show (InfiniteType x t) = "Cannot construct infinite type: " ++ x ++ " ~ " ++ (prettyType t)
  show (VarNotInScope x) = "Variable not in scope: " ++ x
  show (ValConstrNotInScope x) = "Value constructor not in scope: " ++ x
  show (TypeConstrNotInScope x) = "Type constructor not in scope: " ++ x
  show (TypeVarNotInScope x) = "Type variable not in scope: " ++ x
  show (ConflictingVars x) = "Conficting definitions: " ++ x
  show (ConstrutorWrongArity s l r) = "Constructor " ++ s ++ " should have " ++ show l ++ " arguments, but has been given " ++ show r


data Scheme = ForAll [String] Type

type TCM a = StateT TcState (ReaderT TypeEnv (ExceptT TypeError (Identity))) a

data TcState = TcState {
  tcsNS :: NameSupply, -- dostawca nazw
  tcsSubst :: Subst, -- podstawienie
  constraints :: Constraints -- równania
} deriving Show

type STM a = StateT TypeEnv (ExceptT TypeError (Identity)) a

type RTM a = ReaderT TypeEnv (ExceptT TypeError (Identity)) a

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
    TVar (LIdent x) -> Map.findWithDefault t x s
    TArr l r -> TArr (s |-> l) (s |-> r)
    TNull c -> TNull c
    TPoly c ts -> TPoly c (s |-> ts)
  ftv t = case t of
    TVar (LIdent x) -> Set.singleton x
    TArr l r -> Set.union (ftv l) (ftv r)
    TNull _ -> Set.empty
    TPoly _ ts -> foldr (\x a -> Set.union (ftv x) a) Set.empty ts

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

initTypeEnv :: TypeEnv
initTypeEnv =
  TEnv { varTypes = Map.empty,-- Map.empty }
         typeConstrs = Map.fromList [("Int", 0), ("Bool", 0)] }

insertVarType :: String -> Scheme -> TypeEnv -> TypeEnv
insertVarType x s te = te { varTypes = Map.insert x s (varTypes te) }

insertTypeConstr :: String -> Arity -> TypeEnv -> TypeEnv
insertTypeConstr x a te = te { typeConstrs = Map.insert x a (typeConstrs te) }

initTcState :: TcState
initTcState = TcState {
  tcsNS = NameSupply { counter = 0 },
  tcsSubst = emptySubst,
  constraints = Seq.empty
}

int :: Type
int = TNull $ UIdent "Int"

bool :: Type
bool = TNull $ UIdent "Bool"

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
  xs' <- mapM (const freshName) xs
  let s = Map.fromList $ zip xs xs'
  return $ s |-> t

generalize :: Type -> TCM Scheme
generalize t = do
  env <- ask
  let ftvEnv = ftv $ Map.elems $ varTypes env
  let xs = Set.toList $ Set.difference (ftv t) ftvEnv
  return $ ForAll xs t  

addConstraint :: Type -> Type -> TCM ()
addConstraint typ typ' = do
  cs <- gets constraints
  modify (\state -> state { constraints = (cs Seq.|> (typ, typ')) } ) 

checkType :: Expr -> Type -> TCM Type
checkType expr typ = do
  typ' <- typeOf expr
  cs <- gets constraints
  modify (\state -> state { constraints = (cs Seq.|> (typ, typ')) } )  
  return typ'

typeOf :: Expr -> TCM Type
typeOf expr = case expr of
  EInt _ -> return int
  EBool _ -> return bool
  
  EVar (LIdent x) -> do
    found <- asks $ (Map.lookup x) . varTypes
    case found of
      Nothing -> throwError $ VarNotInScope x
      Just s -> instantiate s

  ECVar (UIdent x) -> do
    found <- asks $ (Map.lookup x) . varTypes
    case found of
      Nothing -> throwError $ ValConstrNotInScope x
      Just s -> instantiate s

  ELambda (argx@(Arg (LIdent x)):xs) expr -> do
    when (any (==argx) xs) $ throwError $ ConflictingVars x
    tv <- freshName
    let new = insertVarType x (ForAll [] tv)
    let expr' = case xs of
          [] -> expr
          _ -> ELambda xs expr
    t <- local new $ typeOf expr'
    return (TArr tv t)

  EApply lexpr rexpr -> do
    rtype <- typeOf rexpr
    tv <- freshName
    checkType lexpr (TArr rtype tv)
    return tv

  ELet decls rexpr -> case decls of
    [] -> typeOf rexpr
    d:ds -> do
      ltype <- declare d
      --s <- generalize ltype
      let rexpr' = case ds of [] -> rexpr ; _ -> ELet ds rexpr
      let x = case d of DVar (LIdent v) _ -> v ; DFunc (LIdent f) _ _ -> f
      rtype <- local (insertVarType x (ForAll [] ltype)) $ typeOf rexpr'
      return rtype

  EIfte bexpr lexpr rexpr -> do
    checkType bexpr bool
    lt <- typeOf lexpr
    checkType rexpr lt

  EOr lexpr rexpr -> checkType lexpr bool >> checkType rexpr bool >> return bool
  EAnd lexpr rexpr -> checkType lexpr bool >> checkType rexpr bool >> return bool
  EEq lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return bool
  ENeq lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return bool
  ELeq lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return bool
  ELes lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return bool
  EGre lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return bool
  EGeq lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return bool
  EAdd lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return int
  ESub lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return int
  EMul lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return int
  EDiv lexpr rexpr -> checkType lexpr int >> checkType rexpr int >> return int

  EMatch expr ((Matching pat mexpr) : matchings) -> do
    t <- typeOf expr
    env' <- checkTypePattern pat t
    mt <- local (const env') $ typeOf mexpr
    let f (Matching pat mexpr) = do
          env <- checkTypePattern pat t
          local (const env) $ checkType mexpr mt
    forM_ matchings f
    return mt

arityOf :: Type -> Arity
arityOf t = f t 0 where
  f t c = case t of
    TArr _ r -> f r (c + 1)
    _ -> c
  
checkTypePattern :: Pattern -> Type -> TCM TypeEnv
checkTypePattern pat t = case pat of
  PWildcard -> ask
  PInt _ -> addConstraint int t >> ask
  PBool _ -> addConstraint int t >> ask
  PVar (LIdent p) -> asks $ insertVarType p $ ForAll [] t
  PConstr (UIdent p) pats -> do
    found <- asks $ (Map.lookup p) . varTypes
    pt <- case found of
      Nothing -> throwError $ ValConstrNotInScope p
      Just s -> instantiate s
    let a = arityOf pt
        l = length pats
    unless (a == l) $ throwError $ ConstrutorWrongArity p a l
    let f (a, e, c) x = case a of
          TArr l r -> do
            e' <- local (const e) $ checkTypePattern x l
            return (r, e', c + 1)
    env <- ask
    (t', env', c) <- foldM f (pt, env, 0) pats
    addConstraint t t'
    return env'
    

occursCheck :: String -> Type -> Bool
occursCheck x t = Set.member x (ftv t)

bind :: String -> Type -> TCM Subst
bind x t | t == TVar (LIdent x) = return emptySubst
         | occursCheck x t = throwError $ InfiniteType x t
         | otherwise = return $ Map.singleton x t

unify :: Type -> Type -> TCM Subst
unify (TNull l) (TNull r) | l == r = return emptySubst
unify (TPoly l lts) (TPoly r rts) | l == r && length lts == length rts = unifyMany lts rts
unify (TArr ll lr) (TArr rl rr) = unifyMany [ll, lr] [rl, rr]
unify (TVar (LIdent x)) t = bind x t
unify t (TVar (LIdent x)) = bind x t
unify l r = throwError $ Mismatch l r

unifyMany :: [Type] -> [Type] -> TCM Subst
unifyMany [] [] = return emptySubst
unifyMany (hl:tl) (hr:tr) = do
  sh <- unify hl hr
  st <- unifyMany (sh |-> tl) (sh |-> tr)
  return $ st <@> sh

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
      solve

infer :: Expr -> TCM Type
infer expr = do
  t <- typeOf expr
  s <- solve
  return $ s |-> t


declare :: Decl -> TCM Type
declare decl = do
  let (x, expr) = case decl of
        DVar (LIdent v) expr -> (v, expr)
        DFunc (LIdent f) args expr' -> (f, ELambda args expr')
  tv <- freshName
  local (insertVarType x (ForAll [] tv)) $ checkType expr tv
  return tv

validateType :: Type -> RTM ()
validateType typ = do
  let notInScope x a e = case ((Map.lookup x) . typeConstrs) e of
        Nothing -> True
        Just a' -> (a /= a')
  env <- ask
  case typ of
    TArr t1 t2 -> validateType t1 >> validateType t2
    TVar (LIdent x) -> when (notInScope x (-1) env) $ throwError $ TypeVarNotInScope x
    TNull (UIdent x) -> when (notInScope x 0 env) $ throwError $ TypeConstrNotInScope x
    TPoly (UIdent x) ts -> do
      when (notInScope x (length ts) env) $ throwError $ TypeConstrNotInScope x
      forM_ ts validateType
    
addVars :: [LIdent] -> RTM TypeEnv
addVars vars = case vars of
        [] -> ask
        lh@(LIdent h):t -> do
          when (any (==lh) t) $ throwError $ ConflictingVars h
          local (insertTypeConstr h (-1)) $ addVars t

defineType :: TypeDef -> RTM [Type]
defineType (TypeDef typ vars constrs) = do
  env <- addVars vars                        
  let validateConstr (Constr (UIdent c) ts) = forM_ ts validateType
      typ' = case vars of [] -> TNull typ ; _ -> TPoly typ (map (\x->TVar x) vars)
  local (const env) $ forM_ constrs validateConstr
  return $ map (\(Constr (UIdent c) ts)->foldr TArr typ' ts) constrs

tcmToStm :: TCM a -> STM a
tcmToStm x = StateT $ \e -> fmap (\x -> (x, e)) $ runReaderT (evalStateT x initTcState) e

rtmToStm :: RTM a -> STM a
rtmToStm x = StateT $ \e -> fmap (\x -> (x, e)) $ runReaderT x e

typingInstr :: Instr -> STM [Type]
typingInstr instr = case instr of
  IType td@(TypeDef utyp@(UIdent typ) vars constrs) -> do
    modify $ insertTypeConstr typ (length vars)
    cts <- rtmToStm $ defineType td
    let typ' = case vars of [] -> TNull utyp ; _ -> TPoly utyp (map (\x->TVar x) vars)
        vars' = map (\(LIdent x)->x) vars
        cnames = map (\(Constr (UIdent cn) _)->cn) constrs
    forM_ (zip cnames cts) $ \(c, t)-> modify $ insertVarType c (ForAll vars' t)
    return cts

  IDecl decl -> do
    typ <- tcmToStm $ declare decl >>= \t -> solve >>= \s -> return $ s |-> t
    let x = case decl of DVar (LIdent v) _ -> v ; DFunc (LIdent f) _ _ -> f
    modify $ insertVarType x $ ForAll [] typ
    return [typ]
    
  IExpr expr -> do
    t <- tcmToStm $ typeOf expr >>= \t -> solve >>= \s -> return $ s |-> t
    return [t]

typingProgram :: Program -> STM [Type]
typingProgram (Prog instrs) = forM instrs typingInstr >>= \x -> return $ concat x

typing :: Program -> TypeEnv -> Either TypeError ([Type], TypeEnv)
typing p e = runIdentity $ runExceptT $ runStateT (typingProgram p) e

prettyType :: Type -> String
prettyType t = case t of
  TVar (LIdent x) -> x
  TNull (UIdent con) -> con
  TPoly (UIdent con) tps ->
    let f x = case x of
          TPoly _ _ -> "(" ++ prettyType x ++ ")"
          _ -> prettyType x
    in con ++ " " ++  (unwords $ map f tps)
  TArr (TArr t1 t2) tr ->
    "(" ++ prettyType t1 ++ " -> " ++ prettyType t2 ++ ") -> " ++ prettyType tr 
  TArr tl tr -> prettyType tl ++ " -> " ++ prettyType tr 
