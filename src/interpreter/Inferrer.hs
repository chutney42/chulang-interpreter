module Inferrer where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity

import AbsGrammar
import Environment


data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope String

type TypeEnv = M.Map String Scheme

data Scheme = ForAll [TVar] Type

type TCM a = ErrorT TypeError (StateT TcState (Reader Env)) a

data TcState = TcState {
  tcsNS :: NameSupply, -- dostawca nazw
  tcsSubst :: Subst, -- podstawienie
  constraints :: Constraints -- równania
}

type Constraint = (Type, Type)
type Constraints = [Constraint]

tcmFresh :: TCM String -- daje ´swie˙z ˛a nazw˛e


type Subst = M.Map String Type

nullSubst :: Subst
nullSubst = M.empty

composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = M.map (apply s1) s2 `M.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> S.Set TVar

typeInt :: Type
typeInt = TType $ Constr (UIdent "Int") []

typeBool :: Type
typeBool = TType $ Constr (UIdent "Bool") []
