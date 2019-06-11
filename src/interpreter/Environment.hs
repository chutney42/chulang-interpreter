module Environment where

import qualified Data.Map as M
import Data.Either

import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity

import AbsGrammar


type InterpretError = String

type LazyValue = (Env, Expr)

type LazyValueEnv = M.Map String LazyValue

type TypeEnv = M.Map String Type

data Env = Env { lazyValueEnv :: LazyValueEnv,
                 typeEnv :: TypeEnv }
  deriving (Eq, Ord, Read)

insertLazyValue :: String -> LazyValue -> Env -> Env
insertLazyValue s v env = env { lazyValueEnv = M.insert s v (lazyValueEnv env) }

lookupLazyValue :: String -> Env -> Maybe LazyValue
lookupLazyValue x = (M.lookup x) . lazyValueEnv

insertType :: String -> Type -> Env -> Env
insertType s v env = env { typeEnv = M.insert s v (typeEnv env) }

lookupType :: String -> Env -> Maybe Type
lookupType x = (M.lookup x) . typeEnv

initEnv :: Env
initEnv = Env { lazyValueEnv = M.empty, typeEnv = M.empty }

lookupByName :: Error b => String -> (String -> Env -> Maybe a) -> (String -> b) -> ReaderT Env (ErrorT b Identity) a
lookupByName x lookupFunc notFoundMessage = do
  found <- asks $ lookupFunc x
  case found of
    Just tx -> return tx
    Nothing -> lift $ throwError $ notFoundMessage x

--prepareFunc :: String -> [Arg] -> Expr -> Expr
--prepareFunc f args expr = case args of
--  [] -> expr
--  (x:xs) -> ELambda x xs expr

--prepareConstr :: Constr -> (String, Expr)
--prepareConstr (Constr ic@(UIdent c) p) = (c, prepareFunc c args expr)
--  where n = length p
--        pom = map (\a-> LIdent ('x':(show a))) [0..(n - 1)]
--        args = map (\(a, t)-> Arg a t) $ zip pom p
--        vars = map (EVar) pom
--        expr = EData ic vars
