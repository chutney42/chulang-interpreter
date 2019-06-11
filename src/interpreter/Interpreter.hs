module Interpreter ( interpret ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity

import AbsGrammar 
import Environment
--import qualified TypeChecker as T
import Evaluator

interpret :: Program -> Either InterpretError [Maybe (Value, Type)]
interpret p = runIdentity $ runErrorT $ evalStateT (interpretProgram p) initEnv


interpretProgram :: Program -> StateT Env (ErrorT InterpretError Identity) [Maybe (Value, Type)]
interpretProgram (Prog instrs) = forM instrs interpretInstr

interpretInstr :: Instr -> StateT Env (ErrorT InterpretError Identity) (Maybe (Value, Type))
interpretInstr (IDecl decl) = do
  --T.declare decl
  declare decl
  return Nothing

--interpretInstr (IType def) = do
  --T.defineType def
--  defineType def
--  return Nothing

interpretInstr (IExpr expr) = do
  --t <- StateT $ \env->(fmap (\x->(x, env)) (runReaderT (T.typeOf expr) env))
  v <- StateT $ \env->(fmap (\x->(x, env)) (runReaderT (evaluate expr) env))
  return $ Just (v, TConstr (Constr (UIdent "TODO") []))
