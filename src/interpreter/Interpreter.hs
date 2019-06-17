module Interpreter ( runInterpretOne, runInterpretMany ) where

import Data.Maybe

import System.IO
import System.Environment

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import AbsGrammar
import LexGrammar
import ParGrammar
import ErrM

import Evaluator
import Inferrer


data Env = Env { valueEnv :: ValueEnv, typeEnv :: TypeEnv }

initEnv :: Env
initEnv = Env { valueEnv = initVEnv, typeEnv = initTypeEnv }
  
interpretOne :: String -> StateT Env IO ()
interpretOne s = case pProgram (myLexer s) of
  Bad e -> liftIO $ hPutStrLn stderr "Syntax error"
  Ok p -> do
    tenv <- gets typeEnv
    case typing p tenv of
      Left e -> liftIO $ hPrint stderr e
      Right (ts, tenv') -> do
        liftIO $ putStr $ unlines $ map prettyType ts
        venv <- gets valueEnv
        case execute p venv of
          Left e -> liftIO $ hPrint stderr e
          Right (mvals, venv') -> do
            liftIO $ putStr $ unlines $ map (show . fromJust) $ filter isJust mvals
            put $ Env venv' tenv'

interpretMany :: [String] -> StateT Env IO ()
interpretMany ls = forM_ ls interpretOne

runInterpretOne :: String -> IO ()
runInterpretOne s = evalStateT (interpretOne s) initEnv

runInterpretMany :: [String] -> IO ()
runInterpretMany ls = evalStateT (interpretMany ls) initEnv
