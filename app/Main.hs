module Main  where

import System.IO
import System.Environment

import AbsGrammar
import LexGrammar
import ParGrammar
import Interpreter
import Environment
import Evaluator
import ErrM

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      contents <- getContents
      runInterpretMany $ lines contents
    [f] -> do
      handle <- openFile f ReadMode
      contents <- hGetContents handle
      runInterpretOne contents
      hClose handle
    _ -> hPutStrLn stderr "Too many arguments"
{-
parseAndRun :: String -> IO ()
parseAndRun program =
  case pProgram (myLexer program) of
    Ok p -> case interpret p of
      Right vals -> putStr $ unlines $ map showSomething $ filter isSomething vals
        where isSomething x = case x of Nothing -> False ; _ -> True
              showSomething (Just (v, t)) = show v ++ " : " ++ show t
      Left e -> hPutStrLn stderr e
    Bad e -> hPutStrLn stderr e
-}
