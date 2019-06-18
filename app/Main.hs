module Main  where

import System.IO
import System.Environment

import Control.Monad

import Interpreter

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      contents <- getContents
      runInterpretMany $ lines contents
    fs -> do
      handles <- mapM (\f -> openFile f ReadMode) fs
      contents <- mapM hGetContents handles
      runInterpretMany contents
      mapM_ hClose handles
