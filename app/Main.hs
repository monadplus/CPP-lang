module Main (main) where

--------------------------------------------------------------

import qualified CPP
import System.Environment (getArgs)
import System.Exit (exitFailure)

--------------------------------------------------------------

-- TODO replace with optparse-applicative
main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFilePath] ->
      -- Default
      CPP.runInterpreter sourceFilePath
    [sourceFilePath, "--interpreter"] ->
      CPP.runInterpreter sourceFilePath
    [sourceFilePath, "--jvm"] ->
      CPP.compileJAR sourceFilePath
    _ -> do
      putStrLn "Usage: cpp <program.cpp> <options>"
      putStrLn "<options>: --interpreter"
      putStrLn "           --jvm"
      exitFailure
