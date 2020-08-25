module Main where

--------------------------------------------------------------

import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import qualified CPP

--------------------------------------------------------------

main :: IO ()
main = do args <- getArgs
          case args of
            [fp] -> CPP.compileFile fp
            _      -> do putStrLn "Usage: cpp <SourceFile>"
                         exitFailure
