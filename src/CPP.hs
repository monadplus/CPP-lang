module CPP
  ( compileFile
  , compile
  ) where

--------------------------------------------------------------

import           Control.Monad ((<=<))
import qualified LexCPP        as Lexer
import qualified ParCPP        as Parser
import           System.Exit   (exitFailure)
import qualified TypeChecker

--------------------------------------------------------------

compileFile :: FilePath -> IO ()
compileFile = compile <=< readFile

compile :: String -> IO ()
compile str =
  case Parser.pProgram (Lexer.tokens str) of
    Left err  -> do putStrLn "SYNTAX ERROR"
                    putStrLn err
                    exitFailure
    Right ast -> case TypeChecker.typecheck ast of
                  Left err -> do putStrLn "TYPE ERROR"
                                 putStrLn err
                                 exitFailure
                  Right _ -> do putStrLn "OK"
