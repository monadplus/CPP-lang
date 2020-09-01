module CPP
  ( compileFile,
    compile,
    parseProgram,
  )
where

--------------------------------------------------------------

import CPP.Abs (Program (..))
import Control.Monad ((<=<))
import CPP.Error (CPPErr (..), prettyPrintError)
import qualified CPP.Lex as Lexer
import qualified CPP.Par as Parser
import System.Exit (exitFailure)
import qualified CPP.TypeChecker as TypeChecker

--------------------------------------------------------------

compileFile :: FilePath -> IO ()
compileFile = compile <=< readFile

parseProgram :: String -> Either String Program
parseProgram = Parser.pProgram . Lexer.tokens

compile :: String -> IO ()
compile str =
  case parseProgram str of
    Left err -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Right prog -> do
      print prog
      case TypeChecker.typeCheck prog of
        Left err -> do
          putStrLn "TYPE ERROR"
          putStrLn (prettyPrintError (TypeCheckerError err))
          exitFailure
        Right _ -> putStrLn "OK"
