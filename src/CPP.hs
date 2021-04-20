module CPP
  ( runInterpreter,
    compile,
    parseProgram,
  )
where

--------------------------------------------------------------

import CPP.Abs (UProgram)
import Control.Monad ((<=<))
import CPP.Error (CPPErr (..), prettyPrintError)
import qualified CPP.Lex as Lexer
import qualified CPP.Par as Parser
import System.Exit (exitFailure, exitSuccess)
import qualified CPP.TypeChecker as TypeChecker
import qualified CPP.Interpreter as Interpreter

--------------------------------------------------------------

parseProgram :: String -> Either String UProgram
parseProgram = Parser.pProgram . Lexer.tokens

compile :: String -> IO ()
compile str =
  case parseProgram str of
    Left err -> do
      putStrLn err
      exitFailure
    Right prog -> do
      case TypeChecker.typeCheck prog of
        Left err -> do
          putStrLn (prettyPrintError (TypeCheckerError err))
          exitFailure
        Right tProg -> do
          Interpreter.runIO tProg
          exitSuccess

runInterpreter :: FilePath -> IO ()
runInterpreter = compile <=< readFile
