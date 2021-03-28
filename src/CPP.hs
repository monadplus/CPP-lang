module CPP
  ( compileFile,
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
import System.Exit (exitFailure)
import qualified CPP.TypeChecker as TypeChecker
import qualified CPP.Interpreter as Interpreter

--------------------------------------------------------------

compileFile :: FilePath -> IO ()
compileFile = compile <=< readFile

parseProgram :: String -> Either String UProgram
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
        Right tProg ->
          Interpreter.runIO tProg
