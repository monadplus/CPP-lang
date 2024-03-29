module CPP
  ( compile,
    compileJAR,
    parseProgram,
    runInterpreter,
  )
where

--------------------------------------------------------------

import CPP.AST (TProgram, UProgram)
import CPP.Error (CPPErr (..), prettyPrintError)
import qualified CPP.Interpreter as Interpreter
import qualified CPP.JVM.CodeGen as JVM
import qualified CPP.Parser as Parser
import qualified CPP.TypeChecker as TypeChecker
import Control.Monad ((<=<))
import System.Exit (exitFailure, exitSuccess)
import System.FilePath

--------------------------------------------------------------

parseProgram :: String -> Either String UProgram
parseProgram = Parser.runParser Parser.parseProgram

compile :: String -> IO TProgram
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
        Right tProg ->
          return tProg

runInterpreter :: FilePath -> IO ()
runInterpreter = withAST Interpreter.runIO

compileJAR :: FilePath -> IO ()
compileJAR source = withAST (JVM.compileJAR (takeBaseName source)) source

withAST :: (TProgram -> IO ()) -> FilePath -> IO ()
withAST cont =
  const exitSuccess
    <=< cont
    <=< compile
    <=< readFile
