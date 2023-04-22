module Test.Interpreter (interpreterSpec) where

-----------------------------------------------------

import qualified CPP
import CPP.Error
import qualified CPP.Interpreter as I
import qualified CPP.TypeChecker as T
import Data.Foldable (for_)
import System.FilePath
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Expectations
import Test.Util (ProgramFile (..), getCPPFiles, readFile')

-----------------------------------------------------

interpreterDir :: FilePath
interpreterDir = "test/resources/interpreter"

interpreterSpec :: Spec
interpreterSpec =
  describe "Interpreter" $ do
    goodProgramSpec

goodProgramSpec :: Spec
goodProgramSpec =
  describe "Good programs" $ do
    programs <- runIO $ getTestPrograms (interpreterDir </> "good")
    for_ programs $ \TestProgram {..} ->
      it name $ do
        case CPP.parse source of
          Left err -> expectationFailure err
          Right ast -> do
            case T.typeCheck ast of
              Left err -> expectationFailure (prettyPrintError (TypeCheckerError err))
              Right tyAst -> do
                case I.runMock (I.newInput input) tyAst of
                  Left err -> expectationFailure (prettyPrintError (InterpreterError err))
                  Right output -> output `shouldBe` expectedOutput

data TestProgram = TestProgram
  { name :: String,
    source :: String,
    input :: [String],
    expectedOutput :: [String]
  }

getTestPrograms :: FilePath -> IO [TestProgram]
getTestPrograms dir = do
  files <- getCPPFiles dir
  inputs <- traverse (getInput dir) files
  outputs <- traverse (getOutput dir) files
  return $
    zipWith3
      (\(ProgramFile name src) input output -> TestProgram name src input output)
      files
      inputs
      outputs

getInput :: FilePath -> ProgramFile -> IO [String]
getInput dir (ProgramFile name _) = getFileContent (dir </> name <> ".cc.input")

getOutput :: FilePath -> ProgramFile -> IO [String]
getOutput dir (ProgramFile name _) = getFileContent (dir </> name <> ".cc.output")

getFileContent :: FilePath -> IO [String]
getFileContent fp = do
  str <- readFile' fp
  return (lines str)
