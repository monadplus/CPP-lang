module TypeCheckerSpec
  ( typeCheckerSpec,
  )
where

import qualified CPP
import CPP.Error
import qualified CPP.TypeChecker as TypeChecker
import Control.Monad
import Data.Either
import System.FilePath
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Expectations
import Util(getCPPFilePath)
import Data.Foldable(for_)

---------------------------------------------------
{- TODO

- Test should check the compilation error message..otherwise they could be failing for
  another reason shadowing a bug in the type checker.
-}
---------------------------------------------------

typeCheckingDirectory :: FilePath
typeCheckingDirectory = "test/type-checking"

typeCheckerSpec :: Spec
typeCheckerSpec = describe "Typechecker" $ do
  goodProgramSpec
  badProgramSpec

goodProgramSpec :: Spec
goodProgramSpec = describe "Good programs" $ do
  fps <- runIO $ getCPPFilePath (typeCheckingDirectory </> "good")
  for_ fps $ \fp ->
    it (show fp) $
      typeCheckFile fp (`shouldSatisfy` isRight)

badProgramSpec :: Spec
badProgramSpec = describe "Bad programs" $ do
  fps <- runIO $ getCPPFilePath (typeCheckingDirectory </> "bad")
  forM_ fps $ \fp ->
    it (show fp) $
      typeCheckFile fp (`shouldSatisfy` isLeft)

typeCheckFile :: FilePath -> (forall a. Show a => Either TCErr a -> Expectation) -> Expectation
typeCheckFile fp onResult = do
  str <- readFile fp
  case CPP.parseProgram str of
    Left err -> expectationFailure err
    Right prog -> do
      onResult (TypeChecker.typeCheck prog)
