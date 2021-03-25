module TypeCheckerSpec
  ( typeCheckerSpec,
  )
where

import qualified CPP
import CPP.Error
import qualified CPP.TypeChecker as TypeChecker
import Control.Monad
import Data.Either
import Data.List
import System.Directory
import System.FilePath
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Expectations

---------------------------------------------------
{- TODO

- Test should check the compilation error message..otherwise they could be failing for
  another reason shadowing a bug in the type checker.
-}
---------------------------------------------------

typeCheckingDirectory :: FilePath
typeCheckingDirectory = "test/type-checking"

typeCheckerSpec :: Spec
typeCheckerSpec = describe "Type-checker" $ do
  goodProgramSpec
  badProgramSpec

goodProgramSpec :: Spec
goodProgramSpec = describe "Good programs" $ do
  fps <- runIO $ getCPPFiles (typeCheckingDirectory </> "good")
  forM_ fps $ \fp ->
    it (show fp) $
      typeCheckFile fp (`shouldSatisfy` isRight)

badProgramSpec :: Spec
badProgramSpec = describe "Bad programs" $ do
  fps <- runIO $ getCPPFiles (typeCheckingDirectory </> "bad")
  forM_ fps $ \fp ->
    it (show fp) $
      typeCheckFile fp (`shouldSatisfy` isLeft)

typeCheckFile :: FilePath -> (forall a. Show a => Either TCErr a -> Expectation) -> Expectation
typeCheckFile fp onResult = do
  str <- readFile fp
  case CPP.parseProgram str of
    Left err -> expectationFailure err
    Right prog -> do
      putStrLn ""
      onResult (TypeChecker.typeCheck prog)

getCPPFiles :: FilePath -> IO [FilePath]
getCPPFiles fp = do
  files <- filter (isSuffixOf ".cc") <$> listDirectory fp
  return $ (fp </>) <$> files
