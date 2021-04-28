module Main (main) where

import System.IO (hSetEncoding, stderr, stdout, utf8, hSetBuffering, BufferMode(..))
import Test.CodeGen (codeGenSpec)
import Test.Hspec (Spec, hspec)
import Test.Interpreter (interpreterSpec)
import Test.TypeChecker (typeCheckerSpec)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hspec allUnitTests

allUnitTests :: Spec
allUnitTests = do
  typeCheckerSpec
  interpreterSpec
  codeGenSpec
