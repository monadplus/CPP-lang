module Test.CodeGen (codeGenSpec) where

-----------------------------------------------------

import qualified CPP
import CPP.Error
import qualified CPP.JVM.CodeGen as JVM
import CPP.JVM.Jasmin (Instr (BlankLine))
import qualified CPP.TypeChecker as T
import Data.Foldable (for_, traverse_)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations

-----------------------------------------------------

import qualified Test.CodeGen.Good01 as Good01
import qualified Test.CodeGen.Good02 as Good02
import qualified Test.CodeGen.Good03 as Good03

programs :: [(String, String, [Instr])]
programs =
  [ ("Good01", Good01.src, Good01.instrs),
    ("Good02", Good02.src, Good02.instrs),
    ("Good03", Good03.src, Good03.instrs)
  ]

-----------------------------------------------------

codeGenSpec :: Spec
codeGenSpec =
  describe "Code Generation" $ do
    goodProgramSpec

goodProgramSpec :: Spec
goodProgramSpec =
  describe "Good programs" $ do
    for_ programs $ \(name, source, expected) ->
      it name $ do
        case CPP.parseProgram source of
          Left err -> expectationFailure err
          Right ast -> do
            case T.typeCheck ast of
              Left err -> expectationFailure (prettyPrintError (TypeCheckerError err))
              Right tyAst -> do
                case JVM.compileJasmin tyAst of
                  Left err -> expectationFailure (JVM.prettyPrintErr err)
                  Right instrs -> do
                    print instrs
                    print expected
                    instrs <=> expected

(<=>) :: HasCallStack => [Instr] -> [Instr] -> Expectation
(<=>) a b = do
  let (!a', !b') = (f a, f b)
  length a' `shouldBe` length b'
  traverse_ (uncurry shouldBe) $ zip (w3 a') (w3 b')
  where
    f = filter (/= BlankLine)
    w3 xs = zip3 xs (tail xs) (tail . tail $ xs)
infixr 5 <=>
