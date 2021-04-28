module CPP.Error where

import CPP.Abs
import Control.Exception
import Text.Printf
import GHC.Stack

-- | The CPP error type
data CPPErr
  = TypeCheckerError TCErr
  | InterpreterError IErr
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Type Checker Error
data TCErr
  = VarAlreadyDeclared Id
  | FunAlreadyDeclared Id
  | ReturnStmMissing Id -- fun_name
  | ReturnTypeMismatch Id Type Type -- fun_name expected found
  | SInitTypeMismatch Id Type Type -- expected found
  | SWhileConditionIsNotBool Type
  | SIfElseConditionIsNotBool Type
  | EVarNotDecl Id
  | EFunNotDecl Id
  | EFunNotEnoughArgs Id Int Int -- fun_name #expected #found
  | EFunArgTypesMismatch Id [Type] [Type] -- fun_name expected found
  | EIncrDecrExprNotAVar
  | EIncrDecrExprNotNumerical
  | -- Op includes: *,/,+,-,<,>,=,!=
    EOpInvalidTypes -- E.g. string * string, bool + bool, void < void
  | EOpNotSameTypes -- E.g. double * int, bool < string
  | EAssNotAVar
  | EAssTypeMismatch Id Type Type -- var_name expected found
  | EDownCasting Type Type -- from to
  | MainNotFound
  | MainSignatureIsBogus
  deriving stock (Show)
  deriving anyclass (Exception)

data IErr
  = TypeCheckerBogus CallStack
  | UndefinedVar Id -- var_name
  | ReadConsoleFailed Type
  | FunMissingImpl Id -- fun_name
  | CastUndefined
  deriving stock (Show)
  deriving anyclass (Exception)

prettyPrintError :: CPPErr -> String
prettyPrintError = \case
  (InterpreterError err) -> case err of
    (TypeCheckerBogus errCallStack) -> printf "Ooooops! The type checker should have already checked this one.\n%s" (prettyCallStack errCallStack)
    UndefinedVar (Id varName) -> printf "Variable %s is undefined." varName
    ReadConsoleFailed expectedTy -> printf "Read console failed! A %s was expected." (show expectedTy)
    FunMissingImpl (Id funName) -> printf "Function %s is missing the implementation." funName
    CastUndefined  -> printf "Casting undefined is forbidden."

  (TypeCheckerError err) -> case err of
    VarAlreadyDeclared (Id var_name) -> printf "Variable %s already declared in this scope." var_name
    FunAlreadyDeclared (Id fun_name) -> printf "Function %s already exists." fun_name
    ReturnStmMissing (Id fun_name) -> printf "Missing \"return\" in function %s." fun_name
    ReturnTypeMismatch (Id fun_name) expected found -> printf "In function %s, the return type is %s but it was expecting a %s." fun_name (show found) (show expected)
    SInitTypeMismatch (Id var_name) expected found -> printf "Variable %s initialized with an expression of type %s but expecting %s." var_name (show expected) (show found)
    SWhileConditionIsNotBool ty -> printf "Expecting a `bool` in the \"while\" conditional but found %s." (show ty)
    SIfElseConditionIsNotBool ty -> printf "Expecting a `bool` in the \"if\" conditional but found %s." (show ty)
    EVarNotDecl (Id var_name) -> printf "Variable %s not declared in this scope." var_name
    EFunNotDecl (Id fun_name) -> printf "Fun %s does not exist." fun_name
    EFunNotEnoughArgs (Id fun_name) expected found -> printf "Function %s called with %n arguments but expecting %n arguments." fun_name found expected
    EFunArgTypesMismatch (Id fun_name) expected found -> printf "Function %s called with arguments of type %s but expecting %s." fun_name (show found) (show expected)
    EIncrDecrExprNotAVar -> printf "Increment/Decrement expecting a variable."
    EIncrDecrExprNotNumerical -> printf "Increment/Decrement on a non-numerical variable."
    EOpInvalidTypes -> printf "Operation applied to invalid types."
    EOpNotSameTypes -> printf "Operation applied to mismatched types."
    EAssNotAVar -> printf "Assigment LHS should be a variable."
    EAssTypeMismatch (Id var_name) expected found -> printf "Variable %s assigned to an expression of type %s but expecting %s." var_name (show found) (show expected)
    EDownCasting from to -> printf "Casting from %s to %s is not allowed." (show from) (show to)
    MainNotFound -> printf "Main not found."
    MainSignatureIsBogus -> printf "Main should be declared as int main(void)."
