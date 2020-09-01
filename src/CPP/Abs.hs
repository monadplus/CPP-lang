-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module CPP.Abs where

newtype Id = Id String
  deriving stock (Eq, Ord, Show, Read)

data Program = PDefs [Def]
  deriving stock (Eq, Ord, Show, Read)

data Def = DFun Type Id [Arg] [Stm]
  deriving stock (Eq, Ord, Show, Read)

data Arg = ADecl Type Id
  deriving stock (Eq, Ord, Show, Read)

data Stm
  = SExp Exp
  | SDecls Type [Id]
  | SInit Type Id Exp
  | SReturn Exp
  | SReturnVoid
  | SWhile Exp Stm
  | SBlock [Stm]
  | SIfElse Exp Stm Stm
  deriving stock (Eq, Ord, Show, Read)

data Exp
  = ETrue
  | EFalse
  | EInt Integer
  | EDouble Double
  | EString String
  | EId Id
  | EApp Id [Exp]
  | EPIncr Exp
  | EPDecr Exp
  | EIncr Exp
  | EDecr Exp
  | ETimes Exp Exp
  | EDiv Exp Exp
  | EPlus Exp Exp
  | EMinus Exp Exp
  | ELt Exp Exp
  | EGt Exp Exp
  | ELtEq Exp Exp
  | EGtEq Exp Exp
  | EEq Exp Exp
  | ENEq Exp Exp
  | EAnd Exp Exp
  | EOr Exp Exp
  | EAss Exp Exp
  | ETyped Exp Type
  deriving stock (Eq, Ord, Show, Read)

-- | Order matters for parameters overloading and type conversions!
data Type
  = Type_void
  | Type_bool
  | Type_int
  | Type_double
  | Type_string
  deriving stock (Eq, Ord, Show, Read)
