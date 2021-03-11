module CPP.Abs where

import Data.Function(on)

newtype Id = Id String
  deriving stock (Eq, Ord, Show, Read)

newtype Program = PDefs [Def]
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
  | ECast Type Exp
  | EAss Exp Exp
  | ETyped Exp Type
  deriving stock (Eq, Ord, Show, Read)

data Type
  = Type_void
  | Type_bool
  | Type_int
  | Type_double
  | Type_string
  deriving stock (Eq, Show, Read)

instance Ord Type where
  compare = compare `on` prio
    where
      prio :: Type -> Int
      prio = \case
        Type_void   -> 0
        Type_bool   -> 1
        Type_int    -> 2
        Type_double -> 3
        Type_string -> 4
