{-# LANGUAGE QuantifiedConstraints #-}
module CPP.Abs where

import Data.Functor.Identity
import Data.String

import Data.Function(on)
import Data.Void

newtype Id = Id String
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (IsString)

type Typed = (,) Type

data Arg = ADecl Type Id
  deriving stock (Eq, Ord, Show, Read)

type UProgram = Program Identity
type TProgram = Program Typed
newtype Program f = PDefs [Def f]
deriving stock instance Eq UProgram
deriving stock instance Eq TProgram
deriving stock instance Ord UProgram
deriving stock instance Ord TProgram
deriving stock instance Show UProgram
deriving stock instance Show TProgram
deriving stock instance Read UProgram
deriving stock instance Read TProgram

type UDef = Def Identity
type TDef = Def Typed
data Def f = DFun Type Id [Arg] [Stm f]
deriving stock instance Eq UDef
deriving stock instance Eq TDef
deriving stock instance Ord UDef
deriving stock instance Ord TDef
deriving stock instance Show UDef
deriving stock instance Show TDef
deriving stock instance Read UDef
deriving stock instance Read TDef

toTDef :: Def Identity -> [Stm Typed] -> Def Typed
toTDef (DFun t f a _) = DFun t f a

type UStm = Stm Identity
type TStm = Stm Typed
data Stm f
  = SExp (Exp f)
  | SDecls Type [Id]
  | SInit Type Id (Exp f)
  | SReturn (Exp f)
  | SReturnVoid
  | SWhile (Exp f) (Stm f)
  | SBlock [Stm f]
  | SIfElse (Exp f) (Stm f) (Stm f)
deriving stock instance Eq (Stm Identity)
deriving stock instance Eq (Stm Typed)
deriving stock instance Ord (Stm Identity)
deriving stock instance Ord (Stm Typed)
deriving stock instance Show (Stm Identity)
deriving stock instance Show (Stm Typed)
deriving stock instance Read (Stm Identity)
deriving stock instance Read (Stm Typed)

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

type UExp = Exp Identity
type TExp = Exp Typed
type Exp f = HKD f (Exp' f)
data Exp' f
  = ETrue
  | EFalse
  | EInt Integer
  | EDouble Double
  | EString String
  | EId Id
  | EApp Id [Exp f]
  | EPIncr (Exp f)
  | EPDecr (Exp f)
  | EIncr (Exp f)
  | EDecr (Exp f)
  | ETimes (Exp f) (Exp f)
  | EDiv (Exp f) (Exp f)
  | EPlus (Exp f) (Exp f)
  | EMinus (Exp f) (Exp f)
  | ELt (Exp f) (Exp f)
  | EGt (Exp f) (Exp f)
  | ELtEq (Exp f) (Exp f)
  | EGtEq (Exp f) (Exp f)
  | EEq (Exp f) (Exp f)
  | ENEq (Exp f) (Exp f)
  | EAnd (Exp f) (Exp f)
  | EOr (Exp f) (Exp f)
  | ECast Type (Exp f)
  | EAss (Exp f) (Exp f)
  | ETyped (Exp f) Type -- TODO delete since we type using a typed AST
deriving stock instance Eq (Exp' Identity)
deriving stock instance Eq (Exp' Typed)
deriving stock instance Ord (Exp' Identity)
deriving stock instance Ord (Exp' Typed)
deriving stock instance Show (Exp' Identity)
deriving stock instance Show (Exp' Typed)
deriving stock instance Read (Exp' Identity)
deriving stock instance Read (Exp' Typed)

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

data Value
  = VUndefined
  | VVoid Void
  | VInteger Integer
  | VDouble Double
  | VString String
  deriving stock (Eq, Show, Read)

-- | Predefined functions by the language
predefinedFunctions :: [Def e]
predefinedFunctions =
  [ DFun Type_void (Id "printInt") [ADecl Type_int (Id "")] []
  , DFun Type_void (Id "printDouble") [ADecl Type_double (Id "")] []
  , DFun Type_void (Id "printString") [ADecl Type_string (Id "")] []
  , DFun Type_int (Id "readInt") [] []
  , DFun Type_double (Id "readDouble") [] []
  , DFun Type_string (Id "readString") [] []
  ]
