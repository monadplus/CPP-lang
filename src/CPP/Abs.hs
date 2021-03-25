module CPP.Abs where

import Data.Function(on)
import Data.Void

newtype Id = Id String
  deriving stock (Eq, Ord, Show, Read)

newtype Program = PDefs [UDef]
  deriving stock (Eq, Ord, Show, Read)

newtype TProgram = TPDefs [TDef]
  deriving stock (Eq, Ord, Show, Read)

data Def expr = DFun Type Id [Arg] [Stm expr]
  deriving stock (Eq, Ord, Show, Read)

type UDef = Def Exp
type TDef = Def TExp

toTDef :: UDef -> [Stm TExp] -> TDef
toTDef (DFun t f a _) = DFun t f a

data Arg = ADecl Type Id
  deriving stock (Eq, Ord, Show, Read)

data Stm expr
  = SExp expr
  | SDecls Type [Id]
  | SInit Type Id expr
  | SReturn expr
  | SReturnVoid
  | SWhile expr (Stm expr)
  | SBlock [Stm expr]
  | SIfElse expr (Stm expr) (Stm expr)
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
  | ETyped Exp Type -- TODO delete since we type using a typed AST
  deriving stock (Eq, Ord, Show, Read)

type TExp = (TExp', Type)
data TExp'
  = TETrue
  | TEFalse
  | TEInt Integer
  | TEDouble Double
  | TEString String
  | TEId Id
  | TEApp Id [TExp]
  | TEPIncr TExp
  | TEPDecr TExp
  | TEIncr TExp
  | TEDecr TExp
  | TETimes TExp TExp
  | TEDiv TExp TExp
  | TEPlus TExp TExp
  | TEMinus TExp TExp
  | TELt TExp TExp
  | TEGt TExp TExp
  | TELtEq TExp TExp
  | TEGtEq TExp TExp
  | TEEq TExp TExp
  | TENEq TExp TExp
  | TEAnd TExp TExp
  | TEOr TExp TExp
  | TECast Type TExp
  | TEAss TExp TExp
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

data Value
  = VUndefined
  | VVoid Void
  | VInteger Integer
  | VDouble Double
  | VString String
  deriving stock (Eq, Show, Read)

-- | Predefined functions by the language
--
-- TODO not sure how to handle this.
predefinedFunctions :: [Def e]
predefinedFunctions =
  [ DFun Type_void (Id "printInt") [ADecl Type_int (Id "")] []
  , DFun Type_void (Id "printDouble") [ADecl Type_double (Id "")] []
  , DFun Type_void (Id "printString") [ADecl Type_string (Id "")] []
  , DFun Type_int (Id "readInt") [] []
  , DFun Type_double (Id "readDouble") [] []
  , DFun Type_string (Id "readString") [] []
  ]
