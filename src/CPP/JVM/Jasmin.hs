{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | Jasmin Assembly Language
module CPP.JVM.Jasmin where

import CPP.Abs
import Data.Semigroup (Sum (..))
import Data.String
import Data.Word
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Lens.Micro.Platform
import Text.Printf
import Util
import Data.Foldable(foldl')

-- | Memory address in 64 bits.
newtype Addr = Addr {_unAddr :: Word64}
  deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral, PrintfArg)
  deriving (Semigroup, Monoid) via Sum Addr

addr0, addr1, addr2, addr3 :: Addr
addr0 = 0
addr1 = 1
addr2 = 2
addr3 = 3

-- | Jump label
newtype Label = Label {_unLabel :: String}
  deriving newtype (Show, Eq, Ord, IsString, PrintfArg)

-- | Class
newtype Class = Class {_unClass :: String}
  deriving newtype (Show, Eq, Ord, IsString, PrintfArg)

mainClass :: Class
mainClass = "Main"

objectClass :: Class
objectClass = "java/lang/Object"

data Specifier = Public | Private | Protected
  deriving stock (Generic)
  deriving stock (Eq, Ord)
  deriving (Show) via (CustomShow '[ 'LowerCase] Specifier)

data Modifier = Final | Static | Abstract | Transitient | Synchronized | Volatile
  deriving stock (Generic)
  deriving stock (Eq, Ord)
  deriving (Show) via (CustomShow '[ 'LowerCase] Modifier)

-- | Method
data Method = Method
  { _mClass :: Class,
    _mName :: Id,
    _mParameters :: [JasminType],
    _mReturn :: JasminType
  }
  deriving stock (Show, Eq)

type Byte = Word8

type Short = Word16

type Word = Word32

type Long = Word64

data LimitKind = Locals | Stack
  deriving stock (Generic)
  deriving stock (Eq, Ord)
  deriving (Show) via (CustomShow '[ 'LowerCase] LimitKind)

-- | Jasmin instruction set.
--
-- Some instructions are missing.
-- For more information see <http://jasmin.sourceforge.net/instructions.html>
data Instr
  = ALoad Addr
  | ALoad0
  | ALoad1
  | ALoad2
  | ALoad3
  | AReturn
  | AStore Addr
  | AStore0
  | AStore1
  | AStore2
  | AStore3
  | -- | Push byte - More efficient than ldc
    Bipush Byte
  | -- | Double to int
    D2i
  | DAdd
  | -- | (>) for doubles, returns int
    DCmpg
  | -- | (<) for doubles, returns int
    DCmpl
  | -- | Double const
    DConst0
  | DConst1
  | DDiv
  | DLoad Addr
  | DLoad0
  | DLoad1
  | DLoad2
  | DLoad3
  | DMul
  | -- | Double negation e.g. 12 => -12.0
    DNeg
  | -- | Return double from method
    DReturn
  | DStore Addr
  | DStore0
  | DStore1
  | DStore2
  | DStore3
  | DSub
  | -- | Duplicate 1 word
    Dup
  | -- | Duplicate 2 words i.e. 2 int or 1 double
    Dup2
  | Goto Label
  | -- | Int to double
    I2d
  | IAdd
  | -- | Push int -1 at the top of the stack
    IConstM1
  | IConst0
  | IConst1
  | IConst2
  | IConst3
  | IConst4
  | IConst5
  | IDiv
  | -- | Compare ints
    IfICmp Label Cmp
  | -- | Compare int with 0
    IfCmp Label Cmp
  | -- | Increment var i with bytes. For example, iinc 3 -10
    IInc Addr Int
  | ILoad Addr
  | ILoad0
  | ILoad1
  | ILoad2
  | ILoad3
  | IMul
  | INeg
  | -- | Static methods
    Invokestatic Method
  | -- | Class methods
    Invokevirtual Method
  | -- | Special
    Invokespecial Method
  | IRem
  | -- | Return int from method
    IReturn
  | IStore Addr
  | IStore0
  | IStore1
  | IStore2
  | IStore3
  | ISub
  | -- | Push int constant
    Ldc Int
  | -- | Push literal constant
    Ldcs String
  | -- | Push double constant
    Ldc2w Double
  | Nop
  | -- | Pop 1 word
    Pop
  | -- | Pop 2 words i.e. 2 int or 1 double
    Pop2
  | -- | Return void from method
    Return
  | -- | .method <specifier> <mod1, .., modn> <name>(<param1,..,paramN>)<ret>
    DeclMethod Specifier [Modifier] Method
  | -- | .end method
    EndMethod
  | -- | .class <specifier> <class>
    DeclClass Specifier Class
  | -- | .super <class>
    Super Class
  | -- | .limit locals/stack <int>
    Limit LimitKind Int
  | -- | Not an actual instruction
    BlankLine
  | -- | Adds a label before the instruction
    AddLabel Label
  deriving stock (Eq)

-- |
-- @
--   .method public static <class>/<name>(<params>)<ret>
--   .limit locals 100
--   .limit stack 100
--     <body>
--   .end
-- @
--
-- The .limit is set to an upper bound to simplify things.
withinPublicStaticMethod :: Functor f => Method -> f [Instr] -> f [Instr]
withinPublicStaticMethod method@(Method _ _ arguments _) =
  fmap (\body -> decl body ++ rlle body ++ end)
  where

    -- A function cannot finish with a label.
    -- This happens when the last statement is a conditional.
    rlle body
      | null body = body
      | otherwise =
          case last body of
            AddLabel _ -> body ++ [Nop]
            _otherwise -> body

    decl body =
      [ DeclMethod Public [Static] method,
        localLimitInstr body,
        stackLimitInstr body
      ]

    -- Main in CPP can be written without a explicit return.
    -- We artifically add one.
    end
      | method == mainMethod = [ Return, EndMethod]
      | otherwise = [ EndMethod ]

    localLimitInstr :: [Instr] -> Instr
    localLimitInstr body =
      Limit Locals $
        maximum (sum (fmap sizeOf arguments) : (instr2LocalSize <$> body))
      where
        instr2LocalSize :: Instr -> Int
        instr2LocalSize = \case
          -- 1 bytes xref
          AStore0 -> 1
          AStore1 -> 2
          AStore2 -> 3
          AStore3 -> 4
          AStore addr -> fromIntegral addr + 1
          -- 1 bytes int
          IStore0 -> 1
          IStore1 -> 2
          IStore2 -> 3
          IStore3 -> 4
          IStore addr -> fromIntegral addr + 1
          -- 2 bytes double
          DStore0 -> 2
          DStore1 -> 3
          DStore2 -> 4
          DStore3 -> 5
          DStore addr -> fromIntegral addr + 2
          _ -> 0

    stackLimitInstr :: [Instr] -> Instr
    stackLimitInstr =
      Limit Stack . fst . foldl' go (0, 0)
      where
        go :: (Int, Int) -> Instr -> (Int, Int)
        go (currMax, acc) instr =
          let !next = f instr acc
              !nextMax = max currMax next
           in (nextMax, next)

        f :: Instr -> (Int -> Int)
        f = \case
          ALoad0 -> (+) 1
          ALoad1 -> (+) 1
          ALoad2 -> (+) 1
          ALoad3 -> (+) 1
          ALoad _ -> (+) 1
          AStore0 -> (-) 1
          AStore1 -> (-) 1
          AStore2 -> (-) 1
          AStore3 -> (-) 1
          AStore _ -> (-) 1
          AReturn -> (-) 1
          ILoad0 -> (+) 1
          ILoad1 -> (+) 1
          ILoad2 -> (+) 1
          ILoad3 -> (+) 1
          ILoad _ -> (+) 1
          IStore0 -> (-) 1
          IStore1 -> (-) 1
          IStore2 -> (-) 1
          IStore3 -> (-) 1
          IStore _ -> (-) 1
          DLoad0 -> (+) 2
          DLoad1 -> (+) 2
          DLoad2 -> (+) 2
          DLoad3 -> (+) 2
          DLoad _ -> (+) 2
          DStore0 -> (-) 2
          DStore1 -> (-) 2
          DStore2 -> (-) 2
          DStore3 -> (-) 2
          DStore _ -> (-) 2
          Bipush _ -> (+) 1
          IAdd -> (-) 1
          DAdd -> (-) 2
          DCmpg -> (-) 3
          DCmpl -> (-) 3
          DConst0 -> (+) 2
          DConst1 -> (+) 2
          DDiv -> (-) 2
          DMul -> (-) 2
          DNeg -> id
          DReturn -> (-) 2
          DSub -> (-) 2
          Dup -> (+) 1
          Dup2 -> (+) 2
          Goto _ -> id
          D2i -> (-) 1
          I2d -> (+) 1
          IConstM1 -> (+) 1
          IConst0 -> (+) 1
          IConst1 -> (+) 1
          IConst2 -> (+) 1
          IConst3 -> (+) 1
          IConst4 -> (+) 1
          IConst5 -> (+) 1
          IDiv -> (-) 1
          IfICmp _ _ -> (-) 2
          IfCmp _ _ -> (-) 1
          IInc _ _ -> id
          IMul -> (-) 1
          INeg -> id
          -- Assuming the worst decrease of size.
          Invokestatic (Method _ _ args _) -> (+) 1 . (-) (length args)
          Invokevirtual (Method _ _ args _) -> (+) 1 . (-) (length args)
          Invokespecial (Method _ _ args _) -> (+) 1 . (-) (length args)
          IRem -> (-) 1
          IReturn -> (-) 1
          ISub -> (-) 1
          Ldc _ -> (+) 1
          Ldcs _ -> (+) 1
          Ldc2w _ -> (+) 2
          Nop -> id
          Pop -> (-) 1
          Pop2 -> (-) 2
          Return -> id
          DeclMethod {} -> id
          EndMethod -> id
          DeclClass {} -> id
          Super _ -> id
          Limit {} -> id
          BlankLine -> id
          AddLabel _ -> id

-- | Standard main method definition in java.
mainMethod :: Method
mainMethod = Method "" "main" [JArr JString] V

-- |
-- @
--   .method public <init>()V
--     .limit locals 1
--     .limit stack 1
--     aload_0
--     invokenonvirtual java/lang/Object/<init>()V
--     return
--   .end method
-- @
defaultConstructor :: [Instr]
defaultConstructor =
  [ DeclMethod Public [] (Method "" "<init>" [] V),
    Limit Locals 1,
    Limit Stack 1,
    ALoad0,
    Invokespecial (Method objectClass "<init>" [] V),
    Return,
    EndMethod,
    BlankLine
  ]

-- |
-- @
--   .class public Main
--   .super java/lang/Object
--
--   .method public <init>()V
--     .limit locals 100
--     .limit stack 100
--     aload_0
--     invokenonvirtual java/lang/Object/<init>()V
--     return
--   .end method
--
--   <body>
-- @
withinClass :: Functor f => Class -> f [Instr] -> f [Instr]
withinClass clazz = fmap ((classDef ++ defaultConstructor) ++)
  where
    classDef =
      [ DeclClass Public clazz,
        Super objectClass,
        BlankLine
      ]

withinMainClass :: Functor f => f [Instr] -> f [Instr]
withinMainClass = withinClass mainClass

instance Show Instr where
  show = \case
    ALoad0 -> "aload_0"
    ALoad1 -> "aload_1"
    ALoad2 -> "aload_2"
    ALoad3 -> "aload_3"
    ALoad addr -> printf "aload %u" addr
    AReturn -> "areturn"
    AStore0 -> "astore_0"
    AStore1 -> "astore_1"
    AStore2 -> "astore_2"
    AStore3 -> "astore_3"
    AStore addr -> printf "astore %u" addr
    D2i -> "d2i"
    DAdd -> "dadd"
    DCmpg -> "dcmpg"
    DCmpl -> "dcmpl"
    DDiv -> "ddiv"
    DLoad0 -> "dload_0"
    DLoad1 -> "dload_1"
    DLoad2 -> "dload_2"
    DLoad3 -> "dload_3"
    DLoad addr -> printf "dload %u" addr
    DMul -> "dmul"
    DNeg -> "dneg"
    DReturn -> "dreturn"
    DStore0 -> "dstore_0"
    DStore1 -> "dstore_1"
    DStore2 -> "dstore_2"
    DStore3 -> "dstore_3"
    DStore addr -> printf "dstore %u" addr
    DSub -> "dsub"
    Dup -> "dup"
    Dup2 -> "dup2"
    Goto label -> printf "goto %s" label
    I2d -> "i2d"
    IAdd -> "iadd"
    IDiv -> "idiv"
    IfICmp label op -> printf "if_icmp%s %s" (show op) label
    IfCmp label op -> printf "if%s %s" (show op) label
    IInc addr amount -> printf "iinc %u %d" addr amount
    ILoad0 -> "iload_0"
    ILoad1 -> "iload_1"
    ILoad2 -> "iload_2"
    ILoad3 -> "iload_3"
    ILoad addr -> printf "iload %u" addr
    IMul -> "imul"
    INeg -> "ineg"
    Invokestatic (Method claz funName params ret) ->
      printf "invokestatic %s/%s(%s)%s" claz funName (showParams params) (show ret)
    Invokevirtual (Method claz funName params ret) ->
      printf "invokevirtual %s/%s(%s)%s" claz funName (showParams params) (show ret)
    Invokespecial (Method claz funName params ret) ->
      printf "invokespecial %s/%s(%s)%s" claz funName (showParams params) (show ret)
    IRem -> "irem"
    IReturn -> "ireturn"
    IStore0 -> "istore_0"
    IStore1 -> "istore_1"
    IStore2 -> "istore_2"
    IStore3 -> "istore_3"
    IStore addr -> printf "istore %u" addr
    ISub -> "isub"
    Ldc i -> printf "ldc %d" i
    Ldcs str -> printf "ldc \"%s\"" str
    Ldc2w d -> printf "ldc2_w %d" d
    Bipush byte -> printf "bipush %d" byte
    IConstM1 -> "iconst_m1"
    IConst0 -> "iconst_0"
    IConst1 -> "iconst_1"
    IConst2 -> "iconst_2"
    IConst3 -> "iconst_3"
    IConst4 -> "iconst_4"
    IConst5 -> "iconst_5"
    DConst0 -> "dconst_0"
    DConst1 -> "dconst_1"
    Nop -> "nop"
    Pop -> "pop"
    Pop2 -> "pop2"
    Return -> "return"
    DeclClass spec clazz ->
      printf ".class %s %s" (show spec) clazz
    Super clazz -> printf ".super %s" clazz
    DeclMethod spec mods (Method _ funName params ret) ->
      printf ".method %s %s %s(%s)%s" (show spec) (showMods mods) funName (showParams params) (show ret)
    EndMethod -> ".end method"
    Limit kind limit -> printf ".limit %s %d" (show kind) limit
    BlankLine -> "\n"
    AddLabel label -> printf "%s:" label
    where
      showMods :: [Modifier] -> String
      showMods = unwords . fmap show

      showParams :: [JasminType] -> String
      showParams = concatMap show

-- | (barely) Optimize instructions.
optimize :: [Instr] -> [Instr]
optimize =
  literalsOpt
  where
    -- Translate loads/stores to their corresponding efficient version.
    literalsOpt :: [Instr] -> [Instr]
    literalsOpt = map $ \case
      ALoad addr
        | addr == addr0 -> ALoad0
        | addr == addr1 -> ALoad1
        | addr == addr2 -> ALoad2
        | addr == addr3 -> ALoad3
        | otherwise -> ALoad addr
      AStore addr
        | addr == addr0 -> AStore0
        | addr == addr1 -> AStore1
        | addr == addr2 -> AStore2
        | addr == addr3 -> AStore3
        | otherwise -> AStore addr
      DLoad addr
        | addr == addr0 -> DLoad0
        | addr == addr1 -> DLoad1
        | addr == addr2 -> DLoad2
        | addr == addr3 -> DLoad3
        | otherwise -> DLoad addr
      DStore addr
        | addr == addr0 -> DStore0
        | addr == addr1 -> DStore1
        | addr == addr2 -> DStore2
        | addr == addr3 -> DStore3
        | otherwise -> DStore addr
      ILoad addr
        | addr == addr0 -> ILoad0
        | addr == addr1 -> ILoad1
        | addr == addr2 -> ILoad2
        | addr == addr3 -> ILoad3
        | otherwise -> ILoad addr
      IStore addr
        | addr == addr0 -> IStore0
        | addr == addr1 -> IStore1
        | addr == addr2 -> IStore2
        | addr == addr3 -> IStore3
        | otherwise -> IStore addr
      Ldc i
        | i == - 1 -> IConstM1
        | i == 0 -> IConst0
        | i == 1 -> IConst1
        | i == 2 -> IConst2
        | i == 3 -> IConst3
        | i == 4 -> IConst4
        | i == 5 -> IConst5
        | i <= fromIntegral (maxBound @Byte) -> Bipush (fromIntegral i)
        | otherwise -> Ldc i
      Ldc2w d
        | d == 0.0 -> DConst0
        | d == 1.0 -> DConst1
        | otherwise -> Ldc2w d
      rest -> rest

-- Not that simple, you need to wrap it in an existential version.
-- Instances are also harder to write.
--
-- data T = S | C
--   deriving stock (Show, Ord, Eq)
--
-- data JasminType (t :: T) where
--   V :: JasminType 'S
--   Z :: JasminType 'S
--   I :: JasminType 'S
--   F :: JasminType 'S
--   D :: JasminType 'S
--   JString :: JasminType 'S
--   JArr :: JasminType 'S -> JasminType 'C

{-|
  * V ≣ Void
  * Z ≣ Bool/Bit
  * I ≣ Word32
  * L ≣ Word64
  * F ≣ Float
  * D ≣ Double
  * JString ≣ String
  * JArry a ≣ [a]
-}
data JasminType = V | Z | I | L | F | D | JString | JArr JasminType
  deriving stock (Eq, Ord)

instance Show JasminType where
  show = \case
    V -> "V"
    Z -> "Z"
    I -> "I"
    L -> "L"
    F -> "F"
    D -> "D"
    JString -> "Ljava/lang/String;"
    JArr t -> '[' : show t

-- Size of JVM types.
sizeOf :: JasminType -> Int
sizeOf = \case
  V -> 0
  Z -> 1
  I -> 1
  L -> 2
  F -> 1
  D -> 2
  JString -> 1
  -- TODO
  JArr _ -> 1

type2Jasmin :: Type -> JasminType
type2Jasmin = \case
  Type_void -> V
  Type_bool -> Z
  Type_int -> I
  Type_double -> D
  Type_string -> JString

toMethod :: (Class, TDef) -> Method
toMethod (claz, DFun ret funName args _) =
  Method claz funName (fmap type2Jasmin params) (type2Jasmin ret)
  where
    params = fmap (\(ADecl ty _) -> ty) args

data Cmp = Eq | Ne | Lt | Le | Gt | Ge
  deriving stock (Eq)

instance Show Cmp where
  show Eq = "eq"
  show Ne = "ne"
  show Lt = "lt"
  show Le = "le"
  show Gt = "gt"
  show Ge = "ge"

neg :: Cmp -> Cmp
neg = \case
  Eq -> Ne
  Ne -> Eq
  Lt -> Ge
  Le -> Gt
  Gt -> Le
  Ge -> Lt

-- Not Data.Kind.Type
class Size (s :: Type) where
  size :: Word8

instance (TypeError ('Text "Void does not have a size instance.")) => Size 'Type_void where size = undefined

instance Size 'Type_int where size = 1

instance Size 'Type_bool where size = 1

instance Size 'Type_string where size = 1

instance Size 'Type_double where size = 2

makeLenses ''Addr
makeLenses ''Label
makeLenses ''Class
makeLenses ''Method

nextAddr :: forall (s :: Type). Size s => Addr -> Addr
nextAddr = unAddr %~ (+ (fromIntegral $ size @s))
