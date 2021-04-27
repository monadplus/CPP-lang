-- | Jasmin Assembly Language
module CPP.JVM.Jasmin where

import CPP.Abs
import Data.String
import Data.Word
import Lens.Micro.Platform
import Text.Printf

-- | Memory address in 64 bits.
newtype Addr = Addr {_unAddr :: Word64}
  deriving newtype (Show, PrintfArg)

-- | Jump label
newtype Label = Label {_unLabel :: String}
  deriving newtype (Show, IsString, PrintfArg)

-- | Class
newtype Class = Class {_unClass :: String}
  deriving newtype (Show, IsString, PrintfArg)

-- | Method
data Method = Method
  { _mClass :: Class,
    _mName :: Id,
    _mParameters :: [Type],
    _mReturn :: Type
  }
  deriving stock (Show)

type Byte = Word8

type Short = Word16

type Word = Word32

type Long = Word64

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
  | -- | Push int
    Bipush Int
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
  | DNeg
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
    IConsM1
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
  | -- | invokestatic Runtime/printString(Ljava/lang/String;)V
    Invokestatic Method
  | Invokevirtual Method
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

instance Show Instr where
  show = \case
    ALoad addr -> printf "aload %u" addr
    ALoad0 -> "aload_0"
    ALoad1 -> "aload_1"
    ALoad2 -> "aload_2"
    ALoad3 -> "aload_3"
    AReturn -> "areturn"
    AStore addr -> printf "astore %u" addr
    AStore0 -> "astore_0"
    AStore1 -> "astore_1"
    AStore2 -> "astore_2"
    AStore3 -> "astore_3"
    Bipush i -> printf "bipush %d" i
    D2i -> "d2i"
    DAdd -> "dadd"
    DCmpg -> "dcmpg"
    DCmpl -> "dcmpl"
    DConst0 -> "dconst_0"
    DConst1 -> "dconst_1"
    DDiv -> "ddiv"
    DLoad addr -> printf "dload %u" addr
    DLoad0 -> "dload_0"
    DLoad1 -> "dload_1"
    DLoad2 -> "dload_2"
    DLoad3 -> "dload_3"
    DMul -> "dmul"
    DNeg -> "dneg"
    DReturn -> "dreturn"
    DStore addr -> printf "dstore %u" addr
    DStore0 -> "dstore_0"
    DStore1 -> "dstore_1"
    DStore2 -> "dstore_2"
    DStore3 -> "dstore_3"
    DSub -> "dsub"
    Dup -> "dup"
    Dup2 -> "dup2"
    Goto label -> printf "goto %s" label
    I2d -> "i2d"
    IAdd -> "iadd"
    IConsM1 -> "iconst_m1"
    IConst0 -> "iconst_0"
    IConst1 -> "iconst_1"
    IConst2 -> "iconst_2"
    IConst3 -> "iconst_3"
    IConst4 -> "iconst_4"
    IConst5 -> "iconst_5"
    IDiv -> "idiv"
    IfICmp label op -> printf "if_icmp%s %s" (show op) label
    IfCmp label op -> printf "if%s %s" (show op) label
    IInc addr amount -> printf "iinc %u %d" addr amount
    ILoad addr -> printf "load %u" addr
    ILoad0 -> "iload_0"
    ILoad1 -> "iload_1"
    ILoad2 -> "iload_2"
    ILoad3 -> "iload_3"
    IMul -> "imul"
    INeg -> "ineg"
    --   | -- | invokestatic Runtime/printString(Ljava/lang/String;)V
    Invokestatic (Method claz funName params ret) ->
      printf "invokestatic %s/%s()%s" claz funName (concatMap type2Jasmin params) (type2Jasmin ret)
    Invokevirtual (Method claz funName params ret) ->
      printf "invokevirtual %s/%s()%s" claz funName (concatMap type2Jasmin params) (type2Jasmin ret)
    IRem -> "irem"
    IReturn -> "ireturn"
    IStore addr -> printf "istore %u" addr
    IStore0 -> "istore_0"
    IStore1 -> "istore_1"
    IStore2 -> "istore_2"
    IStore3 -> "istore_3"
    ISub -> "isub"
    Ldc i -> printf "ldc %d" i
    Ldcs str -> printf "ldc \"%s\"" str
    Ldc2w d -> printf "ldc2_w %d" d
    Nop -> "nop"
    Pop -> "pop"
    Pop2 -> "pop2"
    Return -> "return"

type2Jasmin :: Type -> String
type2Jasmin = \case
  Type_void -> "V"
  Type_bool -> "Z"
  Type_int -> "I"
  Type_double -> "D"
  Type_string -> "Ljava/lang/String;"

data Cmp = Eq | Ne | Lt | Le | Gt | Ge

instance Show Cmp where
  show Eq = "eq"
  show Ne = "ne"
  show Lt = "lt"
  show Le = "le"
  show Gt = "gt"
  show Ge = "ge"

makeLenses ''Addr
makeLenses ''Label
