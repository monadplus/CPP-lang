module CPP.JVM.CodeGenerator where

----------------------------------------

import CPP.Abs
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import CPP.JVM.Jasmin
import Lens.Micro.Platform

----------------------------------------

mainClass :: Class
mainClass = "Main"

runtimeClass :: Class
runtimeClass = "Runtime"

newtype Sig = Sig {_unSig :: Map Id Method}
  deriving newtype (Show)

newtype Ctx = Ctx {_unCtx :: Map Id Addr}
  deriving newtype (Show)

data Env = Env
  { _st :: Sig,
    _ctxs :: [(Addr, Ctx)],
    _instr :: [Instr]
  }
  deriving stock (Show)

makeLenses ''Sig
makeLenses ''Ctx
makeLenses ''Env
