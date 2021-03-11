{-# LANGUAGE StarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CPP.Interpreter
  ( runIO
  , runMock
  )
where

----------------------------------------------

import CPP.Abs
import CPP.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Foldable (find, traverse_, for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Monoid (First (..))
import Lens.Micro.Platform
import Data.Proxy
import Text.Read (readMaybe)
import Data.Void
import Control.Monad.Identity
import Data.Char(toLower)

----------------------------------------------

{-
I decided not to update the Program with the information
given by the type checker and some scenarios which are actually impossible
are not reflected on the types.
-}
typeCheckerMsg :: String
typeCheckerMsg = "Ooooops! The type checker should have already checked this one..."

reviewTypeChecker :: a
reviewTypeChecker = error typeCheckerMsg

----------------------------------------------

data Env = Env
  { _sig :: Sig,
    _ctxs :: [Ctx]
  }

newtype Sig = Sig {_unSig :: Map Id Def}

newtype Ctx = Ctx {_unCtx :: Map Id Value}

data InputOutput = InputOutput
  { _input :: [String] -- reads from left to right
  , _output :: [String] -- reverse order
  }

newEnv :: Env
newEnv = Env (Sig Map.empty) []

newCtx :: Ctx
newCtx = Ctx Map.empty

makeLenses ''Env
makeLenses ''Sig
makeLenses ''Ctx
makeLenses ''InputOutput

newtype InterpreterT m a =
  InterpreterT { runInterpreterT :: StateT Env m a}
    deriving newtype
      ( Functor,
        Applicative,
        Monad,
        MonadState Env,
        MonadConsole
      )

class Monad m => MonadEnv m where
  addVar :: Id -> Value -> m () -- VUndefined if not init.
  addFun :: Id -> Def -> m ()
  lookupVar :: Id -> m (Maybe Value)
  lookupFun :: Id -> m (Maybe Def)
  updateVar :: Id -> Value -> m ()
  withNewBlock :: m a -> m a

instance Monad m => MonadEnv (InterpreterT m) where
  addVar name value = do
    env <- get
    newCtxs <- case env ^. ctxs of
      [] -> reviewTypeChecker
      (Ctx ctx : rest) -> case Map.lookup name ctx of
        Just _ -> reviewTypeChecker
        Nothing -> return (Ctx (Map.insert name value ctx) : rest)
    ctxs .= newCtxs

  addFun name fun = do
    env <- get
    when (isJust $ env ^. sig . unSig . at name) reviewTypeChecker
    sig . unSig %= Map.insert name fun

  lookupVar name =
    gets $ \Env {..} ->
      getFirst $ foldMap (First . Map.lookup name . _unCtx) _ctxs

  lookupFun fun =
    use (sig . unSig . at fun)

  updateVar name value =
    let updateFirst [] = []
        updateFirst (ctx:ctxs) =
          case ctx ^. unCtx . at name of
            Nothing -> updateFirst ctxs
            Just _ -> (ctx & unCtx . at name ?~ value) : ctxs
     in modify (ctxs %~ updateFirst)

  withNewBlock action = do
    ctxs %= (newCtx :)
    r <- action
    ctxs %= view _tail
    return r

class Monad m => MonadConsole m where
  printConsole :: Value -> m ()
  default printConsole :: (MonadTrans t, MonadConsole m', m ~ t m') => Value -> m ()
  printConsole = lift . printConsole

  readConsole :: Read a => Proxy a -> m (Maybe a)
  default readConsole :: (MonadTrans t, MonadConsole m', m ~ t m', Read a) => Proxy a -> m (Maybe a)
  readConsole = lift . readConsole

instance MonadConsole m => MonadConsole (StateT s m)
instance MonadConsole m => MonadConsole (ReaderT e m)

prettyValue :: Value -> String
prettyValue = \case
  VUndefined    -> "undefined"
  VVoid l       -> absurd l
  VInteger x    -> show x
  VDouble x     -> show x
  VString str   -> str

instance MonadConsole IO where
  printConsole = putStrLn . prettyValue
  readConsole :: forall a. Read a => Proxy a -> IO (Maybe a)
  readConsole _ = readMaybe @a <$> getLine

newtype ConsoleT m a = ConsoleT (StateT InputOutput m a)
  deriving newtype ( Functor, Applicative, Monad
                   , MonadTrans, MonadState InputOutput
                   )

runConsoleT :: Monad m => InputOutput -> ConsoleT m a -> m (a, [String])
runConsoleT s (ConsoleT m) = do
  (a, s') <- runStateT m s
  return (a, s'^.output.to reverse)

instance Monad m => MonadConsole (ConsoleT m) where
  printConsole v = modify (output %~ (prettyValue v:))
  readConsole :: forall a. Read a => Proxy a -> ConsoleT m (Maybe a)
  readConsole _ = do
    xs <- use input
    case xs of
      (x: xs') -> readMaybe @a x <$ (input .= xs')
      _  -> error "readConsole expecting an input but none is given."

--------------------------------------------------------------
--------------------------------------------------------------

-- | Predefined functions
predefinedFunctions :: [Def]
predefinedFunctions =
  [ DFun Type_void (Id "printInt") [ADecl Type_int (Id "")] []
  , DFun Type_void (Id "printDouble") [ADecl Type_double (Id "")] []
  , DFun Type_void (Id "printString") [ADecl Type_string (Id "")] []
  , DFun Type_int (Id "readInt") [] []
  , DFun Type_double (Id "readDouble") [] []
  , DFun Type_string (Id "readString") [] []
  ]

predefinedFunctionsIds :: [Id]
predefinedFunctionsIds =
  (\(DFun _ id _  _) -> id) <$> predefinedFunctions

-- | Evaluates an expression.
--
-- Expressions do side-effects and return a value.
-- TODO recall about special functions
evalExp :: (MonadConsole m, MonadEnv m) => Exp -> m Value
evalExp = undefined

-- | Evaluates a statement.
--
-- Statements do side-effects but do not return a value.
evalStm :: (MonadConsole m, MonadEnv m) => Stm -> m ()
evalStm = undefined

-- | Evaluates a function.
--
-- Returns the value of the first return (take branches into account).
evalFun :: (MonadConsole m, MonadEnv m) => Def -> m Value
evalFun = undefined

--------------------------------------------------------------
--------------------------------------------------------------

run :: (MonadConsole m, MonadEnv m) => Program -> m ()
run prog = do
  addFunsToEnv prog
  evalMain prog
    where
      addFunsToEnv :: MonadEnv m => Program -> m ()
      addFunsToEnv (PDefs defs) =
        for_ (defs ++ predefinedFunctions) $ \fun@(DFun _ name _ _) ->
          addFun name fun

      evalMain :: (MonadEnv m, MonadConsole m) => Program -> m ()
      evalMain (PDefs defs) =
        case find (\(DFun _ (Id name) _ _) -> fmap toLower name == "main") defs of
          Nothing -> error "main not found."
          Just main@(DFun _ _ args _) -> case args of
            [] -> void $ evalFun main
            _ -> error "main should not have parameters."

-- | Runs the program using the console for I/O.
runIO :: Program -> IO ()
runIO prog =
  evalStateT (runInterpreterT (run prog)) newEnv

-- | Returns the interpreter mocking the I/O.
runMock :: InputOutput -> Program -> [String]
runMock s prog =
  snd $ runIdentity (runConsoleT s (evalStateT (runInterpreterT (run prog)) newEnv))
