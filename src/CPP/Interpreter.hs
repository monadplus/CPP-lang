module CPP.Interpreter
  ( runIO
  , runMock
  , newInput
  )
where

----------------------------------------------

import CPP.Abs
import CPP.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Foldable (find, for_, traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, catMaybes)
import Data.Monoid (First (..))
import Lens.Micro.Platform
import Data.Proxy
import Text.Read (readMaybe)
import Control.Monad.Identity
import Data.Char(isLower, toLower)
import Control.Monad.Except
import qualified Data.Kind as Kind
import Data.Coerce
import GHC.Stack(HasCallStack, callStack)
import System.IO (hSetBuffering, BufferMode(..), stdout)

----------------------------------------------

data Env = Env
  { _sig :: Sig,
    _ctxs :: [Ctx]
  }
  deriving stock Show

newtype Sig = Sig {_unSig :: Map Id TDef}
  deriving newtype Show

newtype Ctx = Ctx {_unCtx :: Map Id Value}
  deriving newtype Show

data InputOutput = InputOutput
  { _input :: [String] -- reads from left to right
  , _output :: [String] -- reverse order
  }

newInput :: [String] -> InputOutput
newInput input = InputOutput input []

newEnv :: Env
newEnv = Env (Sig Map.empty) []

newCtx :: Ctx
newCtx = Ctx Map.empty

makeLenses ''Env
makeLenses ''Sig
makeLenses ''Ctx
makeLenses ''InputOutput

reviewTypeChecker :: forall m a. HasCallStack  => MonadError IErr m => m a
reviewTypeChecker = throwError (TypeCheckerBogus callStack)

-- | Local update semantics i.e. catch (modify >> throw e) with discard
-- the state of the failing branch.
newtype InterpreterT m a =
  InterpreterT { runInterpreterT :: StateT Env (ExceptT IErr m) a}
    deriving newtype
      ( Functor,
        Applicative,
        Monad,
        MonadState Env,
        MonadError IErr,
        MonadConsole
      )

class Monad m => MonadEnv m where
  -- | 'CPP.Abs.SDecls'
  addVar :: Id -> m ()
  -- | 'CPP.Abs.SInit'
  initVar :: Id -> Value -> m ()
  addFun :: Id -> TDef -> m ()
  lookupVar :: Id -> m Value
  lookupFun :: Id -> m TDef
  -- | 'CPP.Abs.EAss'
  updateVar :: Id -> Value -> m ()
  withNewBlock :: m a -> m a

instance Monad m => MonadEnv (InterpreterT m) where
  addVar name =
    initVar name VUndefined

  initVar name value = do
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

  lookupVar name = do
    r <- gets $ \Env {..} ->
           getFirst $ foldMap (First . Map.lookup name . _unCtx) _ctxs
    maybe reviewTypeChecker return r

  lookupFun fun = do
    r <- use (sig . unSig . at fun)
    maybe reviewTypeChecker return r

  updateVar name value = do
    let updateFirst [] = []
        updateFirst (ctx:ctxs') =
          case ctx ^. unCtx . at name of
            Nothing -> ctx : updateFirst ctxs'
            Just _ -> (ctx & unCtx . at name ?~ value) : ctxs'
    modify (ctxs %~ updateFirst)

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
instance MonadConsole m => MonadConsole (ReaderT r m)
instance MonadConsole m => MonadConsole (ExceptT e m)

prettyValue :: Value -> String
prettyValue = \case
  VUndefined    -> "undefined"
  VVoid         -> "void"
  VBool x       -> show x
  VInteger x    -> show x
  VDouble x     -> show x
  VString str   -> str

instance MonadConsole IO where
  printConsole = putStrLn . prettyValue
  readConsole :: forall a. Read a => Proxy a -> IO (Maybe a)
  readConsole _ = do
    putStr "Write your input: "
    readMaybe @a <$> getLine

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

type MonadInterpreter :: (Kind.Type -> Kind.Type) -> Kind.Constraint
type MonadInterpreter m = (HasCallStack, MonadConsole m, MonadError IErr m, MonadEnv m)

--------------------------------------------------------------
--------------------------------------------------------------

predefinedFunctionsIds :: [Id]
predefinedFunctionsIds =
  (\(DFun _ funName _  _) -> funName) <$> predefinedFunctions

{-
This can be implemented in multiple ways each one with its pros/cons:

  1. EApp could treat equally both regular and predefined functions.
     But you still have the problem of handling predefined functions at evalDef.
     And you need to be very careful with the identifier assigned to the variable of printX
     since it may overwrite an existent one.

  2. EApp could handle predefined functions avoiding the need of having to push the parameters
     of these calls to the environment.

This implementation follows the second option.

FIXME Implementation is not required nor synchronized.
      Adding a function in 'CPP.Abs.predefinedFunctions' will break the compiler
      unless you also add an implementation here.
-}
callPredefinedFunction :: MonadInterpreter m => Id -> [TExp] -> m Value
callPredefinedFunction (unId -> funName) es =
  case span isLower funName of
    ("print", _) ->
      case es of
        [e] -> do
          v <- evalExp e
          VVoid <$ printConsole v
        _ -> reviewTypeChecker
    ("read", ty) ->
      case ty of
        "Int" -> readConsole' (Proxy @Integer) VInteger Type_int
        "Double" -> readConsole' (Proxy @Double) VDouble Type_double
        "String" -> readConsole' (Proxy @String) VString Type_string
        _ -> throwError (FunMissingImpl (coerce funName))
    _ -> throwError (FunMissingImpl (coerce funName))
  where
    -- TODO each parameter is doing the same: identifying the parameter.
    readConsole' :: forall a m. (MonadInterpreter m, Read a)
                 => Proxy a -> (a -> Value) -> Type -> m Value
    readConsole' proxy mkValue ty = do
      r <- readConsole proxy
      case r of
        Nothing -> throwError (ReadConsoleFailed ty)
        Just v -> return (mkValue v)

-- | Wraps an expression in a type cast once evaluated
-- with 'evalExp' will produce a value of the given type.
unsafeCast :: TExp -> Type -> TExp
unsafeCast e ty = (ty, ECast ty e)

withBool :: MonadInterpreter m => Value -> (Bool -> m a) -> m a
withBool v f = case v of
  VBool b -> f b
  _ -> reviewTypeChecker

withVar :: MonadInterpreter m => TExp -> (Id -> m a) -> m a
withVar e f = case e of
  (_, EId varName) -> f varName
  _ -> reviewTypeChecker

data IncrType where
  Pre :: IncrType
  Post :: IncrType
  deriving stock (Show)

data BinaryOp where
  Add  :: BinaryOp
  Sub  :: BinaryOp
  Mult :: BinaryOp
  Div  :: BinaryOp
  deriving stock (Show)

{- | Evaluates an expression.

Expressions do side-effects and return a value.

TODO The dependency between the type of the 'TExp' and the type
of the returned 'Value' is lost in the type signatures.
-}
evalExp :: MonadInterpreter m => TExp -> m Value
evalExp = \case
  (_, ETrue) ->
    return (VBool True)
  (_, EFalse) ->
    return (VBool False)
  (_, EInt i) ->
    return (VInteger i)
  (_, EDouble d) ->
    return (VDouble d)
  (_, EString str) ->
    return (VString str)
  (_, EId varName) -> do
    r <- lookupVar varName
    case r of
      VUndefined  -> throwError (UndefinedVar varName)
      v  -> return v
  (_, EApp funName es) ->
    if funName `elem` predefinedFunctionsIds
      then callPredefinedFunction funName es
    else do
      fun@(DFun _ _ args _) <- lookupFun funName
      vs <- traverse evalExp es
      -- This will create an additional block apart from the one from the function.
      withNewBlock $ do
        traverse_ addParameter (zip args vs)
        evalDef fun
  (_, EPIncr e) ->
    incrDecr e (+ 1) Post
  (_, EPDecr e) ->
    incrDecr e (\x -> x - 1) Post
  (_, EIncr e) ->
    incrDecr e (+ 1) Pre
  (_, EDecr e) ->
    incrDecr e (\x -> x - 1) Pre
  (_, ETimes e1 e2) ->
    binaryOp Mult e1 e2
  (_, EDiv e1 e2) ->
    binaryOp Div e1 e2
  (_, EPlus e1 e2) ->
    binaryOp Add e1 e2
  (_, EMinus e1 e2) ->
    binaryOp Sub e1 e2
  (_, ELt e1 e2) ->
    cmpOp (<) e1 e2
  (_, EGt e1 e2) ->
    cmpOp (>) e1 e2
  (_, ELtEq e1 e2) ->
    cmpOp (<=) e1 e2
  (_, EGtEq e1 e2) ->
    cmpOp (>=) e1 e2
  (_, EEq e1 e2) ->
    cmpOp (==) e1 e2
  (_, ENEq e1 e2) ->
    cmpOp (/=) e1 e2
  (_, EAnd e1 e2) -> do
    v1 <- evalExp e1
    withBool v1 $ \case
      False -> return (VBool False)
      True -> do
        v2 <- evalExp e2
        withBool v2 (return . VBool)
  (_, EOr e1 e2) -> do
    v1 <- evalExp e1
    withBool v1 $ \case
      True -> return (VBool True)
      False -> do
        v2 <- evalExp e2
        withBool v2 (return . VBool)
  (_, ECast ty e) -> do
    v <- evalExp e
    v `upcastTill` ty
  (_, EAss e1 e2) ->
    withVar e1 $ \varName -> do
      v <- evalExp e2
      v <$ updateVar varName v
  where
    -- Assign a value to a function parameter
    addParameter :: MonadEnv m => (Arg, Value) -> m ()
    addParameter (ADecl _ varName, v) = initVar varName v

    incrDecr :: MonadInterpreter m => TExp -> (forall a. Num a => a -> a) -> IncrType -> m Value
    incrDecr e f incrType = do
      withVar e $ \varName -> do
        r <- evalExp e
        case r of
          oldValue@(VInteger i) -> do
            let newValue = VInteger (f i)
            updateVar varName newValue
            case incrType of
              Pre -> return newValue
              Post -> return oldValue
          oldValue@(VDouble d) -> do
            let newValue = VDouble (f d)
            updateVar varName newValue
            case incrType of
              Pre -> return newValue
              Post -> return oldValue
          _ -> reviewTypeChecker

    binaryOp :: MonadInterpreter m
             => BinaryOp -> TExp -> TExp -> m Value
    binaryOp op e1@(t1, _) e2@(t2, _) = do
      let ty = max t1 t2
      v1 <- evalExp (unsafeCast e1 ty)
      v2 <- evalExp (unsafeCast e2 ty)
      case ty of
        Type_int ->
          case (v1, v2) of
            (VInteger i, VInteger i2) ->
              case op of
                Add -> return $ VInteger (i + i2)
                Sub -> return $ VInteger (i - i2)
                Mult -> return $ VInteger (i * i2)
                Div -> return $ VInteger (i `div` i2)
            _ -> reviewTypeChecker
        Type_double ->
          case (v1, v2) of
            (VDouble d, VDouble d2) ->
              case op of
                Add -> return $ VDouble (d + d2)
                Sub -> return $ VDouble (d - d2)
                Mult -> return $ VDouble (d * d2)
                Div -> return $ VDouble (d / d2)
            _ -> reviewTypeChecker
        Type_string ->
          case (v1, v2) of
            (VString str, VString str2) ->
              case op of
                Add -> return $ VString (str <> str2)
                _ -> reviewTypeChecker
            _ -> reviewTypeChecker
        _ ->
          reviewTypeChecker

    cmpOp :: MonadInterpreter m
          => (forall a . (Eq a, Ord a) => a -> a -> Bool)
          -> TExp -> TExp -> m Value
    cmpOp f e1 e2 = do
      v1 <- evalExp e1
      v2 <- evalExp e2
      let g :: forall a m. (Ord a, Monad m) => a -> a -> m Value
          g x y = return $ VBool (f x y)
      case (v1, v2) of
        (VBool b1, VBool b2) -> g b1 b2
        (VInteger i1, VInteger i2) -> g i1 i2
        (VDouble d1, VDouble d2) -> g d1 d2
        (VString str1, VString str2) -> g str1 str2
        _ -> reviewTypeChecker

    upcastTill :: (MonadError IErr m) => Value -> Type -> m Value
    upcastTill v ty
      | v `isTypeOf` ty = return v
      | otherwise = case v of
          VUndefined -> throwError CastUndefined
          VVoid -> upcastTill (VBool False) ty
          VBool b -> upcastTill (VInteger (fromIntegral $ fromEnum b)) ty
          VInteger i -> upcastTill (VDouble (fromIntegral i)) ty
          VDouble d -> upcastTill (VString (show d)) ty
          VString _ -> reviewTypeChecker

{- | Evaluates a statement.

Only "Return" statements return a value.
-}
evalStm :: MonadInterpreter m => TStm -> m (Maybe Value)
evalStm = \case
  SExp e -> do
    Nothing <$ evalExp e
  SDecls _ ids -> do
    Nothing <$ traverse addVar ids
  SInit _ varName e -> do
    v <- evalExp e
    Nothing <$ initVar varName v
  SReturn e ->
    Just <$> evalExp e
  SReturnVoid ->
    return (Just VVoid)
  SWhile cond e -> do
    let go = do
          cond' <- evalExp cond
          case cond' of
            VBool p ->
              if p
              then evalStm e >> go
              else return Nothing
            _       ->
              reviewTypeChecker
    go
  SBlock stmts -> do
    r <- withNewBlock (traverse evalStm stmts)
    case catMaybes r of
      [] -> return Nothing
      (x:_) -> return (Just x)
  SIfElse e if' else' -> do
    e' <- evalExp e
    case e' of
      VBool p ->
        if p
        then evalStm if'
        else case else' of
          EElse else'' -> evalStm else''
          EEmpty -> return Nothing
      _ ->
        reviewTypeChecker

{- | Evaluates a function definition.

Returns the value of the first return (take branches into account).

The environment should contain the input variables initialized to the value of the call.
-}
evalDef :: MonadInterpreter m => TDef -> m Value
evalDef (DFun _ _ _ stms) = do
  r <- catMaybes <$> withNewBlock (traverse evalStm stms)
  case r of
    [] -> reviewTypeChecker
    (x: _) -> return x

--------------------------------------------------------------
--------------------------------------------------------------

run :: forall m. MonadInterpreter m => TProgram -> m ()
run prog = do
  addFunsToEnv prog
  evalMain prog
    where
      addFunsToEnv :: TProgram -> m ()
      addFunsToEnv (PDefs defs) =
        for_ (defs ++ predefinedFunctions) $ \fun@(DFun _ name _ _) ->
          addFun name fun

      evalMain :: TProgram -> m ()
      evalMain (PDefs defs) =
        case find (\(DFun _ (Id name) _ _) -> fmap toLower name == "main") defs of
          Nothing -> reviewTypeChecker
          Just main@(DFun _ _ args _) ->
            case args of
              [] -> void . evalDef $ addReturn main
              _ -> reviewTypeChecker

      addReturn (DFun ty name args stmts) =
        let return0 = SReturn (Type_int, EInt 0)
         in DFun ty name args (stmts ++ [return0])

-- | Runs the program using the console for I/O.
runIO :: TProgram -> IO ()
runIO prog = do
  hSetBuffering stdout NoBuffering
  r <- runExceptT (evalStateT (runInterpreterT (run prog)) newEnv)
  case r of
    Left err -> putStrLn $ prettyPrintError (InterpreterError err)
    Right _ -> return ()

-- | Returns the interpreter mocking the I/O.
runMock :: InputOutput -> TProgram -> Either IErr [String]
runMock s prog =
  let (r, consoleOutput) = runIdentity (runConsoleT s (runExceptT (evalStateT (runInterpreterT (run prog)) newEnv)))
   in case r of
    Left err -> Left err
    Right _ -> return consoleOutput
