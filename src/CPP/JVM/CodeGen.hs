{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module CPP.JVM.CodeGen
  ( compileJasmin,
    compileJAR,
    prettyPrintErr,
  )
where

----------------------------------------

import CPP.Abs
import CPP.JVM.Jasmin
import qualified CPP.JVM.Runtime as Runtime
import Control.Exception (throwIO)
import Control.Monad.Except
import Control.Monad.State
import Counter
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Foldable
import Data.Functor.Identity
import Data.List (intercalate, intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (First (..))
import Data.String.QQ
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import Lens.Micro.Platform
import System.Directory
import System.FilePath (takeBaseName, (</>))
import System.IO (IOMode (..), withFile)
import System.Process.Typed
import Text.Printf

----------------------------------------

runtimeClass :: Class
runtimeClass = "Runtime"

-- | Functions declared on Runtime.class that are require for *internal* work.
--
-- Example:
--
-- @
-- IConst0
-- Invokestatic (jvmFunctions ! "i2s")
-- @
--
-- Users should not have access to these functions.
jvmFunctions :: Map Id Method
jvmFunctions =
  Map.fromList
    [ ("i2s", Method runtimeClass "i2s" [I] JString),
      ("i2d", Method runtimeClass "i2d" [D] JString),
      ("sadd", Method runtimeClass "sadd" [JString, JString] JString),
      ("seq", Method runtimeClass "seq" [JString, JString] Z),
      ("sne", Method runtimeClass "sne" [JString, JString] Z)
    ]

newtype Sig = Sig {_unSig :: Map Class (Map Id Method)}
  deriving newtype (Show)

newtype Ctx = Ctx {_unCtx :: Map Id Addr}
  deriving newtype (Show)

newCtx :: Ctx
newCtx = Ctx Map.empty

data Env = Env
  { _st :: Sig,
    _ctx :: [(Addr, Ctx)],
    _counter :: Counter
  }

newEnv :: Env
newEnv =
  Env (Sig Map.empty) [(addr0, Ctx Map.empty)] newCounter

makeLenses ''Sig
makeLenses ''Ctx
makeLenses ''Env

data Err
  = TypeCheckerBogus CallStack
  | CastUndefined Type Type
  | InequalityOnStringNotImplemented

prettyPrintErr :: Err -> String
prettyPrintErr = \case
  TypeCheckerBogus errCallStack -> printf "Ooooops! The type checker should have already checked this one.\n%s" (prettyCallStack errCallStack)
  CastUndefined from' to' -> printf "Undefined cast from %s to %s" (show from') (show to')
  InequalityOnStringNotImplemented -> printf "Inequalities on %s is not implemented" (show JString)

reviewTypeChecker :: forall m a. HasCallStack => MonadError Err m => m a
reviewTypeChecker = throwError (TypeCheckerBogus callStack)

newtype GenT m a = GenT {runGenT :: StateT Env (ExceptT Err m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError Err,
      MonadState Env
    )

class Monad m => MonadEnv m where
  addMethod :: Method -> m ()
  lookupMethod :: Class -> Id -> m Method
  addVar :: Type -> Id -> m ()
  lookupVar :: Id -> m Addr
  newLabel :: m Label
  withNewCtx :: m a -> m a

instance Monad m => MonadEnv (GenT m) where
  addMethod method@(Method clazz name _ _) =
    st . unSig %= (at clazz . non Map.empty . at name ?~ method)

  lookupMethod clazz name = do
    r <- use (st . unSig . at clazz)
    case r of
      Nothing -> reviewTypeChecker
      Just d -> maybe reviewTypeChecker return (d ^. at name)

  addVar ty varName =
    case ty of
      Type_void ->
        reviewTypeChecker
      Type_bool -> do
        addr <- ctx . _head . _1 <<%= (nextAddr @'Type_bool)
        ctx . _head . _2 . unCtx %= Map.insert varName addr
      Type_int -> do
        addr <- ctx . _head . _1 <<%= (nextAddr @'Type_int)
        ctx . _head . _2 . unCtx %= Map.insert varName addr
      Type_string -> do
        addr <- ctx . _head . _1 <<%= (nextAddr @'Type_string)
        ctx . _head . _2 . unCtx %= Map.insert varName addr
      Type_double -> do
        addr <- ctx . _head . _1 <<%= (nextAddr @'Type_double)
        ctx . _head . _2 . unCtx %= Map.insert varName addr

  lookupVar varName = do
    r <- gets $ \Env {..} ->
      getFirst $ foldMap (First . Map.lookup varName . _unCtx . snd) _ctx
    maybe reviewTypeChecker return r

  newLabel = do
    c <- use counter
    let (c', i) = nextCounter c
    counter .= c'
    return $ Label (printf "Label%d" i)

  withNewCtx action = do
    addr <- use (ctx . _head . _1)
    ctx %= ((addr, newCtx) :)
    r <- action
    ctx %= tail
    return r

-- | Product of type classes used on code generation.
type MonadGen m = (HasCallStack, MonadError Err m, MonadEnv m)

withVar :: MonadGen m => TExp -> (Id -> m a) -> m a
withVar e f = case e of
  (_, EId varName) -> f varName
  _ -> reviewTypeChecker

data PreOrPost = Pre | Post
  deriving stock (Show, Eq)

data IncrOrDecr = Incr | Decr
  deriving stock (Show, Eq)

data BinaryOp = Add | Sub | Mult | Div
  deriving stock (Show, Eq)

data BooleanOp = And | Or
  deriving stock (Show, Eq)

-- | Wraps an expression in a type cast once evaluated.
--
-- *Unsafe*, you have the responsibility to guarantee that this cast is safe.
unsafeCast :: TExp -> Type -> TExp
unsafeCast e ty = (ty, ECast ty e)

genLoadAddr :: MonadGen m => Type -> Addr -> m [Instr]
genLoadAddr ty addr =
  case ty of
    Type_void -> reviewTypeChecker
    Type_bool -> return [ILoad addr]
    Type_int -> return [ILoad addr]
    Type_double -> return [DLoad addr]
    Type_string -> return [ALoad addr]

genStoreAddr :: MonadGen m => Type -> Addr -> m [Instr]
genStoreAddr ty addr =
  case ty of
    Type_void -> reviewTypeChecker
    Type_bool -> return [IStore addr]
    Type_int -> return [IStore addr]
    Type_double -> return [DStore addr]
    Type_string -> return [AStore addr]

genExp :: MonadGen m => TExp -> m [Instr]
genExp = \case
  (_, EFalse) ->
    return [IConst0]
  (_, ETrue) ->
    return [IConst1]
  (_, EInt i) ->
    -- This may overflow due to resize.
    return [Ldc (fromIntegral i)]
  (_, EDouble d) ->
    return [Ldc2w d]
  (_, EString str) ->
    return [Ldcs str]
  (ty, EId varName) -> do
    addr <- lookupVar varName
    genLoadAddr ty addr
  (_, EApp name es) -> do
    -- this is a bit hacky since we are compiling a language without classes.
    method <- lookupMethod mainClass name `catchError` const (lookupMethod runtimeClass name)
    paramsInstrs <- concat <$> traverse genExp es
    return (paramsInstrs ++ [Invokestatic method])
  (_, EPIncr e) ->
    incr e Incr Post
  (_, EPDecr e) ->
    incr e Decr Post
  (_, EIncr e) ->
    incr e Incr Pre
  (_, EDecr e) ->
    incr e Decr Pre
  (_, ETimes e1 e2) ->
    binaryOp Mult e1 e2
  (_, EDiv e1 e2) ->
    binaryOp Div e1 e2
  (_, EPlus e1 e2) ->
    binaryOp Add e1 e2
  (_, EMinus e1 e2) ->
    binaryOp Sub e1 e2
  (_, ELt e1 e2) ->
    cmpOp Lt e1 e2
  (_, EGt e1 e2) ->
    cmpOp Gt e1 e2
  (_, ELtEq e1 e2) ->
    cmpOp Le e1 e2
  (_, EGtEq e1 e2) ->
    cmpOp Ge e1 e2
  (_, EEq e1 e2) ->
    cmpOp Eq e1 e2
  (_, ENEq e1 e2) ->
    cmpOp Ne e1 e2
  (_, EAnd e1 e2) ->
    booleanOp And e1 e2
  (_, EOr e1 e2) -> do
    booleanOp Or e1 e2
  (_, ECast to' e@(from', _)) -> do
    exprInstrs <- genExp e
    upcastInstrs <- genUpcast from' to'
    return (exprInstrs ++ upcastInstrs)
  (_, EAss e1@(ty, _) e2) ->
    withVar e1 $ \varName -> do
      addr <- lookupVar varName
      exprInstrs <- genExp e2
      popInstrs <-
        case ty of
          Type_void -> return []
          Type_bool -> return [Dup]
          Type_int -> return [Dup]
          Type_string -> return [Dup]
          Type_double -> return [Dup2]
      storeInstrs <- genStoreAddr ty addr
      return (exprInstrs ++ popInstrs ++ storeInstrs)
  where
    incr :: MonadGen m => TExp -> IncrOrDecr -> PreOrPost -> m [Instr]
    incr e@(ty, _) op order =
      withVar e $ \varName -> do
        addr <- lookupVar varName
        case ty of
          Type_void -> reviewTypeChecker
          Type_bool -> reviewTypeChecker
          Type_string -> reviewTypeChecker
          Type_int ->
            return $
              case order of
                Pre ->
                  [ ILoad addr,
                    IConst1,
                    if op == Incr then IAdd else ISub,
                    Dup,
                    IStore addr
                  ]
                Post ->
                  [ ILoad addr,
                    Dup,
                    IConst1,
                    if op == Incr then IAdd else ISub,
                    IStore addr
                  ]
          Type_double ->
            return $
              case order of
                Pre ->
                  [ DLoad addr,
                    DConst1,
                    if op == Incr then DAdd else DSub,
                    Dup2,
                    DStore addr
                  ]
                Post ->
                  [ DLoad addr,
                    Dup2,
                    DConst1,
                    if op == Incr then DAdd else DSub,
                    DStore addr
                  ]

    binaryOp :: MonadGen m => BinaryOp -> TExp -> TExp -> m [Instr]
    binaryOp op e1@(t1, _) e2@(t2, _) = do
      let ty = max t1 t2
      e1Instrs <- genExp (unsafeCast e1 ty)
      e2Instrs <- genExp (unsafeCast e2 ty)
      addInstrs <- case ty of
        Type_void -> reviewTypeChecker
        Type_bool -> reviewTypeChecker
        Type_int -> return $
          case op of
            Add -> [IAdd]
            Sub -> [ISub]
            Mult -> [IMul]
            Div -> [IDiv]
        Type_double -> return $
          case op of
            Add -> [DAdd]
            Sub -> [DSub]
            Mult -> [DMul]
            Div -> [DDiv]
        Type_string ->
          case op of
            Add -> return [Invokestatic (jvmFunctions Map.! "sadd")]
            _ -> reviewTypeChecker
      return $
        e1Instrs ++ e2Instrs ++ addInstrs

    -- The typechecker guarantees that the expression have the same type.
    cmpOp :: MonadGen m => Cmp -> TExp -> TExp -> m [Instr]
    cmpOp op e1@(ty, _) e2 = do
      e1Instrs <- genExp e1
      e2Instrs <- genExp e2
      trueLabel <- newLabel
      compareInstrs <-
        case ty of
          Type_string ->
            case op of
              Eq ->
                return
                  [ Invokestatic (jvmFunctions Map.! "seq"),
                    IfCmp trueLabel op
                  ]
              Ne ->
                return
                  [ Invokestatic (jvmFunctions Map.! "neq"),
                    IfCmp trueLabel op
                  ]
              _inequalities ->
                throwError InequalityOnStringNotImplemented
          Type_double ->
            case op of
              Eq -> return [DCmpl, IfICmp trueLabel Ne]
              Ne -> return [DCmpl, IfICmp trueLabel Eq]
              Lt -> return [DCmpl, IfICmp trueLabel Eq]
              Le -> return [DCmpg, IfICmp trueLabel Ne]
              Gt -> return [DCmpg, IfICmp trueLabel Eq]
              Ge -> return [DCmpl, IfICmp trueLabel Ne]
          _restOfTypes ->
            return [IfICmp trueLabel op]
      return $
        IConst1 :
        e1Instrs
          ++ e2Instrs
          ++ compareInstrs
          ++ [ Pop,
               IConst0,
               AddLabel trueLabel
             ]

    booleanOp :: MonadGen m => BooleanOp -> TExp -> TExp -> m [Instr]
    booleanOp op e1 e2 = do
      e1Instrs <- genExp e1
      e2Instrs <- genExp e2
      endLabel <- newLabel
      return $
        (if op == And then IConst0 else IConst1) :
        e1Instrs
          ++ [IfCmp endLabel (if op == And then Eq else Ne)]
          ++ e2Instrs
          ++ [IfCmp endLabel (if op == And then Eq else Ne)]
          ++ [ Pop,
               if op == And then IConst1 else IConst0,
               AddLabel endLabel
             ]

    genUpcast :: (MonadError Err m) => Type -> Type -> m [Instr]
    genUpcast from' to'
      | from' == to' = return []
      | otherwise = case from' of
        Type_void -> throwError (CastUndefined from' to')
        Type_bool -> genUpcast Type_int to'
        Type_int -> (I2d :) <$> genUpcast Type_double to'
        Type_double -> return [Invokestatic (jvmFunctions Map.! "d2s")]
        Type_string -> throwError (CastUndefined from' to')

genStm :: MonadGen m => TStm -> m [Instr]
genStm = \case
  SExp e@(ty, _) -> do
    r <- genExp e
    case ty of
      Type_void -> return r
      Type_bool -> return (r ++ [Pop])
      Type_int -> return (r ++ [Pop])
      Type_string -> return (r ++ [Pop])
      Type_double -> return (r ++ [Pop2])
  SDecls ty ids -> do
    traverse_ (addVar ty) ids
    defaultValue <- getDefaultValue ty
    let storeDefault (varName, defVal) =
          do
            addr <- lookupVar varName
            store <- genStoreAddr ty addr
            return (defVal : store)
    concat <$> traverse storeDefault (zip ids (repeat defaultValue))
  SInit ty varName e -> do
    initInstrs <- genExp e
    addVar ty varName
    addr <- lookupVar varName
    storeInstrs <- genStoreAddr ty addr
    return (initInstrs ++ storeInstrs)
  SReturn e@(ty, _) -> do
    r <- genExp e
    case ty of
      Type_void -> return (r ++ [Return])
      Type_bool -> return (r ++ [IReturn])
      Type_int -> return (r ++ [IReturn])
      Type_string -> return (r ++ [AReturn])
      Type_double -> return (r ++ [DReturn])
  SReturnVoid ->
    return [Return]
  -- Fusion of jumps (experimental)
  SWhile (_, ELt e1 e2) stm ->
    optimizedWhile (neg Lt) e1 e2 stm
  SWhile (_, EGt e1 e2) stm ->
    optimizedWhile (neg Gt) e1 e2 stm
  SWhile (_, ELtEq e1 e2) stm ->
    optimizedWhile (neg Le) e1 e2 stm
  SWhile (_, EGtEq e1 e2) stm ->
    optimizedWhile (neg Ge) e1 e2 stm
  SWhile (_, EEq e1 e2) stm ->
    optimizedWhile (neg Eq) e1 e2 stm
  SWhile (_, ENEq e1 e2) stm ->
    optimizedWhile (neg Ne) e1 e2 stm
  -- And/Or is more tricky to implement.
  SWhile cond stm -> do
    label1 <- newLabel
    label2 <- newLabel
    condInstrs <- genExp cond
    stmInstrs <- genStm stm
    return $
      [AddLabel label1]
        ++ condInstrs
        ++ [IfCmp label2 Eq]
        ++ stmInstrs
        ++ [Goto label1]
        ++ [AddLabel label2]
  SBlock stmts ->
    withNewCtx $ do
      rs <- traverse genStm stmts
      return (concat rs)
  SIfElse cond if' EEmpty -> do
    falseLabel <- newLabel
    condInstrs <- genExp cond
    ifInstrs <- genStm if'
    return $
      condInstrs
        ++ [IfCmp falseLabel Eq]
        ++ ifInstrs
        ++ [AddLabel falseLabel]
  SIfElse cond if' (EElse else') -> do
    falseLabel <- newLabel
    trueLabel <- newLabel
    condInstrs <- genExp cond
    ifInstrs <- genStm if'
    elseInstrs <- genStm else'
    return $
      condInstrs
        ++ [IfCmp falseLabel Eq]
        ++ ifInstrs
        ++ [Goto trueLabel]
        ++ [AddLabel falseLabel]
        ++ elseInstrs
        ++ [AddLabel trueLabel]
  where
    getDefaultValue :: MonadError Err m => Type -> m Instr
    getDefaultValue = \case
      Type_void -> reviewTypeChecker
      Type_bool -> return IConst0
      Type_int -> return IConst0
      Type_double -> return DConst0
      Type_string -> return (Ldcs "")

    optimizedWhile :: MonadGen m => Cmp -> TExp -> TExp -> TStm -> m [Instr]
    optimizedWhile cmp e1 e2 stm = do
      testLabel <- newLabel
      endLabel <- newLabel
      e1Instr <- genExp e1
      e2Instr <- genExp e2
      stmInstrs <- genStm stm
      return $
        [AddLabel testLabel]
          ++ e1Instr
          ++ e2Instr
          ++ [IfICmp endLabel cmp]
          ++ stmInstrs
          ++ [Goto testLabel]
          ++ [AddLabel endLabel]

genDef :: MonadGen m => (TDef, Method) -> m [Instr]
genDef (DFun _ _ args stms, method) =
  withMethod $
    withinPublicStaticMethod method $
      concat <$> traverse genStm stms
  where
    withMethod :: MonadGen m => m a -> m a
    withMethod body =
      withNewCtx $ do
        traverse_
          (\(ADecl argTy argName) -> addVar argTy argName)
          args
        body

-- | Generates the Main.class from the definition of our program.
--
-- Assume Runtime.class is callable.
compileJasmin' :: MonadGen m => TProgram -> m [Instr]
compileJasmin' (PDefs defs) = do
  addSigs
  withinMainClass $
    traverse genDef (fixMain <$> zip defs mainMethods)
      <&> optimize . intercalate [BlankLine]
  where
    mainMethods :: [Method]
    mainMethods = toMethod . (mainClass,) <$> defs

    -- Ad-hoc rewrite of main signature since CPP expects
    -- a () -> Int while Java expects [String] -> Int
    fixMain (def@(DFun ty name _ stms), method)
      | name == "main" =
        -- We do not have array types yet, so we add a type of size 1 byte.
        let mainDef = DFun ty name [ADecl Type_int "args"] stms
         in (mainDef, mainMethod)
      | otherwise =
        (def, method)

    runtimeMethods :: [Method]
    runtimeMethods = toMethod . (runtimeClass,) <$> predefinedFunctions

    addSigs :: MonadGen m => m ()
    addSigs = for_ (mainMethods ++ runtimeMethods) addMethod

compileJasmin :: TProgram -> Either Err [Instr]
compileJasmin prog =
  runIdentity (runExceptT (evalStateT (runGenT (compileJasmin' prog)) newEnv))

-- | From a list of jasmin instructions to a processable jasmin file.
toJasminByteString :: [Instr] -> ByteString
toJasminByteString = C.pack . concatMap show . intersperse BlankLine

-- | Compiles the program and generates a runnable *jar*.
--
--  1. Compile 'TProgram' to '[Instr]' using 'compileJasmin'
--  2. From '[Instr]' to 'Data.ByteString.ByteString'
--  3. Save the bytestrings to *.j temporal files.
--  4. Compile *.j files to *.class using jasmin executable.
--  5. Package *.class and manifest in jar format.
compileJAR :: String -> TProgram -> IO ()
compileJAR fileName prog = do
  validateExecutable "jasmin"
  validateExecutable "jar"
  case compileJasmin prog of
    Left err -> putStrLn $ prettyPrintErr err
    Right mainInstrs -> do
      tmpDir <- getTemporaryDirectory
      let mainFile = "Main.j"
          runtimeFile = "Runtime.j"
          manifestFile = "manifest.mf"
      withFile (tmpDir </> mainFile) WriteMode $ \h ->
        C.hPut h (toJasminByteString mainInstrs)
      withFile (tmpDir </> runtimeFile) WriteMode $ \h ->
        C.hPut h Runtime.runtimeJ
      withFile (tmpDir </> manifestFile) WriteMode $ \h ->
        C.hPut h manifest
      runProcess_ $
        setWorkingDir tmpDir (shell $ printf "jasmin %s %s" mainFile runtimeFile)
      currentDir <- getCurrentDirectory
      runProcess_ $
        setWorkingDir
          tmpDir
          ( shell $
              printf
                "jar cfm %s.jar %s %s.class %s.class"
                fileName
                manifestFile
                (takeBaseName mainFile)
                (takeBaseName runtimeFile)
          )
      runProcess_ . shell $
        printf "mv %s.jar %s.jar" (tmpDir </> fileName) (currentDir </> fileName)
      putStrLn $ printf "%s.jar created successfully!" fileName
  where
    validateExecutable :: String -> IO ()
    validateExecutable exec = do
      r <- findExecutable exec
      case r of
        Just _ -> return ()
        Nothing -> throwIO $ userError (printf "%s executable not found in PATH" exec)

    manifest :: ByteString
    manifest =
      [s|
Manifest-version: 1.0
Main-Class: Main
|]
