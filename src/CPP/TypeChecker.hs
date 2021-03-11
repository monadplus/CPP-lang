module CPP.TypeChecker
  ( typeCheck,
  )
where

----------------------------------------------

import CPP.Abs
import CPP.Error
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Monoid (First (..))
import Lens.Micro.Platform

----------------------------------------------

data Env = Env
  { _sig :: Sig,
    _ctxs :: [Ctx]
  }

newEnv :: Env
newEnv = Env (Sig Map.empty) []

data FunTypes = FunTypes {_argTypes :: [Type], _retType :: Type}

-- | Top-level functions declaration
newtype Sig = Sig {_unSig :: Map Id FunTypes}

-- | Var declarations
-- Grouped by blocks.
-- Ordered from inner to outer block.
newtype Ctx = Ctx {_unCtx :: Map Id Type}

newCtx :: Ctx
newCtx = Ctx Map.empty

data BlockCtx = BlockCtx {_blockFun :: (Id, Type)}

makeLenses ''Env

makeLenses ''FunTypes

makeLenses ''Sig

makeLenses ''Ctx

makeLenses ''BlockCtx

newtype Check a = Check {runCheck :: StateT Env (Except TCErr) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState Env,
      MonadError TCErr
    )

-- TODO split in LookUp/Update
class (Monad m, MonadError TCErr m) => MonadEnv m where
  lookupVar :: Id -> m (Maybe Type)
  lookupFun :: Id -> m (Maybe FunTypes)
  updateVar :: Id -> Type -> m ()
  updateFun :: Id -> FunTypes -> m ()
  withNewBlock :: m a -> m a

instance MonadEnv Check where
  lookupVar var_name =
    gets $ \Env {..} ->
      getFirst $ foldMap (First . Map.lookup var_name . _unCtx) _ctxs

  lookupFun fun_name = use (sig . unSig . at fun_name)

  updateVar var_name ty = do
    env <- get
    newCtxs <- case env ^. ctxs of
      [] -> error "Ooops! Empty context?"
      (Ctx ctx : rest) -> case Map.lookup var_name ctx of
        Just _ -> throwError (VarAlreadyDeclared var_name)
        Nothing -> return (Ctx (Map.insert var_name ty ctx) : rest)
    ctxs .= newCtxs

  updateFun fun_name funTy = do
    env <- get
    when (isJust $ env ^. sig . unSig . at fun_name) $
      throwError (FunAlreadyDeclared fun_name)
    sig . unSig %= Map.insert fun_name funTy

  withNewBlock action = do
    ctxs %= (newCtx :)
    r <- action
    ctxs %= view _tail
    return r

data OverloadingOpt = AllowOv | DisallowOv

-- | Type-checks an expression and returns its type.
checkInferExpr :: MonadEnv m => Exp -> m Type
checkInferExpr = \case
  ETrue -> return Type_bool
  EFalse -> return Type_bool
  EInt _ -> return Type_int
  EDouble _ -> return Type_double
  EString _ -> return Type_string
  EId var_name -> do
    maybeTy <- lookupVar var_name
    case maybeTy of
      Nothing -> throwError (EVarNotDecl var_name)
      Just ty -> return ty
  EApp fun_name args -> do
    maybeFunTy <- lookupFun fun_name
    case maybeFunTy of
      Nothing -> throwError (EFunNotDecl fun_name)
      Just fun -> do
        let nArgsExpected = fun ^. argTypes . to length
            nArgsFound = length args
        when (nArgsExpected /= nArgsFound) $ throwError (EFunNotEnoughArgs fun_name nArgsExpected nArgsFound)
        argTypesFound <- traverse checkInferExpr args
        let argTypesExpected = fun ^. argTypes
        when (argTypesExpected /= argTypesFound) $ throwError (EFunArgTypesMismatch fun_name argTypesExpected argTypesFound)
        return $ fun ^. retType
  EPIncr expr -> checkIncrDecr expr
  EPDecr expr -> checkIncrDecr expr
  EIncr expr -> checkIncrDecr expr
  EDecr expr -> checkIncrDecr expr
  ETimes e1 e2 -> checkOp AllowOv [Type_int, Type_double] e1 e2
  EDiv e1 e2 -> checkOp AllowOv [Type_int, Type_double] e1 e2
  EPlus e1 e2 -> checkOp AllowOv [Type_string, Type_int, Type_double] e1 e2
  EMinus e1 e2 -> checkOp AllowOv [Type_int, Type_double] e1 e2
  ELt e1 e2 -> checkCmp e1 e2
  EGt e1 e2 -> checkCmp e1 e2
  ELtEq e1 e2 -> checkCmp e1 e2
  EGtEq e1 e2 -> checkCmp e1 e2
  EEq e1 e2 -> checkCmp e1 e2
  ENEq e1 e2 -> checkCmp e1 e2
  EAnd e1 e2 -> checkAndOr e1 e2
  EOr e1 e2 -> checkAndOr e1 e2
  EAss e1 e2 -> do
    var_name <- checkIsVar e1 EAssNotAVar
    maybeTy <- lookupVar var_name
    case maybeTy of
      Nothing -> throwError (EVarNotDecl var_name)
      Just tyExpected -> do
        tyFound <- checkInferExpr e2
        when (tyFound /= tyExpected) $ throwError (EAssTypeMismatch var_name tyExpected tyFound)
        return tyExpected
  ETyped expr tyCast -> do
    -- Assume: int < double < string
    exprTy <- checkInferExpr expr
    -- Down-casting is dangerous: double to int losses information.
    when (tyCast < exprTy) $ throwError (EDownCasting exprTy tyCast)
    return tyCast
  where
    -- Check if the expr is a variable (i.e. EId) or throws the given error.
    checkIsVar (EId var_name) _ = return var_name
    checkIsVar _ e = throwError e

    checkIncrDecr e = do
      _ <- checkIsVar e EIncrDecrExprNotAVar
      ty <- checkInferExpr e
      unless (validateTypes [Type_int , Type_double] [ty]) $
        throwError EIncrDecrExprNotNumerical
      return ty

    checkOp opt valid_types e1 e2 = do
      ty1 <- checkInferExpr e1
      ty2 <- checkInferExpr e2
      unless (validateTypes valid_types [ty1, ty2]) $ throwError EOpInvalidTypes
      case opt of
        AllowOv ->
          return $ max ty1 ty2
        DisallowOv -> do
          when (ty1 /= ty2) $ throwError EOpNotSameTypes
          return ty1
    -- Check comparison and (in)equality
    checkCmp e1 e2 = do
      let valid_types = [Type_bool, Type_int, Type_double, Type_string]
      _ <- checkOp DisallowOv valid_types e1 e2
      return Type_bool
    -- Conjunction and disjunction
    checkAndOr e1 e2 = do
      let valid_types = [Type_bool]
      _ <- checkOp DisallowOv valid_types e1 e2
      return Type_bool

-- | Checks if all the elements of the second list are included
-- in the first list.
validateTypes :: Eq a => [a] -> [a] -> Bool
validateTypes x = all (`elem` x)

-- | Type-checks an statement and returns its type.
--
-- TODO I REALLY dislike having an explicit return ctx.
checkStm :: MonadEnv m => BlockCtx -> Stm -> m ()
checkStm ctx = \case
  SExp expr ->
    void $ checkInferExpr expr
  SDecls ty var_names ->
    traverse_ (`updateVar` ty) var_names
  SReturn expr -> do
    tyFound <- checkInferExpr expr
    let (fun_name, tyExpected) = ctx ^. blockFun
    when (tyFound /= tyExpected) $ throwError (ReturnTypeMismatch fun_name tyExpected tyFound)
    return ()
  SReturnVoid -> do
    let (fun_name, tyExpected) = ctx ^. blockFun
    when (tyExpected /= Type_void) $ throwError (ReturnTypeMismatch fun_name tyExpected Type_void)
    return ()
  SInit ty var_name expr -> do
    exprTy <- checkInferExpr expr
    when (ty /= exprTy) $ throwError (SInitTypeMismatch var_name ty exprTy)
    updateVar var_name ty
  SWhile expr stm -> do
    exprTy <- checkInferExpr expr
    when (exprTy /= Type_bool) $ throwError (SWhileConditionIsNotBool exprTy)
    checkStm ctx stm
  SBlock stms ->
    withNewBlock (traverse_ (checkStm ctx) stms)
  SIfElse expr if' else' -> do
    exprTy <- checkInferExpr expr
    when (exprTy /= Type_bool) $ throwError (SIfElseConditionIsNotBool exprTy)
    traverse_ (checkStm ctx) [if', else']

-- | Checks if the function has at least a return statement.
checkReturns :: MonadEnv m => Def -> m ()
checkReturns (DFun _ fun_name _ stms) = do
  if any hasReturn stms
    then return ()
    else throwError (ReturnStmMissing fun_name)
  where
    -- Looks up for a "return" statement.
    hasReturn :: Stm -> Bool
    hasReturn (SReturn _) = True
    hasReturn SReturnVoid = True
    hasReturn (SWhile _ stm) = hasReturn stm
    hasReturn (SBlock statements) = any hasReturn statements
    hasReturn (SIfElse _ if' else') = hasReturn if' || hasReturn else'
    hasReturn _ = False

checkDef :: MonadEnv m => Def -> m ()
checkDef fun@(DFun tyFun fun_name args stms) = withNewBlock $ do
  addParamsToEnv args
  traverse_ (checkStm $ BlockCtx (fun_name, tyFun)) stms
  checkReturns fun
  where
    -- If a parameter is declared multiple times, this method will return an error.
    addParamsToEnv = traverse_ (\(ADecl ty var_name) -> updateVar var_name ty)

-- | Adds 'FunTypes' to 'Env' for all function declarations found in the program.
addFunsToEnv :: MonadEnv m => Program -> m ()
addFunsToEnv (PDefs defs) =
  forM_ defs $ \(DFun retType' name args _) -> do
    let argTypes' = map getArgType args
    updateFun name (FunTypes argTypes' retType')
  where
    getArgType :: Arg -> Type
    getArgType (ADecl ty _) = ty

-- | Given a CPP program, type-checks it and return the first type error found.
--
-- TODO return a typed-AST
typeCheck :: Program -> Either TCErr ()
typeCheck prog@(PDefs defs) =
  runExcept $ evalStateT (runCheck typeCheckProg) newEnv
  where
    typeCheckProg = do
      addFunsToEnv prog
      traverse_ checkDef defs
