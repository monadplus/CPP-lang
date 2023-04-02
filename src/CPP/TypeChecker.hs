{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPP.TypeChecker
  ( typeCheck,
  )
where

----------------------------------------------

import CPP.Abs
import CPP.Error
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (find, traverse_)
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

newtype Ctx = Ctx {_unCtx :: Map Id Type}

newCtx :: Ctx
newCtx = Ctx Map.empty

newtype BlockCtx = BlockCtx {_blockFun :: (Id, Type)}

makeLenses ''Env
makeLenses ''FunTypes
makeLenses ''Sig

-- makeLenses ''Ctx
makeLenses ''BlockCtx

newtype Check a = Check {runCheck :: StateT Env (Except TCErr) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState Env,
      MonadError TCErr
    )

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

-- | Used to allow overloading in certain operations (addition, subtraction, etc)
data OverloadingOpt = AllowOv | DisallowOv

-- | Type-checks an expression and returns its type.
checkInferExpr :: MonadEnv m => UExp -> m TExp
checkInferExpr = \case
  ETrue -> return (Type_bool, ETrue)
  EFalse -> return (Type_bool, EFalse)
  EInt i -> return (Type_int, EInt i)
  EDouble d -> return (Type_double, EDouble d)
  EString str -> return (Type_string, EString str)
  EId var_name -> do
    maybeTy <- lookupVar var_name
    case maybeTy of
      Nothing -> throwError (EVarNotDecl var_name)
      Just ty -> return (ty, EId var_name)
  EApp fun_name args -> do
    maybeFunTy <- lookupFun fun_name
    case maybeFunTy of
      Nothing -> throwError (EFunNotDecl fun_name)
      Just fun -> do
        let nArgsExpected = fun ^. argTypes . to length
            nArgsFound = length args
        when (nArgsExpected /= nArgsFound) $ throwError (EFunNotEnoughArgs fun_name nArgsExpected nArgsFound)
        texprs <- traverse checkInferExpr args
        let argTypesFound = fmap fst texprs
            argTypesExpected = fun ^. argTypes
        when (argTypesExpected /= argTypesFound) $ throwError (EFunArgTypesMismatch fun_name argTypesExpected argTypesFound)
        return (fun ^. retType, EApp fun_name texprs)
  EPIncr expr -> do
    texpr@(ty, _) <- checkIncrDecr expr
    return (ty, EPIncr texpr)
  EPDecr expr -> do
    texpr@(ty, _) <- checkIncrDecr expr
    return (ty, EPDecr texpr)
  EIncr expr -> do
    texpr@(ty, _) <- checkIncrDecr expr
    return (ty, EIncr texpr)
  EDecr expr -> do
    texpr@(ty, _) <- checkIncrDecr expr
    return (ty, EDecr texpr)
  ETimes e1 e2 ->
    checkOp ETimes AllowOv [Type_int, Type_double] e1 e2
  EDiv e1 e2 ->
    checkOp EDiv AllowOv [Type_int, Type_double] e1 e2
  EPlus e1 e2 ->
    checkOp EPlus AllowOv [Type_string, Type_int, Type_double] e1 e2
  EMinus e1 e2 ->
    checkOp EMinus AllowOv [Type_int, Type_double] e1 e2
  ELt e1 e2 ->
    checkCmp ELt e1 e2
  EGt e1 e2 ->
    checkCmp EGt e1 e2
  ELtEq e1 e2 ->
    checkCmp ELtEq e1 e2
  EGtEq e1 e2 ->
    checkCmp EGtEq e1 e2
  EEq e1 e2 ->
    checkCmp EEq e1 e2
  ENEq e1 e2 ->
    checkCmp ENEq e1 e2
  EAnd e1 e2 ->
    checkAndOr EAnd e1 e2
  EOr e1 e2 ->
    checkAndOr EOr e1 e2
  EAss e1 e2 -> do
    var_name <- checkIsVar e1 EAssNotAVar
    te1 <- checkInferExpr e1
    maybeTy <- lookupVar var_name
    case maybeTy of
      Nothing -> throwError (EVarNotDecl var_name)
      Just tyExpected -> do
        te2@(ty, _) <- checkInferExpr e2
        when (ty /= tyExpected) $ throwError (EAssTypeMismatch var_name tyExpected ty)
        return (ty, EAss te1 te2)
  ECast tyCast expr -> do
    (exprTy, expr') <- checkInferExpr expr
    -- Down-casting is dangerous: double to int losses information.
    when (exprTy `isSupertype` tyCast) $ throwError (EDownCasting exprTy tyCast)
    return (tyCast, expr')
  where
    checkIsVar :: MonadError TCErr m => UExp -> TCErr -> m Id
    checkIsVar (EId var_name) _ = return var_name
    checkIsVar _ e = throwError e

    -- Incr/Decr/PIncr/PDecr
    checkIncrDecr :: MonadEnv m => UExp -> m TExp
    checkIncrDecr e = do
      _ <- checkIsVar e EIncrDecrExprNotAVar
      texpr@(ty, _) <- checkInferExpr e
      when ([ty] `notSubset` [Type_int, Type_double]) $ throwError EIncrDecrExprNotNumerical
      return texpr

    checkOp constr opt valid_types e1 e2 = do
      texpr1@(ty1, _) <- checkInferExpr e1
      texpr2@(ty2, _) <- checkInferExpr e2
      let r = constr texpr1 texpr2
      when ([ty1, ty2] `notSubset` valid_types) $ throwError EOpInvalidTypes
      case opt of
        AllowOv ->
          return (max ty1 ty2, r)
        DisallowOv -> do
          when (ty1 /= ty2) $ throwError EOpNotSameTypes
          return (ty1, r)

    -- Check comparison and (in)equality
    checkCmp constr e1 e2 = do
      let valid_types = [Type_bool, Type_int, Type_double, Type_string]
      (_, expr) <- checkOp constr DisallowOv valid_types e1 e2
      return (Type_bool, expr)
    -- Conjunction and disjunction
    checkAndOr constr e1 e2 = do
      let valid_types = [Type_bool]
      (_, expr) <- checkOp constr DisallowOv valid_types e1 e2
      return (Type_bool, expr)

-- | Checks if all the elements of the second list are included
-- in the first list.
-- subset :: (Foldable f, Eq a) => f a -> f a -> Bool
-- subset a b = all (`elem` b) a
notSubset :: (Foldable f, Eq a) => f a -> f a -> Bool
notSubset a b = any (`notElem` b) a

-- isSubtype :: Type -> Type -> Bool
-- isSubtype = (<=)

isSupertype :: Type -> Type -> Bool
isSupertype = (>)

-- | Type-checks an statement and returns its type.
--
-- TODO I REALLY dislike having an explicit return ctx.
checkStm :: MonadEnv m => BlockCtx -> UStm -> m TStm
checkStm ctx = \case
  SExp expr ->
    SExp <$> checkInferExpr expr
  SDecls ty var_names -> do
    traverse_ (`updateVar` ty) var_names
    return (SDecls ty var_names)
  SReturn expr -> do
    texpr@(ty, _) <- checkInferExpr expr
    let (fun_name, tyExpected) = ctx ^. blockFun
    when (ty /= tyExpected) $
      throwError (ReturnTypeMismatch fun_name tyExpected ty)
    return (SReturn texpr)
  SReturnVoid -> do
    let (fun_name, tyExpected) = ctx ^. blockFun
    when (tyExpected /= Type_void) $
      throwError (ReturnTypeMismatch fun_name tyExpected Type_void)
    return SReturnVoid
  SInit tyExpected var_name expr -> do
    texpr@(ty, _) <- checkInferExpr expr
    when (ty /= tyExpected) $ throwError (SInitTypeMismatch var_name tyExpected ty)
    updateVar var_name ty
    return (SInit tyExpected var_name texpr)
  SWhile expr stm -> do
    texpr@(ty, _) <- checkInferExpr expr
    when (ty /= Type_bool) $ throwError (SWhileConditionIsNotBool ty)
    tstm <- checkStm ctx stm
    return (SWhile texpr tstm)
  SBlock stms -> do
    tstms <- withNewBlock (traverse (checkStm ctx) stms)
    return (SBlock tstms)
  SIfElse expr if' else' -> do
    texpr@(ty, _) <- checkInferExpr expr
    when (ty /= Type_bool) $ throwError (SIfElseConditionIsNotBool ty)
    case else' of
      EElse else'' -> do
        (tif', telse') <- traverseOf both (checkStm ctx) (if', else'')
        return (SIfElse texpr tif' (EElse telse'))
      EEmpty ->
        (\tif -> SIfElse texpr tif EEmpty) <$> checkStm ctx if'

-- | Checks if the function has at least a return statement.
--
-- Main function does not require an explicit return (default 1).
checkReturns :: MonadEnv m => UDef -> m ()
checkReturns (DFun _ "main" _ _) = return ()
checkReturns (DFun _ fun_name _ stms) = do
  if any hasReturn stms
    then return ()
    else throwError (ReturnStmMissing fun_name)
  where
    -- Looks up for a "return" statement.
    hasReturn :: Stm e -> Bool
    hasReturn (SReturn _) = True
    hasReturn SReturnVoid = True
    hasReturn (SWhile _ stm) = hasReturn stm
    hasReturn (SBlock statements) = any hasReturn statements
    hasReturn (SIfElse _ if' (EElse else')) = hasReturn if' && hasReturn else'
    hasReturn (SIfElse _ if' EEmpty) = hasReturn if'
    hasReturn _ = False

checkDef :: MonadEnv m => UDef -> m TDef
checkDef fun@(DFun tyFun fun_name args stms) = do
  checkReturns fun
  withNewBlock $ do
    addParamsToEnv args
    tstms <- traverse (checkStm $ BlockCtx (fun_name, tyFun)) stms
    return (toTDef fun tstms)
  where
    -- If a parameter is declared multiple times, this method will return an error.
    addParamsToEnv = traverse_ (\(ADecl ty var_name) -> updateVar var_name ty)

-- | Checks if main exist and its type signature
checkMainExists :: MonadEnv m => UProgram -> m ()
checkMainExists (PDefs defs) = do
  case find (\(DFun _ name _ _) -> name == "main") defs of
    Nothing ->
      throwError MainNotFound
    Just main ->
      case main of
        -- NOTE no arguments passed to main yet.
        DFun Type_void _ [] _ -> return ()
        DFun Type_int _ [] _ -> return ()
        _ -> throwError MainSignatureIsBogus

-- | Adds 'FunTypes' to 'Env' for all function declarations found in the program.
addFunsToEnv :: MonadEnv m => UProgram -> m ()
addFunsToEnv (PDefs defs) =
  forM_ (defs ++ predefinedFunctions) $ \(DFun retType' name args _) -> do
    let getArgType (ADecl ty _) = ty
        argTypes' = map getArgType args
    updateFun name (FunTypes argTypes' retType')

-- | Given a CPP program, type-checks it and return the first type error found.
typeCheck :: UProgram -> Either TCErr TProgram
typeCheck prog@(PDefs defs) =
  runExcept $ evalStateT (runCheck typeCheckProg) newEnv
  where
    typeCheckProg = do
      checkMainExists prog
      addFunsToEnv prog
      PDefs <$> traverse checkDef defs
