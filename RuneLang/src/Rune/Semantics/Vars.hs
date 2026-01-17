{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

#if defined(TESTING_EXPORT)
module Rune.Semantics.Vars (
  verifVars,
  verifScope,
  verifExpr,
  verifExprWithContext,
  mangleFuncStack,
  isGeneric,
  hasAny,
  getDefName,
  verifTopLevel,
  verifMethod,
  tryInstantiateTemplate,
  resolveReturnType,
  alreadyInstantiated,
  registerInstantiation,
  resolveCall,
  canAccessMember,
  isSelfAccess,
  verifFieldAccess,
  verifStructFieldAccess,
  lookupStructFields,
  findField,
  raiseVisibilityError,
  checkFieldVisibility,
  verifStaticMethodCall,
  verifInstanceMethodCall,
  applyInferenceToParams,
  verifStaticCall,
  verifInstanceCall,
  injectDefaultArgs,
  isStaticMethod,
  SemState(..),
  SemM,
) where
#else
module Rune.Semantics.Vars (verifVars) where
#endif

import Control.Monad (unless, when)
import Control.Monad.State.Strict

import Text.Printf (printf)

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc, inferParamType)
import Rune.Semantics.Struct (findStruct)
import Rune.Semantics.Generic (instantiate)

import Rune.Semantics.Type
  ( FuncStack
  , VarStack
  , StructStack
  , Templates
  , Stack
  )

import Rune.Semantics.Helper
  ( checkParamType
  , mangleName
  , exprType
  , assignVarType
  , checkMultipleType
  , isTypeCompatible
  , SemanticError(..)
  , formatSemanticError
  , fixSelfType
  )
import Rune.Semantics.OpType (iHTBinary)

--
-- state monad
--

data SemState = SemState
  { stFuncs        :: FuncStack               -- << known signatures
  , stTemplates    :: Templates               -- << generic functions ('any')
  , stNewDefs      :: [TopLevelDef]           -- << new functions
  , stInstantiated :: HM.HashMap String Bool  -- << cache of instantiated templates
  , stStructs      :: StructStack             -- << known structs
  , stCurrentStruct :: Maybe String           -- << current struct name (for method calls)
  }

type SemM a = StateT SemState (Either String) a

--
-- public
--

verifVars :: Program -> Either String (Program, FuncStack)
verifVars (Program n defs) = do
  -- Apply type inference to parameters before checking if generic
  let defsWithInferredTypes = map applyInferenceToParams defs
      (templatesList, concreteDefs) = List.partition isGeneric defsWithInferredTypes
      templatesMap = HM.fromList $ map (\d -> (getDefName d, d)) templatesList

  fs <- findFunc (Program n concreteDefs)
  (Program _ updatedDefs, ss) <- findStruct (Program n concreteDefs)

  let initialState = SemState
        { stFuncs = fs
        , stTemplates = templatesMap
        , stNewDefs = []
        , stInstantiated = HM.empty
        , stStructs = ss
        , stCurrentStruct = Nothing
        }

  (defs', finalState) <- runStateT (mapM verifTopLevel updatedDefs) initialState
  let allDefs = defs' <> stNewDefs finalState
      finalFuncStack = mangleFuncStack $ stFuncs finalState
  pure (Program n allDefs, finalFuncStack)

--
-- private
--
mkErrorReturn :: SourcePos -> String -> String -> String
mkErrorReturn (SourcePos file line col) expected got =
  formatSemanticError $ SemanticError file line col expected got ["function body", "control flow"]

isGeneric :: TopLevelDef -> Bool
isGeneric (DefFunction _ params ret _ _ _ _ _) = hasAny ret || any (hasAny . paramType) params
isGeneric _ = False

-- | Apply type inference to function parameters from default values
applyInferenceToParams :: TopLevelDef -> TopLevelDef
applyInferenceToParams (DefFunction name params ret body isExport visibility isStatic isAbstract) =
  DefFunction name (map inferParamType params) ret body isExport visibility isStatic isAbstract
applyInferenceToParams def = def


hasAny :: Type -> Bool
hasAny TypeAny = True
hasAny (TypeArray t) = hasAny t
hasAny _ = False


getDefName :: TopLevelDef -> String
getDefName (DefFunction n _ _ _ _ _ _ _) = n
getDefName (DefStruct n _ _ _ _) = n
getDefName (DefSomewhere {}) = ""

mangleFuncStack :: FuncStack -> FuncStack
mangleFuncStack fs = fs

getCustomTypeName :: Type -> Maybe String
getCustomTypeName (TypeCustom name) = Just name
getCustomTypeName _                 = Nothing

--
-- verif
--

verifTopLevel :: TopLevelDef -> SemM TopLevelDef
verifTopLevel (DefFunction name params r_t body isExport visibility isStatic isAbstract) = do
  let paramTypes = map paramType params
      expectedMangled = mangleName name r_t paramTypes
      -- Don't double-mangle: if name is already mangled form, use it as-is
      finalName = if name == "main" || name == expectedMangled 
                  then name 
                  else mangleName name r_t paramTypes

  let vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params
  body' <- verifScope vs body
  pure $ DefFunction finalName params r_t body' isExport visibility isStatic isAbstract

verifTopLevel (DefStruct name fields methods isAbstract extensions) = do
  methods' <- mapM (verifMethod name) methods
  pure $ DefStruct name fields methods' isAbstract extensions

verifTopLevel def = pure def -- Somewhere

-- | scope verification
-- NOTE: 'FuncStack' is read from State, 'VarStack' is passed locally
verifScope :: VarStack -> Block -> SemM Block
verifScope vs (StmtVarDecl pos v t e : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)
      SourcePos file line col = pos

  e'      <- verifExprWithContext t vs e

  e_t     <- lift $ either 
               (\msg -> Left . formatSemanticError $ SemanticError file line col "valid type deduction" msg ["variable declaration"]) 
               Right $ exprType s e'

  t'      <- lift $ either (Left . formatSemanticError) Right $ checkMultipleType v file line col t e_t
  vs'     <- lift $ either (Left . formatSemanticError) Right $ assignVarType vs v file line col t'

  stmts'  <- verifScope vs' stmts
  pure $ StmtVarDecl pos v (Just t') e' : stmts'

verifScope vs (StmtExpr pos e : stmts) = do
  e'      <- verifExpr vs e
  stmts'  <- verifScope vs stmts
  pure $ StmtExpr pos e' : stmts'

verifScope vs (StmtReturn pos (Just e) : stmts) = do
  e'      <- verifExpr vs e
  if not (null stmts)
    then lift $ Left $ mkErrorReturn pos "no statements after return" (printf "%d unreachable statement(s) after return" (length stmts))
    else pure [StmtReturn pos (Just e')]

verifScope _ (StmtReturn pos Nothing : stmts) = do
  if not (null stmts)
    then lift $ Left $ mkErrorReturn pos "no statements after return" (printf "%d unreachable statement(s) after return" (length stmts))
    else pure [StmtReturn pos (Just (ExprLitNull pos))]

verifScope vs (StmtIf pos cond a (Just b) : stmts) = do
  cond'   <- verifExpr vs cond
  a'      <- verifScope vs a
  b'      <- verifScope vs b
  stmts'  <- verifScope vs stmts
  pure $ StmtIf pos cond' a' (Just b') : stmts'

verifScope vs (StmtIf pos cond a Nothing : stmts) = do
  cond'   <- verifExpr vs cond
  a'      <- verifScope vs a
  stmts'  <- verifScope vs stmts
  pure $ StmtIf pos cond' a' Nothing : stmts'

verifScope vs (StmtFor pos v t (Just start) end body : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)
      SourcePos file line col = pos

  start'  <- verifExpr vs start
  e_t     <- lift $ either
               (\msg -> Left . formatSemanticError $ SemanticError file line col "valid type deduction" msg ["for loop start"]) 
               Right $ exprType s start'

  vs'     <- lift $ either (Left . formatSemanticError) Right $ assignVarType vs v file line col e_t
  t'      <- lift $ either (Left . formatSemanticError) Right $ checkMultipleType v file line col t e_t

  end'    <- verifExpr vs' end
  body'   <- verifScope vs' body
  stmts'  <- verifScope vs stmts
  pure $ StmtFor pos v (Just t') (Just start') end' body' : stmts'

verifScope vs (StmtFor pos v t Nothing end body : stmts) = do
  let SourcePos file line col = pos
      e_t = fromMaybe TypeAny t
  vs'     <- lift $ either (Left . formatSemanticError) Right $ assignVarType vs v file line col e_t
  t'      <- lift $ either (Left . formatSemanticError) Right $ checkMultipleType v file line col t e_t
  end'    <- verifExpr vs' end
  body'   <- verifScope vs' body
  stmts'  <- verifScope vs stmts
  pure $ StmtFor pos v (Just t') Nothing end' body' : stmts'

verifScope vs (StmtForEach pos v t iter body : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)
      SourcePos file line col = pos

  iter'   <- verifExpr vs iter
  e_t     <- lift $ either
               (\msg -> Left . formatSemanticError $ SemanticError file line col "valid iterable" msg ["for-each iterable"]) 
               Right $ exprType s iter'

  elem_t <- case e_t of
    TypeArray inner -> pure inner
    TypeString -> pure TypeChar
    TypeAny -> pure TypeAny
    _ -> pure TypeAny

  vs'     <- lift $ either (Left . formatSemanticError) Right $ assignVarType vs v file line col elem_t
  t'      <- lift $ either (Left . formatSemanticError) Right $ checkMultipleType v file line col t elem_t

  body'   <- verifScope vs' body
  stmts'  <- verifScope vs stmts
  pure $ StmtForEach pos v (Just t') iter' body' : stmts'

verifScope vs (StmtLoop pos body : stmts) = do
  body'   <- verifScope vs body
  stmts'  <- verifScope vs stmts
  pure $ StmtLoop pos body' : stmts'

verifScope vs (StmtAssignment pos (ExprVar pv lv) rv : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)
      SourcePos file line col = pos

  rv'     <- verifExpr vs rv
  rv_t    <- lift $ either
               (\msg -> Left . formatSemanticError $ SemanticError file line col "valid type deduction" msg ["assignment RHS"]) 
               Right $ exprType s rv'

  vs'     <- lift $ either (Left . formatSemanticError) Right $ assignVarType vs lv file line col rv_t
  stmts'  <- verifScope vs' stmts
  pure $ StmtAssignment pos (ExprVar pv lv) rv' : stmts'

verifScope vs (StmtAssignment pos lhs rv : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)
      SourcePos file line col = pos

  lhs'    <- verifExpr vs lhs
  rv'     <- verifExpr vs rv

  lhs_t   <- lift $ either
               (\msg -> Left . formatSemanticError $ SemanticError file line col "valid lhs type" msg ["assignment LHS"]) 
               Right $ exprType s lhs'
  rv_t    <- lift $ either
               (\msg -> Left . formatSemanticError $ SemanticError file line col "valid rhs type" msg ["assignment RHS"]) 
               Right $ exprType s rv'

  unless (isTypeCompatible lhs_t rv_t) $
    lift $ Left $ formatSemanticError $ SemanticError file line col
      (printf "expression of type %s" (show lhs_t))
      (printf "type %s" (show rv_t)) ["assignment", "global context"]

  stmts'  <- verifScope vs stmts
  pure $ StmtAssignment pos lhs' rv' : stmts'

verifScope vs (StmtStop pos : stmts) = do
  stmts'  <- verifScope vs stmts
  pure $ StmtStop pos : stmts'

verifScope vs (StmtNext pos : stmts) = do
  stmts'  <- verifScope vs stmts
  pure $ StmtNext pos : stmts'

verifScope _ [] = pure []


-- | expression verification
verifExpr :: VarStack -> Expression -> SemM Expression
verifExpr = verifExprWithContext Nothing


verifExprWithContext :: Maybe Type -> VarStack -> Expression -> SemM Expression

verifExprWithContext hint vs (ExprUnary pos op val) = do
  val' <- verifExprWithContext hint vs val
  pure $ ExprUnary pos op val'

verifExprWithContext hint vs (ExprBinary pos op l r) = do
  l' <- verifExprWithContext hint vs l
  r' <- verifExprWithContext hint vs r

  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)
      SourcePos file line col = pos

  leftType  <- lift $ either
                 (\msg -> Left . formatSemanticError $ SemanticError file line col "valid type" msg ["binary left operand"]) 
                 Right $ exprType s l'
  rightType <- lift $ either
                 (\msg -> Left . formatSemanticError $ SemanticError file line col "valid type" msg ["binary right operand"]) 
                 Right $ exprType s r'

  case iHTBinary op leftType rightType of
    Left err -> lift $ Left $ formatSemanticError $ SemanticError file line col "binary operation type mismatch" err ["binary operation"]
    Right _  -> pure $ ExprBinary pos op l' r'

-- Function call: func(args...)
verifExprWithContext hint vs (ExprCall cPos (ExprVar vPos name) args) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)

  args' <- mapM (verifExpr vs) args
  finalArgs <- injectDefaultArgs fs name args' vs
  finalArgTypes <- lift $ mapM (exprType s) finalArgs

  callExpr <- resolveCall cPos vPos s hint name finalArgs finalArgTypes
  pure $ ExprCall cPos callExpr finalArgs

-- Method call: target.method(args...) with inheritance chain support
verifExprWithContext hint vs (ExprCall cPos (ExprAccess accPos target method) args) = do
  ss <- gets stStructs
  args' <- mapM (verifExpr vs) args

  let targetName = case target of
        ExprVar _ n -> n
        _           -> ""
      isStaticCall = HM.member targetName ss
      adjustedTarget = case target of
        ExprVar vPos tName ->
          convertVarToAccess (ExprVar vPos tName) ss (getCustomTypeName =<< HM.lookup tName vs) Nothing (Just method)
        _ -> target

  if isStaticCall
    then verifStaticCall cPos accPos adjustedTarget method args' hint vs
    else verifInstanceCall cPos accPos adjustedTarget method args' hint vs

-- Invalid function call target
verifExprWithContext _ _ (ExprCall {}) =
  lift $ Left "Invalid function call target"

-- Field access: target.field with inheritance chain support
verifExprWithContext hint vs (ExprAccess pos expr@(ExprVar vPos target) field) = do
  ss <- gets stStructs
  currentStruct <- gets stCurrentStruct

  let SourcePos file line col = pos

  case convertVarToAccess expr ss (getCustomTypeName =<< HM.lookup target vs) (Just field) Nothing of
    ExprVar{} -> do
      case HM.lookup target ss of
        Just _ -> do
          fields <- lookupStructFields ss target file line col
          Field _ _ visibility isStatic _ <- findField fields field target file line col
          unless isStatic $
            lift $ Left $ formatSemanticError $ SemanticError file line col
              (printf "static field access '%s.%s'" target field)
              (printf "field '%s' is not static, use an instance instead" field)
              ["static field access"]
          checkFieldVisibility ss visibility currentStruct target (isSelfAccess expr) field file line col
          let globalVarName = target ++ "_" ++ field
          pure $ ExprVar vPos globalVarName
        Nothing -> verifFieldAccess pos (ExprVar vPos target) field hint vs

    expr' -> verifExprWithContext hint vs (ExprAccess pos expr' field)

-- Other field access
verifExprWithContext hint vs (ExprAccess pos target field) =
  verifFieldAccess pos target field hint vs

verifExprWithContext hint vs (ExprStructInit pos name fields) = do
  fields' <- mapM (\(l, e) -> (l,) <$> verifExprWithContext hint vs e) fields
  ss <- gets stStructs
  currentStruct <- gets stCurrentStruct

  let SourcePos file line col = pos
  case HM.lookup name ss of
    Just (DefStruct _ sFields _ isAbstract _) -> do
      when isAbstract $
        canAccessAbstractStruct ss currentStruct name
          (printf "instantiation of abstract struct '%s'" name)
          (printf "cannot instantiate abstract struct '%s'" name)
          file line col
      pure $ ExprStructInit pos name (fields' ++ setDefaultFields fields' sFields)
    _ -> pure $ ExprStructInit pos name fields'
  where
    setDefaultFields :: [(String, Expression)] -> [Field] -> [(String, Expression)]
    setDefaultFields providedFields sFields =
      let providedFieldNames = map fst providedFields
          missingFields = [(fieldName f, fieldDefault f) | f <- sFields, not (fieldIsStatic f), fieldName f `notElem` providedFieldNames]
      in [(fname, expr) | (fname, Just expr) <- missingFields]

verifExprWithContext hint vs (ExprIndex pos target idx) = do
  target' <- verifExprWithContext hint vs target
  idx'    <- verifExpr vs idx
  pure $ ExprIndex pos target' idx'

verifExprWithContext hint vs (ExprLitArray pos elems) = do
  elems' <- mapM (verifExprWithContext hint vs) elems
  pure $ ExprLitArray pos elems'

verifExprWithContext hint vs (ExprCast pos expr typ) = do
  expr' <- verifExprWithContext hint vs expr
  pure $ ExprCast pos expr' typ

verifExprWithContext _ vs (ExprVar pos var)
  | HM.member var vs = pure (ExprVar pos var)
  | otherwise       = lift $ Left $ formatSemanticError $ SemanticError (posFile pos) (posLine pos) (posCol pos) (printf "Undefined variable '%s'" var) "undefined variable" ["variable reference"]

verifExprWithContext _ _ expr = pure expr

verifStaticCall :: SourcePos -> SourcePos -> Expression -> String -> [Expression] -> Maybe Type -> VarStack -> SemM Expression
verifStaticCall cPos accPos target method args' hint vs = do
  fs <- gets stFuncs
  ss <- gets stStructs
  currentStruct <- gets stCurrentStruct
  let s = (fs, vs, ss)
      SourcePos file line col = cPos
      sName = case target of
        ExprVar _ n -> n
        _ -> ""
  argTypes <- lift $ mapM (exprType s) args'
  let baseName = sName ++ "_" ++ method
  callExpr <- resolveCall cPos accPos s hint baseName args' argTypes
  let mangledName = case callExpr of ExprVar _ n -> n; _ -> ""
  verifStaticMethodCall ss fs mangledName currentStruct sName method file line col
  pure $ ExprCall cPos callExpr args'

verifInstanceCall :: SourcePos -> SourcePos -> Expression -> String -> [Expression] -> Maybe Type -> VarStack -> SemM Expression
verifInstanceCall cPos accPos target method args' hint vs = do
  fs <- gets stFuncs
  ss <- gets stStructs
  currentStruct <- gets stCurrentStruct
  let s = (fs, vs, ss)
      SourcePos file line col = cPos

  target' <- verifExprWithContext hint vs target
  targetType <- lift $ either
    (\msg -> Left . formatSemanticError $ SemanticError file line col "valid target type for method" msg ["method call"])
    Right $ exprType s target'
  argTypes <- lift $ mapM (exprType s) args'

  let sName = case targetType of 
        TypeCustom name -> name
        _ -> show targetType
      baseName = sName ++ "_" ++ method
      allArgs = target' : args'
      allArgTypes = targetType : argTypes

  callExpr <- resolveCall cPos accPos s hint baseName allArgs allArgTypes
  let mangledName = case callExpr of ExprVar _ n -> n; _ -> ""
  verifInstanceMethodCall ss fs mangledName currentStruct sName (isSelfAccess target') method file line col
  pure $ ExprCall cPos callExpr allArgs

verifMethod :: String -> TopLevelDef -> SemM TopLevelDef
verifMethod sName (DefFunction methodName params retType body isExport visibility isStatic isAbstract) = do
  checkMethodParams methodName isStatic params
  let params' = if isStaticMethod methodName isStatic then params else fixSelfType sName params
      paramTypes = map paramType params'
      baseName = sName ++ "_" ++ methodName
      finalName = mangleName baseName retType paramTypes

      vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params'
  -- Set struct context for method body verification
  oldStruct <- gets stCurrentStruct
  modify $ \st -> st { stCurrentStruct = Just sName }
  body' <- verifScope vs body
  modify $ \st -> st { stCurrentStruct = oldStruct }

  pure $ DefFunction finalName params' retType body' isExport visibility isStatic isAbstract
verifMethod _ def = pure def

--
-- helper
--

-- | Inject default parameter values for missing arguments
injectDefaultArgs :: FuncStack -> String -> [Expression] -> VarStack -> SemM [Expression]
injectDefaultArgs fs name args vs =
  case HM.lookup name fs of
    Just ((_, params), _, _) | length args < length params ->
      let missingParams = drop (length args) params
          -- Extract only the expressions from parameters that have defaults
          defaultExprs = [expr | p <- missingParams, Just expr <- [paramDefault p]]
      in do
        verifiedDefaults <- mapM (verifExpr vs) defaultExprs
        pure (args ++ verifiedDefaults)
    _ -> pure args

--
-- instanciation
--

tryInstantiateTemplate :: TopLevelDef -> String -> [Expression] -> [Type] -> Maybe Type -> SemM Expression
tryInstantiateTemplate def originalName args argTypes contextRetType = do
  retTy <- resolveReturnType originalName argTypes contextRetType
  let mangled = mangleName originalName retTy argTypes
      pos = case args of
              (e:_) -> getExprPos e
              []    -> SourcePos "<generated>" 0 0

  alreadyInstantiated mangled >>= \case
    True  -> pure $ ExprVar pos mangled
    False -> do
      verified <- verifTopLevel (instantiate def argTypes retTy)
      registerInstantiation mangled verified retTy argTypes
      pure $ ExprVar pos mangled

resolveReturnType :: String -> [Type] -> Maybe Type -> SemM Type
resolveReturnType originalName argTypes mCtx =
  case mCtx of
    Just t | t /= TypeAny -> pure t
    _ -> fromArgs
  where
    fromArgs = case argTypes of
      t:_ -> pure t
      []  -> lift $ Left $
        printf
          "Generic function '%s' cannot be instantiated: no arguments provided and no return type context."
          originalName


alreadyInstantiated :: String -> SemM Bool
alreadyInstantiated name = HM.member name <$> gets stInstantiated

registerInstantiation :: String -> TopLevelDef -> Type -> [Type] -> SemM ()
registerInstantiation name def retTy argTys =
  let params = map (\t -> Parameter "" t Nothing) argTys
  in modify $ \st -> st
    { stNewDefs      = stNewDefs st <> [def]
    , stFuncs        = HM.insert name ((retTy, params), Public, False) (stFuncs st)
    , stInstantiated = HM.insert name True (stInstantiated st)
    }

resolveCall :: SourcePos -> SourcePos -> Stack -> Maybe Type -> String -> [Expression] -> [Type] -> SemM Expression
resolveCall cPos vPos s hint name args argTypes = do
  let file = posFile cPos
      line = posLine cPos
      col = posCol cPos
  -- First, try to resolve as a struct method if we're inside a struct
  currentStruct <- gets stCurrentStruct
  case currentStruct of
    Just sName -> do
      let structMethodName = sName ++ "_" ++ name
          matchStruct = checkParamType s (structMethodName, argTypes) file line col args
      case matchStruct of
        Right foundName -> pure $ ExprVar vPos foundName
        Left _ -> resolveRegular
    Nothing -> resolveRegular
  where
    resolveRegular = do
      let file = posFile cPos
          line = posLine cPos
          col = posCol cPos
          match = checkParamType s (name, argTypes) file line col args
      case match of
        Right foundName -> pure $ ExprVar vPos foundName
        Left err -> do
          templates <- gets stTemplates
          case HM.lookup name templates of
            Nothing -> lift $ Left $ formatSemanticError err
            Just templateDef -> tryInstantiateTemplate templateDef name args argTypes hint

-- | Check if a member (field or method) can be accessed based on visibility rules
-- Returns True if access is allowed, False otherwise
canAccessMember :: StructStack -> Visibility -> Maybe String -> String -> Bool -> Bool
canAccessMember _ Public _ _ _ = True
canAccessMember _ Private currentStruct targetStruct isSelf =
  case currentStruct of
    Nothing -> False
    Just ctx -> ctx == targetStruct && isSelf
canAccessMember ss Protected currentStruct targetStruct isSelf =
  case currentStruct of
    Nothing -> False
    Just ctx -> (ctx == targetStruct || (isSelf && isDescendantOf ss ctx targetStruct))

-- | Check if we can access an abstract struct from current context
canAccessAbstractStruct :: StructStack -> Maybe String -> String -> String -> String -> String -> Int -> Int -> SemM ()
canAccessAbstractStruct ss currentStruct sName context errorMsg file line col = do
  case HM.lookup sName ss of
    Just (DefStruct _ _ _ True _) -> do
      case currentStruct of
        Just ctx | ctx == sName || isDescendantOf ss ctx sName ->
          pure ()
        _ -> lift $ Left $ formatSemanticError $ SemanticError file line col
              context
              errorMsg
              ["struct instantiation"]
    _ -> pure ()

isSelfAccess :: Expression -> Bool
isSelfAccess (ExprVar _ "self") = True
isSelfAccess (ExprAccess _ target "__base") = isSelfAccess target
isSelfAccess _ = False

isDescendantOf :: StructStack -> String -> String -> Bool
isDescendantOf ss ctx target
  | ctx == target = True
  | otherwise = case HM.lookup ctx ss of
      Just (DefStruct _ _ _ _ (Just (ext:_))) -> isDescendantOf ss ext target
      _ -> False

-- This function is used to check if a function is static or not.
-- It's here just to declare 'new' as a default static method.
isStaticMethod :: String -> Bool -> Bool
isStaticMethod _ True = True
isStaticMethod "new" _ = True
isStaticMethod _ False = False

checkMethodParams :: String -> Bool -> [Parameter] -> SemM ()
checkMethodParams methodName isStatic params
  | isStaticMethod methodName isStatic = pure ()  -- Static methods don't need self
  | otherwise = case params of
      [] -> lift $ Left $ printf "Instance method '%s' must have at least one parameter (self)" methodName
      (p:_) | paramName p /= "self" ->
        lift $ Left $ printf "First parameter of method '%s' must be 'self', got '%s'" methodName (paramName p)
      _ -> pure ()

-- | Verify field access with visibility checking
verifFieldAccess :: SourcePos -> Expression -> String -> Maybe Type -> VarStack -> SemM Expression
verifFieldAccess pos target field hint vs = do
  fs <- gets stFuncs
  ss <- gets stStructs
  currentStruct <- gets stCurrentStruct
  let s = (fs, vs, ss)
      SourcePos file line col = pos
  target' <- verifExprWithContext hint vs target
  targetType <- lift $ either
    (\msg -> Left . formatSemanticError $ SemanticError file line col "valid target type" msg ["field access"])
    Right $ exprType s target'
  verifStructFieldAccess pos target' field targetType currentStruct ss file line col

-- | Verify struct field access with visibility checking
verifStructFieldAccess :: SourcePos -> Expression -> String -> Type -> Maybe String -> StructStack -> String -> Int -> Int -> SemM Expression
verifStructFieldAccess pos target' field targetType currentStruct ss file line col =
  case targetType of
    TypeCustom sName -> do
      fields <- lookupStructFields ss sName file line col
      Field _ _ visibility isStatic _ <- findField fields field sName file line col
      when isStatic $
        lift $ Left $ formatSemanticError $ SemanticError file line col
          (printf "instance field access on '%s'" field)
          (printf "field '%s' is static, call it with '%s.%s' instead" field sName field)
          ["instance field access"]
      checkFieldVisibility ss visibility currentStruct sName (isSelfAccess target') field file line col
      pure $ ExprAccess pos target' field
    _ -> lift $ Left $ formatSemanticError $ SemanticError file line col
      "struct type for field access"
      (printf "type '%s' does not have fields" (show targetType))
      ["field access"]

-- | Lookup struct fields in the struct stack
lookupStructFields :: StructStack -> String -> String -> Int -> Int -> SemM [Field]
lookupStructFields ss sName file line col =
  case HM.lookup sName ss of
    Nothing -> lift $ Left $ formatSemanticError $ SemanticError file line col
      (printf "struct '%s'" sName)
      (printf "struct '%s' not found" sName)
      ["field access"]
    Just (DefStruct _ fields _ _ _) -> pure fields
    Just _ -> lift $ Left $ formatSemanticError $ SemanticError file line col
      (printf "struct '%s'" sName)
      (printf "'%s' is not a struct" sName)
      ["field access"]

-- | Find a field in a list of fields
findField :: [Field] -> String -> String -> String -> Int -> Int -> SemM Field
findField fields targetFieldName sName file line col =
  case List.find (\f -> fieldName f == targetFieldName) fields of
    Nothing -> lift $ Left $ formatSemanticError $ SemanticError file line col
      (printf "field '%s' in struct '%s'" targetFieldName sName)
      (printf "field '%s' does not exist" targetFieldName)
      ["field access"]
    Just f -> pure f

-- | Raise a visibility error if access is not allowed
raiseVisibilityError :: StructStack -> Visibility -> Maybe String -> String -> Bool -> String -> String -> String -> Int -> Int -> String -> SemM ()
raiseVisibilityError ss visibility currentStruct sName isSelf memberName memberType file line col context =
  unless (canAccessMember ss visibility currentStruct sName isSelf) $
    lift $ Left $ formatSemanticError $ SemanticError file line col
      (printf "access to %s %s '%s' of struct '%s'" (show visibility) memberType memberName sName)
      (if isSelf
       then printf "cannot access %s %s outside struct context" (show visibility) memberType
       else printf "cannot access %s %s '%s' on other instances (only 'self' allowed)" (show visibility) memberType memberName)
      [context, "visibility"]

-- | Verify static method call - checks that the resolved method is actually static
verifStaticMethodCall :: StructStack -> FuncStack -> String -> Maybe String -> String -> String -> String -> Int -> Int -> SemM ()
verifStaticMethodCall ss fs mangledName currentStruct target method file line col = do
  case HM.lookup mangledName fs of
    Just (_, visibility, isStatic) -> do
      unless (isStaticMethod method isStatic) $
        lift $ Left $ formatSemanticError $ SemanticError file line col
          (printf "static method call '%s.%s'" target method)
          (printf "method '%s' is not static, use an instance instead" method)
          ["static method call"]
      canAccessAbstractStruct ss currentStruct target
        (printf "static method call '%s.%s'" target method)
        (printf "cannot call static method '%s' on abstract struct '%s' from this context" method target)
        file line col
      raiseVisibilityError ss visibility currentStruct target False method "method" file line col "static method call"
    Nothing -> pure ()

-- | Verify instance method call - checks visibility only
verifInstanceMethodCall :: StructStack -> FuncStack -> String -> Maybe String -> String -> Bool -> String -> String -> Int -> Int -> SemM ()
verifInstanceMethodCall ss fs mangledName currentStruct sName isSelf method file line col = do
  case HM.lookup mangledName fs of
    Just (_, visibility, isStatic) -> do
      when (isStaticMethod method isStatic) $
        lift $ Left $ formatSemanticError $ SemanticError file line col
          (printf "instance method call on '%s'" method)
          (printf "method '%s' is static, call it with '%s.%s' instead" method sName method)
          ["instance method call"]
      raiseVisibilityError ss visibility currentStruct sName isSelf method "method" file line col "method call"
    Nothing -> pure ()

-- | Check if field access is allowed based on visibility rules
checkFieldVisibility :: StructStack -> Visibility -> Maybe String -> String -> Bool -> String -> String -> Int -> Int -> SemM ()
checkFieldVisibility ss visibility currentStruct sName isSelf field file line col =
  raiseVisibilityError ss visibility currentStruct sName isSelf field "field" file line col "field access"

-- | 1. Get the current struct in the stack
-- | 2. Check if the struct inherit from another
-- | -> If no, return the expression as is
-- | -> If yes, convert the variable to an access expression
-- | Do it recursively until the top of the inheritance chain
convertVarToAccess :: Expression -> StructStack -> Maybe String -> Maybe String -> Maybe String -> Expression

convertVarToAccess expr ss (Just sName) (Just field) Nothing =
  case HM.lookup sName ss of
    Just (DefStruct _ fields _ _ (Just (ext:_))) ->
      case findFieldInList fields field of
        Just _ -> expr
        Nothing ->
          let baseAccess = ExprAccess (SourcePos "<unknown>" 0 0) expr "__base"
          in convertVarToAccess baseAccess ss (Just ext) (Just field) Nothing
    _ -> expr
  where
    findFieldInList fs fname = List.find (\f -> fieldName f == fname) fs

convertVarToAccess expr ss (Just sName) Nothing (Just method) =
  case HM.lookup sName ss of
    Just (DefStruct _ _ methods _ (Just (ext:_))) ->
      case findMethodInList methods method of
        Just _ -> expr
        Nothing ->
          let baseAccess = ExprAccess (SourcePos "<unknown>" 0 0) expr "__base"
          in convertVarToAccess baseAccess ss (Just ext) Nothing (Just method)
    _ -> expr
  where
    findMethodInList ms mname =
      List.find (\d -> case d of
        DefFunction n _ _ _ _ _ _ _ -> n == mname
        _                           -> False) ms

convertVarToAccess expr _ _ _ _ = expr
