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
  resolveFinalName,
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
  checkMethodVisibility,
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

-- import Debug.Trace (trace)

--
-- state monad
--

data SemState = SemState
  { stFuncs         :: FuncStack               -- << known signatures
  , stTemplates     :: Templates               -- << generic functions ('any')
  , stNewDefs       :: [TopLevelDef]           -- << new functions
  , stInstantiated  :: HM.HashMap String Bool  -- << cache of instantiated templates
  , stStructs       :: StructStack             -- << known structs
  , stCurrentStruct :: Maybe String            -- << current struct context (for methods)
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
  ss <- findStruct (Program n concreteDefs)

  let initialState = SemState
        { stFuncs = fs
        , stTemplates = templatesMap
        , stNewDefs = []
        , stInstantiated = HM.empty
        , stStructs = ss
        , stCurrentStruct = Nothing
        }

  (defs', finalState) <- runStateT (mapM verifTopLevel concreteDefs) initialState
  let allDefs = defs' <> stNewDefs finalState
      finalFuncStack = mangleFuncStack $ stFuncs finalState
  pure (Program n allDefs, finalFuncStack)

--
-- private
--

isGeneric :: TopLevelDef -> Bool
isGeneric (DefFunction _ params ret _ _ _ _) = hasAny ret || any (hasAny . paramType) params
isGeneric _ = False

-- | Apply type inference to function parameters from default values
applyInferenceToParams :: TopLevelDef -> TopLevelDef
applyInferenceToParams (DefFunction name params ret body isExport visibility isStatic) =
  DefFunction name (map inferParamType params) ret body isExport visibility isStatic
applyInferenceToParams def = def


hasAny :: Type -> Bool
hasAny TypeAny = True
hasAny (TypeArray t) = hasAny t
hasAny _ = False


getDefName :: TopLevelDef -> String
getDefName (DefFunction n _ _ _ _ _ _) = n
getDefName (DefStruct n _ _) = n
getDefName (DefSomewhere {}) = ""

mangleFuncStack :: FuncStack -> FuncStack
mangleFuncStack fs = fs

--
-- verif
--

resolveFinalName :: String -> Type -> [Type] -> SemM String
resolveFinalName baseName retType paramTypes = do
  fs <- gets stFuncs
  pure $ case HM.lookup baseName fs of
    Just ((exRet, exArgs), _, _) ->
      if exRet == retType && map paramType exArgs == paramTypes
      then baseName
      else mangleName baseName retType paramTypes
    Nothing -> baseName

verifTopLevel :: TopLevelDef -> SemM TopLevelDef
verifTopLevel (DefFunction name params r_t body isExport visibility isStatic) = do
  let paramTypes = map paramType params
  finalName <- resolveFinalName name r_t paramTypes

  let vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params
  body' <- verifScope vs body
  pure $ DefFunction finalName params r_t body' isExport visibility isStatic

verifTopLevel (DefStruct name fields methods) = do
  methods' <- mapM (verifMethod name) methods
  pure $ DefStruct name fields methods'

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
  stmts'  <- verifScope vs stmts
  pure $ StmtReturn pos (Just e') : stmts'

verifScope vs (StmtReturn pos Nothing : stmts) = do
  stmts'  <- verifScope vs stmts
  pure $ StmtReturn pos (Just (ExprLitNull pos)) : stmts'

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

  fs <- gets stFuncs
  ss <- gets stStructs
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

verifExprWithContext hint vs (ExprCall cPos (ExprVar vPos name) args) = do
  fs <- gets stFuncs
  ss <- gets stStructs
  let s = (fs, vs, ss)

  args' <- mapM (verifExpr vs) args
  _ <- lift $ mapM (exprType s) args'

  -- Inject default values for missing parameters
  finalArgs <- injectDefaultArgs fs name args' vs

  finalArgTypes <- lift $ mapM (exprType s) finalArgs

  callExpr <- resolveCall cPos vPos s hint name finalArgs finalArgTypes
  pure $ ExprCall cPos callExpr finalArgs

verifExprWithContext hint vs (ExprAccess pos target field) =
  verifFieldAccess pos target field hint vs

verifExprWithContext hint vs (ExprCall cPos (ExprAccess _ (ExprVar vPos target) method) args) = do
  fs <- gets stFuncs
  ss <- gets stStructs
  currentStruct <- gets stCurrentStruct
  let s = (fs, vs, ss)
      SourcePos file line col = cPos

  args' <- mapM (verifExpr vs) args
  argTypes <- lift $ mapM (exprType s) args'

  case HM.lookup target ss of
    Just _ -> do
      let baseName = target ++ "_" ++ method
      callExpr <- resolveCall cPos vPos s hint baseName args' argTypes
      let mangledName = case callExpr of ExprVar _ n -> n; _ -> ""
      verifStaticMethodCall fs mangledName currentStruct target method file line col
      pure $ ExprCall cPos callExpr args'
    Nothing -> do
      targetType <- lift $ exprType s (ExprVar vPos target)
      let baseName = show targetType ++ "_" ++ method
          sName = case targetType of { TypeCustom name -> name; _ -> show targetType }
          isSelf = target == "self"
          allArgs = ExprVar vPos target : args'
          allArgTypes = targetType : argTypes

      callExpr <- resolveCall cPos vPos s hint baseName allArgs allArgTypes
      let mangledName = case callExpr of ExprVar _ n -> n; _ -> ""
      verifInstanceMethodCall fs mangledName currentStruct sName isSelf method file line col
      pure $ ExprCall cPos callExpr allArgs

verifExprWithContext _ _ (ExprCall {}) =
  lift $ Left "Invalid function call target"

verifExprWithContext hint vs (ExprStructInit pos name fields) = do
  fields' <- mapM (\(l, e) -> (l,) <$> verifExprWithContext hint vs e) fields
  pure $ ExprStructInit pos name fields'

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

verifMethod :: String -> TopLevelDef -> SemM TopLevelDef
verifMethod sName (DefFunction methodName params retType body isExport visibility isStatic) = do
  modify $ \st -> st {stCurrentStruct = Just sName}
  let params' = if isStatic then params else fixSelfType sName params
      paramTypes = map paramType params'
      baseName = sName ++ "_" ++ methodName

  finalName <- resolveFinalName baseName retType paramTypes

  let vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params'
  body' <- verifScope vs body
  modify $ \st -> st {stCurrentStruct = Nothing}
  pure $ DefFunction finalName params' retType body' isExport visibility isStatic
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
canAccessMember :: Visibility -> Maybe String -> String -> Bool -> Bool
canAccessMember Public _ _ _ = True
canAccessMember Private currentStruct targetStruct isSelf =
  case currentStruct of
    Nothing -> False
    Just ctx -> ctx == targetStruct && isSelf
canAccessMember Protected currentStruct targetStruct isSelf =
  case currentStruct of
    Nothing -> False
    Just ctx -> (ctx == targetStruct || isChildOf ctx targetStruct) && isSelf
  where
    isChildOf _ _ = True  -- TODO: implement inheritance checking

isSelfAccess :: Expression -> Bool
isSelfAccess (ExprVar _ "self") = True
isSelfAccess _                  = False

-- This function is used to check if a function is static or not.
-- It's here just to declare 'new' as a default static method.
isStaticMethod :: String -> Bool -> Bool
isStaticMethod _ True = True
isStaticMethod "new" _ = True
isStaticMethod _ False = False

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
      Field _ _ visibility _ <- findField fields field sName file line col
      checkFieldVisibility visibility currentStruct sName (isSelfAccess target') field file line col
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
    Just (DefStruct _ fields _) -> pure fields
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
raiseVisibilityError :: Visibility -> Maybe String -> String -> Bool -> String -> String -> String -> Int -> Int -> String -> SemM ()
raiseVisibilityError visibility currentStruct sName isSelf memberName memberType file line col context =
  unless (canAccessMember visibility currentStruct sName isSelf) $
    lift $ Left $ formatSemanticError $ SemanticError file line col
      (printf "access to %s %s '%s' of struct '%s'" (show visibility) memberType memberName sName)
      (if isSelf
       then printf "cannot access %s %s outside struct context" (show visibility) memberType
       else printf "cannot access %s %s '%s' on other instances (only 'self' allowed)" (show visibility) memberType memberName)
      [context, "visibility"]

-- | Verify static method call - checks that the resolved method is actually static
verifStaticMethodCall :: FuncStack -> String -> Maybe String -> String -> String -> String -> Int -> Int -> SemM ()
verifStaticMethodCall fs mangledName currentStruct target method file line col = do
  case HM.lookup mangledName fs of
    Just (_, visibility, isStatic) -> do
      unless (isStaticMethod method isStatic) $
        lift $ Left $ formatSemanticError $ SemanticError file line col
          (printf "static method call '%s.%s'" target method)
          (printf "method '%s' is not static, use an instance instead" method)
          ["static method call"]
      raiseVisibilityError visibility currentStruct target False method "method" file line col "static method call"
    Nothing -> pure ()

-- | Verify instance method call - checks visibility only
verifInstanceMethodCall :: FuncStack -> String -> Maybe String -> String -> Bool -> String -> String -> Int -> Int -> SemM ()
verifInstanceMethodCall fs mangledName currentStruct sName isSelf method file line col = do
  case HM.lookup mangledName fs of
    Just (_, visibility, isStatic) -> do
      when (isStaticMethod method isStatic) $
        lift $ Left $ formatSemanticError $ SemanticError file line col
          (printf "instance method call on '%s'" method)
          (printf "method '%s' is static, call it with '%s.%s' instead" method sName method)
          ["instance method call"]
      raiseVisibilityError visibility currentStruct sName isSelf method "method" file line col "method call"
    Nothing -> pure ()

-- | Check if field access is allowed based on visibility rules
checkFieldVisibility :: Visibility -> Maybe String -> String -> Bool -> String -> String -> Int -> Int -> SemM ()
checkFieldVisibility visibility currentStruct sName isSelf field file line col =
  raiseVisibilityError visibility currentStruct sName isSelf field "field" file line col "field access"

-- | Check if method call is allowed based on visibility rules
checkMethodVisibility :: FuncStack -> String -> Maybe String -> String -> Bool -> String -> String -> Int -> Int -> SemM ()
checkMethodVisibility fs baseName currentStruct sName isSelf method file line col = do
  let prefix = baseName ++ "$"
      matchingMethods = HM.filterWithKey (\k _ -> k == baseName || prefix `List.isPrefixOf` k) fs
      visibilities = map (\(_, (_, vis, _)) -> vis) (HM.toList matchingMethods)
  case visibilities of
    [] -> pure ()
    (visibility:_) ->
      raiseVisibilityError visibility currentStruct sName isSelf method "method" file line col "method call"
