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
  hasImplicitOrExplicitReturn,
  blockContainsReturn
) where
#else
module Rune.Semantics.Vars (verifVars) where
#endif

import Control.Monad (unless)
import Control.Monad.State.Strict

import Text.Printf (printf)

import Data.Maybe (fromMaybe, fromJust, isJust)
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

-- | Extract all DeclDefs from somewhere blocks and return them as separate top-level definitions
-- Also return cleaned somewhere blocks with only signatures  
-- Note: DeclUse statements should have been preprocessed away by this point
extractAndCleanSomewhereDefs :: [TopLevelDef] -> ([TopLevelDef], [TopLevelDef])
extractAndCleanSomewhereDefs defs =
  let (extracted, cleaned) = unzip $ map processTopLevel defs
  in (concat extracted, cleaned)
  where
    processTopLevel :: TopLevelDef -> ([TopLevelDef], TopLevelDef)
    processTopLevel (DefSomewhere decls) =
      let (extractedDefs, remainingDecls) = partitionSomewhereDecls decls
      in (extractedDefs, DefSomewhere remainingDecls)
    processTopLevel other = ([], other)
    
    partitionSomewhereDecls :: [SomewhereDecl] -> ([TopLevelDef], [SomewhereDecl])
    partitionSomewhereDecls decls =
      let extractedDefs = [def | DeclDefs def <- decls]
          remainingDecls = [decl | decl <- decls, not (isDeclDefs decl) && not (isDeclUse decl)]
      in (extractedDefs, remainingDecls)
    
    isDeclDefs :: SomewhereDecl -> Bool
    isDeclDefs (DeclDefs _) = True
    isDeclDefs _ = False
    
    isDeclUse :: SomewhereDecl -> Bool
    isDeclUse (DeclUse _) = True
    isDeclUse _ = False

--
-- public
--

verifVars :: Program -> Either String (Program, FuncStack)
verifVars (Program n defs) = do
  -- Apply type inference to parameters before checking if generic
  let defsWithInferredTypes = map applyInferenceToParams defs
      (templatesList, concreteDefs) = List.partition isGeneric defsWithInferredTypes
      templatesMap = HM.fromList $ map (\d -> (getDefName d, d)) templatesList
      
      -- Extract all full definitions from somewhere blocks and flatten them  
      (flattenedDefs, cleanedDefs) = extractAndCleanSomewhereDefs concreteDefs
      allConcreteDefs = flattenedDefs ++ cleanedDefs

  fs <- findFunc (Program n allConcreteDefs)
  ss <- findStruct (Program n allConcreteDefs)

  let initialState = SemState
        { stFuncs = fs
        , stTemplates = templatesMap
        , stNewDefs = []
        , stInstantiated = HM.empty
        , stStructs = ss
        , stCurrentStruct = Nothing
        }

  (defs', finalState) <- runStateT (mapM verifTopLevel allConcreteDefs) initialState
  let allDefs = defs' <> stNewDefs finalState
      finalFuncStack = mangleFuncStack $ stFuncs finalState
  pure (Program n allDefs, finalFuncStack)

--
-- private
--
mkErrorReturn :: SourcePos -> String -> String -> [String] -> String
mkErrorReturn (SourcePos file line col) expected got context =
  formatSemanticError $ SemanticError file line col expected got context

isGeneric :: TopLevelDef -> Bool
isGeneric (DefFunction _ params ret _ _) = hasAny ret || any (hasAny . paramType) params
isGeneric _ = False

-- | Apply type inference to function parameters from default values
applyInferenceToParams :: TopLevelDef -> TopLevelDef
applyInferenceToParams (DefFunction name params ret body isExport) =
  DefFunction name (map inferParamType params) ret body isExport
applyInferenceToParams def = def


hasAny :: Type -> Bool
hasAny TypeAny = True
hasAny (TypeArray t) = hasAny t
hasAny _ = False


getDefName :: TopLevelDef -> String
getDefName (DefFunction n _ _ _ _) = n
getDefName (DefStruct n _ _) = n
getDefName (DefSomewhere _) = ""

mangleFuncStack :: FuncStack -> FuncStack
mangleFuncStack fs = fs

hasImplicitOrExplicitReturn :: Block -> Bool
hasImplicitOrExplicitReturn [] = False
hasImplicitOrExplicitReturn stmts = case last stmts of
  StmtReturn _ (Just _) -> True
  StmtReturn _ Nothing -> True
  StmtExpr _ _ -> True
  StmtIf _ _ thenB (Just elseB) -> 
    hasImplicitOrExplicitReturn thenB && hasImplicitOrExplicitReturn elseB
  StmtLoop _ body -> blockContainsReturn body
  _ -> False

blockContainsReturn :: Block -> Bool
blockContainsReturn = any stmtContainsReturn
  where
    stmtContainsReturn (StmtReturn _ (Just _)) = True
    stmtContainsReturn (StmtIf _ _ thenB elseB) = 
      blockContainsReturn thenB || maybe False blockContainsReturn elseB
    stmtContainsReturn (StmtFor _ _ _ _ _ body) = blockContainsReturn body
    stmtContainsReturn (StmtForEach _ _ _ _ body) = blockContainsReturn body
    stmtContainsReturn (StmtLoop _ body) = blockContainsReturn body
    stmtContainsReturn _ = False

checkFnReturn :: SourcePos -> String -> Type -> Block -> SemM ()
checkFnReturn _ _ TypeNull _ = pure ()
checkFnReturn pos fnName _ [] = 
  lift $ Left $ mkErrorReturn pos
    (printf "explicit return statement of type %s" (show TypeNull))
    (printf "no return statement in function '%s'" fnName)
    ["function body", "return statement"]
checkFnReturn pos fnName retType body =
  unless (hasImplicitOrExplicitReturn body) $
    lift $ Left $ mkErrorReturn pos
      (printf "explicit return statement of type %s" (show retType))
      (printf "no return statement in function '%s'" fnName)
      ["function body", "return statement"]

--
-- verif
--

verifTopLevel :: TopLevelDef -> SemM TopLevelDef
verifTopLevel (DefFunction name params r_t body isExport) = do
  let paramTypes = map paramType params
      expectedMangled = mangleName name r_t paramTypes
      -- Don't double-mangle: if name is already mangled form, use it as-is
      finalName = if name == "main" || name == expectedMangled 
                  then name 
                  else mangleName name r_t paramTypes
      pos = case body of
              (stmt:_) -> getStmtPos stmt
              [] -> SourcePos "<unknownPos>" 0 0

  checkFnReturn pos name r_t body
  checkUniqueParamNames pos "function definition" name params
  let vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params
  body' <- verifScope vs body
  pure $ DefFunction finalName params r_t body' isExport

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
  if not (null stmts)
    then lift $ Left $ mkErrorReturn pos "no statements after return" (printf "%d unreachable statement(s) after return" (length stmts)) ["function body", "control flow"]
    else pure [StmtReturn pos (Just e')]

verifScope _ (StmtReturn pos Nothing : stmts) = do
  if not (null stmts)
    then lift $ Left $ mkErrorReturn pos "no statements after return" (printf "%d unreachable statement(s) after return" (length stmts)) ["function body", "control flow"]
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

verifExprWithContext hint vs (ExprCall cPos (ExprAccess _ (ExprVar vPos target) method) args) = do
  fs <- gets stFuncs
  ss <- gets stStructs
  let s = (fs, vs, ss)

  case HM.lookup target ss of

    -- | static method call: StructName.method(args)
    Just _ -> do
      let baseName = target ++ "_" ++ method
      args' <- mapM (verifExpr vs) args
      _ <- lift $ mapM (exprType s) args'
      finalArgs <- injectDefaultArgs fs baseName args' vs
      finalArgTypes <- lift $ mapM (exprType s) finalArgs
      callExpr <- resolveCall cPos vPos s hint baseName finalArgs finalArgTypes
      pure $ ExprCall cPos callExpr finalArgs

    -- instance method call: variable.method(args)
    Nothing -> do
      targetType <- lift $ exprType s (ExprVar vPos target)
      let baseName = show targetType ++ "_" ++ method

      args' <- mapM (verifExpr vs) args
      _ <- lift $ mapM (exprType s) args'
      let selfArg = ExprVar vPos target

      -- Inject defaults for instance methods (after self parameter)
      finalArgs <- case HM.lookup baseName fs of
        Just (_, params) | not (null params) ->
          let missingParams = drop (length args' + 1) params  -- +1 for self
          in if not (null missingParams) && all (isJust . paramDefault) missingParams
               then
                 let defaults = [fromJust (paramDefault p) | p <- missingParams]
                 in do
                   verifiedDefaults <- mapM (verifExpr vs) defaults
                   pure (selfArg : args' ++ verifiedDefaults)
               else pure (selfArg : args')
        _ -> pure (selfArg : args')

      finalArgTypes <- lift $ mapM (exprType s) finalArgs
      callExpr <- resolveCall cPos vPos s hint baseName finalArgs finalArgTypes
      pure $ ExprCall cPos callExpr finalArgs

verifExprWithContext _ _ (ExprCall {}) =
  lift $ Left "Invalid function call target"

verifExprWithContext hint vs (ExprStructInit pos name fields) = do
  fields' <- mapM (\(l, e) -> (l,) <$> verifExprWithContext hint vs e) fields
  pure $ ExprStructInit pos name fields'

verifExprWithContext hint vs (ExprAccess pos target field) = do
  target' <- verifExprWithContext hint vs target
  pure $ ExprAccess pos target' field

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
verifMethod sName (DefFunction methodName params retType body isExport) = do
  let pos = case body of
              (stmt:_) -> getStmtPos stmt
              [] -> SourcePos "<unknownPos>" 0 0
  checkMethodParams methodName params pos
  let params' = if isStaticMethod methodName then params else fixSelfType sName params
      paramTypes = map paramType params'
      baseName = sName ++ "_" ++ methodName
      finalName = mangleName baseName retType paramTypes

      vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params'
  checkFnReturn pos methodName retType body
  oldStruct <- stCurrentStruct <$> get
  modify $ \st -> st { stCurrentStruct = Just sName }
  body' <- verifScope vs body
  modify $ \st -> st { stCurrentStruct = oldStruct }
  pure $ DefFunction finalName params' retType body' isExport
verifMethod _ def = pure def

--
-- helper
--

-- | Inject default parameter values for missing arguments
injectDefaultArgs :: FuncStack -> String -> [Expression] -> VarStack -> SemM [Expression]
injectDefaultArgs fs name args vs =
  case HM.lookup name fs of
    Just (_, params) | length args < length params ->
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
    , stFuncs        = HM.insert name (retTy, params) (stFuncs st)
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

-- | Check if a method is static (doesn't need self)
isStaticMethod :: String -> Bool
isStaticMethod "new" = True
isStaticMethod _     = False

checkMethodParams :: String -> [Parameter] -> SourcePos -> SemM ()
checkMethodParams methodName params pos
  | isStaticMethod methodName = checkUniqueParamNames pos "method definition" methodName params
  | otherwise = case params of
      [] -> lift $ Left $ mkErrorReturn pos
        "at least one parameter (self)"
        (printf "no parameters in method '%s'" methodName)
        ["method definition", "parameters"]
      (p:_) | paramName p /= "self" ->
        lift $ Left $ mkErrorReturn pos
          "first parameter named 'self'"
          (printf "first parameter is '%s'" (paramName p))
          ["method definition", "parameters"]
      _ -> checkUniqueParamNames pos "method definition" methodName params

checkUniqueParamNames :: SourcePos -> String -> String -> [Parameter] -> SemM ()
checkUniqueParamNames pos context fName params = do
  let paramNames = map paramName params
      duplicates = paramNames List.\\ List.nub paramNames
  unless (null duplicates) $
    lift $ Left $ mkErrorReturn pos
      (printf "unique parameter names in function '%s'" fName)
      (printf "%s '%s' has duplicate parameter names: %s" context fName (show duplicates))
      ["function definition", "parameters"]
