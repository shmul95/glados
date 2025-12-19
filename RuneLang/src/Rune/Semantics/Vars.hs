{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

#if defined(TESTING_EXPORT)
module Rune.Semantics.Vars (
  verifVars,
  verifScope,
  verifExpr,
  verifExprWithContext,
  mangleFuncStack
) where
#else
module Rune.Semantics.Vars (verifVars) where
#endif

import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

import Text.Printf (printf)

import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc)
import Rune.Semantics.Struct (findStruct)
import Rune.Semantics.Generic (instantiate)

import Rune.Semantics.Type
  ( FuncStack
  , VarStack
  , StructStack
  , Stack
  , Templates
  )

import Rune.Semantics.Helper
  ( checkParamType
  , mangleName
  , exprType
  , assignVarType
  , checkMultipleType
  , SemanticError(..)
  , formatSemanticError
  , fixSelfType
  )
import Rune.Semantics.OpType (iHTBinary)

import Debug.Trace (trace)

--
-- state monad
--

data SemState = SemState
  { stFuncs        :: FuncStack               -- << known signatures
  , stTemplates    :: Templates               -- << generic functions ('any')
  , stNewDefs      :: [TopLevelDef]           -- << new functions
  , stInstantiated :: HM.HashMap String Bool  -- << cache of instantiated templates
  , stStructs      :: StructStack            -- << known structs
  }

type SemM a = StateT SemState (Either String) a

--
-- public
--

verifVars :: Program -> Either String (Program, FuncStack)
verifVars (Program n defs) = do
  let (templatesList, concreteDefs) = List.partition isGeneric defs
      templatesMap = HM.fromList $ map (\d -> (getDefName d, d)) templatesList

  fs <- findFunc (Program n concreteDefs)
  ss <- findStruct (Program n defs)

  let initialState = SemState 
        { stFuncs = fs
        , stTemplates = templatesMap
        , stNewDefs = []
        , stInstantiated = HM.empty
        , stStructs = trace (show fs) ss
        }

  (defs', finalState) <- runStateT (mapM verifTopLevel concreteDefs) initialState
  let allDefs = defs' <> stNewDefs finalState
      finalFuncStack = mangleFuncStack (stFuncs finalState)

  pure (Program n allDefs, finalFuncStack)

--
-- private
--

isGeneric :: TopLevelDef -> Bool
isGeneric (DefFunction _ params ret _) = ret == TypeAny || any ((== TypeAny) . paramType) params
isGeneric (DefOverride _ params ret _) = ret == TypeAny || any ((== TypeAny) . paramType) params
isGeneric _ = False

getDefName :: TopLevelDef -> String
getDefName (DefFunction n _ _ _) = n
getDefName (DefOverride n _ _ _) = n
getDefName (DefStruct n _ _) = n

mangleFuncStack :: FuncStack -> FuncStack
mangleFuncStack fs = HM.foldlWithKey' expandOverloads fs fs
  where
    expandOverloads acc name sigs
      | length sigs > 1 = foldr (addMangled name) acc sigs
      | otherwise = acc

    addMangled name (ret, args) acc =
        let mName = mangleName name ret args
        in HM.insert mName [(ret, args)] acc

--
-- verif
--

verifTopLevel :: TopLevelDef -> SemM TopLevelDef
verifTopLevel (DefFunction name params r_t body) = do
  fs <- gets stFuncs
  let vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params
      paramTypes = map paramType params

      name' = case HM.lookup name fs of
          Just sigs | length sigs > 1 -> mangleName name r_t paramTypes
          _ -> name

  body' <- verifScope vs body
  pure $ DefFunction name' params r_t body'

verifTopLevel (DefOverride name params r_t body) = do
  let paramTypes = map paramType params
      name' = mangleName name r_t paramTypes
      vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params

  body' <- verifScope vs body
  pure $ DefOverride name' params r_t body'

verifTopLevel (DefStruct name fields methods) = do
  methods' <- mapM (verifMethod name) methods
  pure $ DefStruct name fields methods'

-- | scope verification
-- NOTE: 'FuncStack' is read from State, 'VarStack' is passed locally
verifScope :: VarStack -> Block -> SemM Block
verifScope vs (StmtVarDecl pos v t e : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)
      SourcePos file line col = pos

  e'      <- verifExprWithContext t vs e

  let e_t = exprType s e'

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
      e_t = exprType s start

  vs'     <- lift $ either (Left . formatSemanticError) Right $ assignVarType vs v file line col e_t
  t'      <- lift $ either (Left . formatSemanticError) Right $ checkMultipleType v file line col t e_t

  start'  <- verifExpr vs' start
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
      e_t = exprType s iter

  vs'     <- lift $ either (Left . formatSemanticError) Right $ assignVarType vs v file line col e_t
  t'      <- lift $ either (Left . formatSemanticError) Right $ checkMultipleType v file line col t e_t

  iter'   <- verifExpr vs' iter
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
  let e_t = exprType s rv'
  vs'     <- lift $ either (Left . formatSemanticError) Right $ assignVarType vs lv file line col e_t
  stmts'  <- verifScope vs' stmts
  pure $ StmtAssignment pos (ExprVar pv lv) rv' : stmts'

verifScope vs (StmtAssignment pos lhs rv : stmts) = do
  lhs'    <- verifExpr vs lhs
  rv'     <- verifExpr vs rv
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

  -- Verify type compatibility for binary operations
  fs <- gets stFuncs
  ss <- gets stStructs
  let s = (fs, vs, ss)
      SourcePos file line col = pos
      leftType = exprType s l'
      rightType = exprType s r'

  case iHTBinary op leftType rightType of
    Left err -> lift $ Left $ formatSemanticError $ SemanticError file line col "binary operation type mismatch" err ["binary operation", "global context"]
    Right _ -> pure $ ExprBinary pos op l' r'

verifExprWithContext hint vs (ExprCall cPos (ExprVar _ fname) args) = do
  fs <- gets stFuncs
  ss <- gets stStructs
  let s = (fs, vs, ss)

  args' <- mapM (verifExpr vs) args
  let argTypes = map (exprType s) args'

  callOrInstantiate cPos hint s fname args' argTypes

verifExprWithContext hint vs (ExprCall cPos (ExprAccess _ (ExprVar vPos target) method) args) = do
  fs <- gets stFuncs
  ss <- gets stStructs
  let s = (fs, vs, ss)

  let targetType = exprType s (ExprVar vPos target)
  let mangledName = show targetType ++ "_" ++ method

  _ <- case HM.lookup mangledName fs of
    Just (_:_) -> pure ()
    _ -> lift $ Left $ printf "Method '%s' not found on type '%s'" method (show targetType)

  args' <- mapM (verifExpr vs) args
  let argTypes = map (exprType s) args'
  let allArgs = ExprVar vPos target : args'
      allArgTypes = targetType : argTypes
  callOrInstantiate cPos hint s mangledName allArgs allArgTypes

verifExprWithContext _ _ (ExprCall _ _ _) =
  lift $ Left "Invalid function call target"

verifExprWithContext hint vs (ExprStructInit pos name fields) = do
  fields' <- mapM (\(l, e) -> (l,) <$> verifExprWithContext hint vs e) fields
  pure $ ExprStructInit pos name fields'

verifExprWithContext _ _ expr = pure expr

verifMethod :: String -> TopLevelDef -> SemM TopLevelDef
verifMethod sName (DefFunction methodName params retType body) = do
  let params' = fixSelfType sName params
      paramTypes = map paramType params'
      vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params'
      baseName = sName ++ "_" ++ methodName
      mangledName = mangleName baseName retType paramTypes

  body' <- verifScope vs body
  pure $ DefFunction mangledName params' retType body'
verifMethod _ def = pure def

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
  modify $ \st -> st
    { stNewDefs      = stNewDefs st <> [def]
    , stFuncs        = HM.insertWith (<>) name [(retTy, argTys)] (stFuncs st)
    , stInstantiated = HM.insert name True (stInstantiated st)
    }

callOrInstantiate :: SourcePos -> Maybe Type -> Stack -> String -> [Expression] -> [Type] -> SemM Expression
callOrInstantiate pos hint s name args argTypes = do
  let SourcePos file line col = pos

  case checkParamType s name file line col args of
    Right foundName -> pure $ ExprCall pos (ExprVar pos foundName) args
    Left err -> do
      templates <- gets stTemplates
      case HM.lookup name templates of
        Just templateDef -> do
          targetExpr <- tryInstantiateTemplate templateDef name args argTypes hint
          pure $ ExprCall pos targetExpr args
        Nothing -> lift $ Left $ formatSemanticError err
