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
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

import Text.Printf (printf)

import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc)
import Rune.Semantics.Struct (checkFields)
import Rune.Semantics.Generic (instantiate)

import Rune.Semantics.Type
  ( FuncStack
  , VarStack
  , StructStack
  , Templates
  )

import Rune.Semantics.Helper
  ( checkParamTypeWithReturnContext
  , mangleName
  , exprType
  , assignVarType
  , checkMultipleType
  )

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

  let initialState = SemState
        { stFuncs = fs
        , stTemplates = templatesMap
        , stNewDefs = []
        , stInstantiated = HM.empty
        , stStructs = HM.empty
        }

  (defs', finalState) <- runStateT (mapM verifTopLevel concreteDefs) initialState
  let allDefs = defs' <> stNewDefs finalState
      finalFs = mangleFuncStack (stFuncs finalState)

  pure (Program n allDefs, finalFs)

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
  ss     <- gets stStructs
  when (HM.member name ss) $
    lift $ Left $ printf "Struct '%s' is already defined" name
  fields'  <- lift $ checkFields name ss fields
  methods' <- mapM (verifMethod name) methods

  modify $ \s -> s
    { stStructs = HM.insert name (DefStruct name fields' methods') (stStructs s)
    }
  pure $ DefStruct name fields' methods'

-- | scope verification
-- NOTE: 'FuncStack' is read from State, 'VarStack' is passed locally
verifScope :: VarStack -> Block -> SemM Block
verifScope vs (StmtVarDecl v t e : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)

  e'      <- verifExprWithContext t vs e

  e_t     <- lift $ exprType s e'

  t'      <- lift $ checkMultipleType v t e_t
  vs'     <- lift $ assignVarType vs v t'

  stmts'  <- verifScope vs' stmts
  pure $ StmtVarDecl v (Just t') e' : stmts'

verifScope vs (StmtExpr e : stmts) = do
  e'      <- verifExpr vs e
  stmts'  <- verifScope vs stmts
  pure $ StmtExpr e' : stmts'

verifScope vs (StmtReturn (Just e) : stmts) = do
  e'      <- verifExpr vs e
  stmts'  <- verifScope vs stmts
  pure $ StmtReturn (Just e') : stmts'

verifScope vs (StmtReturn Nothing : stmts) = do
  stmts'  <- verifScope vs stmts
  pure $ StmtReturn (Just ExprLitNull) : stmts'

verifScope vs (StmtIf cond a (Just b) : stmts) = do
  cond'   <- verifExpr vs cond
  a'      <- verifScope vs a
  b'      <- verifScope vs b
  stmts'  <- verifScope vs stmts
  pure $ StmtIf cond' a' (Just b') : stmts'

verifScope vs (StmtIf cond a Nothing : stmts) = do
  cond'   <- verifExpr vs cond
  a'      <- verifScope vs a
  stmts'  <- verifScope vs stmts
  pure $ StmtIf cond' a' Nothing : stmts'

verifScope vs (StmtFor v t (Just start) end body : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)
  e_t     <- lift $ exprType s start

  vs'     <- lift $ assignVarType vs v e_t
  t'      <- lift $ checkMultipleType v t e_t

  start'  <- verifExpr vs' start
  end'    <- verifExpr vs' end
  body'   <- verifScope vs' body
  stmts'  <- verifScope vs stmts
  pure $ StmtFor v (Just t') (Just start') end' body' : stmts'

verifScope vs (StmtFor v t Nothing end body : stmts) = do
  let e_t = fromMaybe TypeAny t
  vs'     <- lift $ assignVarType vs v e_t
  t'      <- lift $ checkMultipleType v t e_t
  end'    <- verifExpr vs' end
  body'   <- verifScope vs' body
  stmts'  <- verifScope vs stmts
  pure $ StmtFor v (Just t') Nothing end' body' : stmts'

verifScope vs (StmtForEach v t iter body : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)
  e_t     <- lift $ exprType s iter

  vs'     <- lift $ assignVarType vs v e_t
  t'      <- lift $ checkMultipleType v t e_t

  iter'   <- verifExpr vs' iter
  body'   <- verifScope vs' body
  stmts'  <- verifScope vs stmts
  pure $ StmtForEach v (Just t') iter' body' : stmts'

verifScope vs (StmtLoop body : stmts) = do
  body'   <- verifScope vs body
  stmts'  <- verifScope vs stmts
  pure $ StmtLoop body' : stmts'

verifScope vs (StmtAssignment (ExprVar lv) rv : stmts) = do
  fs      <- gets stFuncs
  ss      <- gets stStructs
  let s   = (fs, vs, ss)

  rv'     <- verifExpr vs rv
  e_t     <- lift $ exprType s rv'
  vs'     <- lift $ assignVarType vs lv e_t
  stmts'  <- verifScope vs' stmts
  pure $ StmtAssignment (ExprVar lv) rv' : stmts'

verifScope vs (StmtAssignment lhs rv : stmts) = do
  lhs'    <- verifExpr vs lhs
  rv'     <- verifExpr vs rv
  stmts'  <- verifScope vs stmts
  pure $ StmtAssignment lhs' rv' : stmts'

verifScope vs (StmtStop : stmts) = do
  stmts'  <- verifScope vs stmts
  pure $ StmtStop : stmts'

verifScope vs (StmtNext : stmts) = do
  stmts'  <- verifScope vs stmts
  pure $ StmtNext : stmts'

verifScope _ [] = pure []

-- | expression verification
verifExpr :: VarStack -> Expression -> SemM Expression
verifExpr = verifExprWithContext Nothing

verifExprWithContext :: Maybe Type -> VarStack -> Expression -> SemM Expression
verifExprWithContext hint vs (ExprUnary op val) = do
  val'    <- verifExprWithContext hint vs val
  pure $ ExprUnary op val'

verifExprWithContext hint vs (ExprBinary op l r) = do
  l'      <- verifExprWithContext hint vs l
  r'      <- verifExprWithContext hint vs r
  pure $ ExprBinary op l' r'

verifExprWithContext hint vs (ExprCall (ExprVar fname) args) = do
  fs <- gets stFuncs
  ss <- gets stStructs
  let s = (fs, vs, ss)

  args' <- trace ("FuncStack: " ++ show fs) $ mapM (verifExpr vs) args
  argTypes <- lift $ mapM (exprType s) args'

  let match = checkParamTypeWithReturnContext hint s fname args'
  case match of
    Right foundName -> pure $ ExprCall (ExprVar foundName) args'
    Left _ -> do
      templates <- gets stTemplates
      case HM.lookup fname templates of
        Nothing -> lift $ Left $ printf "Function %s not found (neither concrete nor generic)" fname
        Just templateDef -> do
          targetExpr <- tryInstantiateTemplate templateDef fname args' argTypes hint
          pure $ ExprCall targetExpr args'

verifExprWithContext hint vs (ExprCall (ExprAccess (ExprVar target) method) args) = do
  fs <- gets stFuncs
  ss <- gets stStructs
  let s = (fs, vs, ss)

  args' <- mapM (verifExpr vs) args
  argTypes <- lift $ mapM (exprType s) args'
  targetType <- trace (show argTypes) lift $ exprType s (ExprVar target)

  let name = show targetType ++ "_" ++ method
      match = trace ("ARGS TYPES: " ++ show argTypes ++ "/\\ ARGS: " ++ show args') checkParamTypeWithReturnContext hint s name args'
  case match of
    Right foundName -> pure $ ExprCall (ExprVar foundName) args'
    Left _ -> do
      templates <- gets stTemplates
      case HM.lookup name templates of
        Nothing -> lift $ Left $ printf "Function %s not found (neither concrete nor generic)" name
        Just templateDef -> do
          targetExpr <- tryInstantiateTemplate templateDef name args' argTypes hint
          pure $ ExprCall targetExpr args'

verifExprWithContext _ _ (ExprCall _ _) =
  lift $ Left "Invalid function call target"

verifExprWithContext hint vs (ExprStructInit name fields) = do
  fields' <- mapM (\(l, e) -> (l,) <$> verifExprWithContext hint vs e) fields
  pure $ ExprStructInit name fields'

verifExprWithContext _ vs (ExprVar var)
  | HM.member var vs = pure (ExprVar var)
  | otherwise       = lift $ Left $ printf msg var
  where
    msg = "\n\tUndefinedVar: %s doesn't exist in the scope"

verifExprWithContext _ _ expr = pure expr

verifMethod :: String -> TopLevelDef -> SemM TopLevelDef
verifMethod sName (DefFunction methodName params retType body) = do
  fs <- gets stFuncs
  let params' = fixSelfType sName params
      paramTypes = map paramType params'
      vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params'
      mangledName = sName ++ "_" ++ methodName

  when (HM.member mangledName fs) $
    lift $ Left $ printf "Method '%s' in struct '%s' is already defined" methodName sName

  body' <- verifScope vs body

  modify $ \st -> st
    { stFuncs = HM.insertWith (<>) mangledName [(retType, paramTypes)] (stFuncs st)
    }
  pure $ DefFunction mangledName params' retType body'
verifMethod _ def = pure def

--
-- instanciation
--

tryInstantiateTemplate :: TopLevelDef -> String -> [Expression] -> [Type] -> Maybe Type -> SemM Expression
tryInstantiateTemplate def originalName _ argTypes contextRetType = do
  retTy <- resolveReturnType originalName argTypes contextRetType
  let mangled = mangleName originalName retTy argTypes

  alreadyInstantiated mangled >>= \case
    True  -> pure $ ExprVar mangled
    False -> do
      verified <- verifTopLevel (instantiate def argTypes retTy)
      registerInstantiation mangled verified retTy argTypes
      pure $ ExprVar mangled

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

---
--- Helpers
---

fixSelfType :: String -> [Parameter] -> [Parameter]
fixSelfType sName (p:rest)
  | paramName p == "self" = p { paramType = TypeCustom sName } : rest
fixSelfType _ params = params
