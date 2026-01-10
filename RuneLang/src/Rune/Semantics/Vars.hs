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

import Control.Monad (unless)
import Control.Monad.State.Strict

import Text.Printf (printf)

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

import Rune.AST.Nodes
import Rune.Semantics.Func (findFunc)
import Rune.Semantics.Generic (instantiate)

import Rune.Semantics.Type
  ( FuncStack
  , VarStack
  , Templates
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
        }

  (defs', finalState) <- runStateT (mapM verifTopLevel concreteDefs) initialState
  let allDefs = defs' <> stNewDefs finalState
      finalFuncStack = mangleFuncStack $ stFuncs finalState
  
  pure (Program n allDefs, finalFuncStack)

--
-- private
--

isGeneric :: TopLevelDef -> Bool
isGeneric (DefFunction _ params ret _ _) = hasAny ret || any (hasAny . paramType) params
isGeneric (DefOverride {}) = False
isGeneric _ = False


hasAny :: Type -> Bool
hasAny TypeAny = True
hasAny (TypeArray t) = hasAny t
hasAny _ = False


getDefName :: TopLevelDef -> String
getDefName (DefFunction n _ _ _ _) = n
getDefName (DefOverride n _ _ _ _) = n
getDefName (DefStruct n _ _) = n
getDefName (DefSomewhere {}) = ""


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
verifTopLevel (DefFunction name params r_t body isExport) = do
  fs <- gets stFuncs
  let vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params
      paramTypes = map paramType params

      name' = case HM.lookup name fs of
          Just sigs | length sigs > 1 -> mangleName name r_t paramTypes
          _ -> name

  body' <- verifScope vs body
  pure $ DefFunction name' params r_t body' isExport

verifTopLevel (DefOverride name params r_t body isExport) = do
  let paramTypes = map paramType params
      name' = mangleName name r_t paramTypes
      vs = HM.fromList $ map (\p -> (paramName p, paramType p)) params

  body' <- verifScope vs body
  pure $ DefOverride name' params r_t body' isExport

verifTopLevel def = pure def -- Structs & Somewhere

-- | scope verification
-- NOTE: 'FuncStack' is read from State, 'VarStack' is passed locally
verifScope :: VarStack -> Block -> SemM Block
verifScope vs (StmtVarDecl pos v t e : stmts) = do
  fs      <- gets stFuncs
  let s   = (fs, vs)
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
  let s   = (fs, vs)
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
  let s   = (fs, vs)
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
  let s   = (fs, vs)
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
  let s   = (fs, vs)
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
  let s = (fs, vs)
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

verifExprWithContext hint vs (ExprCall pos name args) = do
  fs <- gets stFuncs
  let s   = (fs, vs)
      SourcePos file line col = pos

  args' <- mapM (verifExpr vs) args
  
  let argTypesResult = mapM (exprType s) args'
  case argTypesResult of
    Left err -> lift $ Left $ formatSemanticError $ SemanticError file line col "valid argument types" err ["function call"]
    Right argTypes -> do
      let match = checkParamType s name file line col args'
      case match of
        Right foundName -> pure $ ExprCall pos foundName args'
        Left err -> do
            templates <- gets stTemplates
            case HM.lookup name templates of
                Nothing -> lift $ Left $ formatSemanticError err
                Just templateDef -> tryInstantiateTemplate templateDef name args' argTypes hint

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
    True  -> pure $ ExprCall pos mangled args
    False -> do
      verified <- verifTopLevel (instantiate def argTypes retTy)
      registerInstantiation mangled verified retTy argTypes
      pure $ ExprCall pos mangled args


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
