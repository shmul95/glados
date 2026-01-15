{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.GenTopLevel
  ( genTopLevel,
    genFunction,
    genStruct,
    genStructMethod,
    genParam,
    resetFunctionState,
    clearFunctionState,
    ensureReturn,
    lastOrNothing
  )
where
#else
module Rune.IR.Generator.GenTopLevel
  ( genTopLevel,
  )
where
#endif

import Control.Monad.State (modify)
import Control.Monad.Except (throwError)
import Data.Map (empty, insert, fromList, union)
import Rune.AST.Nodes (Field (..), Parameter (..), TopLevelDef (..), Type (..), Visibility (..), Expression (..))
import Rune.IR.Generator.GenStatement (genStatement)
import Rune.IR.Generator.GenExpression (genExpression)
import Rune.IR.IRHelpers (astTypeToIRType, registerVar)
import Rune.IR.Nodes
  ( GenState (..),
    IRFunction (..),
    IRGen,
    IRInstruction (..),
    IROperand (IRParam),
    IRTopLevel (..),
    IRType (..),
  )

--
-- public
--

genTopLevel :: TopLevelDef -> IRGen [IRTopLevel]
genTopLevel def@DefFunction {} = genFunction def
genTopLevel str@DefStruct {} = genStruct str
genTopLevel DefSomewhere {} = pure []

--
-- private
--

-- | generate IR for a normal function
-- def foo(a: i32, b: f32) -> i32 { ... }
-- DEF foo(p_a: i32, p_b: f32)
genFunction :: TopLevelDef -> IRGen [IRTopLevel]
genFunction (DefFunction name params retType body isExport _ _ _) = do
  resetFunctionState name

  irParams <- mapM genParam params
  bodyInstrs <- concat <$> mapM genStatement body

  let irRetType = case retType of
                    TypeArray elemType -> IRPtr (IRArray (astTypeToIRType elemType) 0)
                    t -> astTypeToIRType t
      cleaned = ensureReturn irRetType bodyInstrs
      func = IRFunction name irParams (Just irRetType) cleaned isExport

  clearFunctionState
  pure [IRFunctionDef func]
genFunction x = throwError $ "genFunction called on non-function: received " ++ show x

-- | generate IR for a struct definition and its methods
-- | For static variables, it creates IRStaticVar definitions
-- STRUCT Vec2f { x: f32, y: f32 }
-- | generate IR for a struct definition and its methods
-- | For static variables, it creates IRStaticVar definitions
-- STRUCT Vec2f { x: f32, y: f32 }
genStruct :: TopLevelDef -> IRGen [IRTopLevel]
genStruct (DefStruct name fields methods _ _) = do
  let nonStaticFields = [f | f <- fields, not (fieldIsStatic f)]
      staticFields = [(n, t, d) | Field n t _ True d <- fields]

  irFields <- mapM convertFieldToIR nonStaticFields
  staticVars <- convertStaticToIRType name staticFields

  modify $ \s -> s
    { gsStructs = insert name irFields (gsStructs s)
    , gsStaticVars = fromList [(name ++ "_" ++ n, astTypeToIRType t) | (n, t, _) <- staticFields] `union` gsStaticVars s
    }

  methodDefs <- concat <$> mapM (genStructMethod name) methods
  pure $ IRStructDef name irFields : staticVars ++ methodDefs
genStruct _ = pure []

convertFieldToIR :: Field -> IRGen (String, IRType, Maybe IROperand)
convertFieldToIR (Field name typ _ _ mbExpr) = do
  mbOp <- case mbExpr of
    Just expr -> do
      (_, op, _) <- genExpression expr
      pure $ Just op
    Nothing -> pure Nothing
  pure (name, astTypeToIRType typ, mbOp)

convertStaticToIRType :: String -> [(String, Type, Maybe Expression)] -> IRGen [IRTopLevel]
convertStaticToIRType sName = mapM (\(n, t, mbExpr) -> do
  mbOp <- case mbExpr of
    Just expr -> do
      (_, op, _) <- genExpression expr
      pure $ Just op
    Nothing -> pure Nothing
  pure $ IRStaticVar (sName ++ "_" ++ n) (astTypeToIRType t) mbOp)

-- | generate IR for a struct method
genStructMethod :: String -> TopLevelDef -> IRGen [IRTopLevel]
genStructMethod _ (DefFunction methName params retType body _ _ isStatic isAbstract) =
  genFunction (DefFunction methName params retType body False Public isStatic isAbstract)
genStructMethod _ _ = pure []

--
-- helpers
--

-- | generate IR for a function parameter and register it in the symbol table
genParam :: Parameter -> IRGen (String, IRType)
genParam (Parameter name typ _) = do
  let irType = case typ of
                 TypeArray elemType -> IRPtr (IRArray (astTypeToIRType elemType) 0)
                 TypeCustom s -> IRPtr (IRStruct s)
                 t -> astTypeToIRType t
      irName = "p_" ++ name

  registerVar name (IRParam irName irType) irType
  pure (irName, irType)

resetFunctionState :: String -> IRGen ()
resetFunctionState name =
  modify $ \s ->
    s
      { gsCurrentFunc = Just name,
        gsSymTable = empty,
        gsTempCounter = 0
      }

clearFunctionState :: IRGen ()
clearFunctionState = modify $ \s -> s {gsCurrentFunc = Nothing}

ensureReturn :: IRType -> [IRInstruction] -> [IRInstruction]
ensureReturn IRNull instrs =
  case lastOrNothing instrs of
    Just (IRRET _) -> instrs
    _ -> instrs ++ [IRRET Nothing]
ensureReturn _ instrs = instrs

lastOrNothing :: [a] -> Maybe a
lastOrNothing [] = Nothing
lastOrNothing xs = Just $ last xs
