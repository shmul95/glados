{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.GenTopLevel
  ( genTopLevel,
    genFunction,
    genOverride,
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
import Data.Map (empty, insert)
import Rune.AST.Nodes (Field (..), Parameter (..), TopLevelDef (..), Type (..))
import Rune.IR.Generator.GenStatement (genStatement)
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
genTopLevel ovr@DefOverride {} = genOverride ovr
genTopLevel str@DefStruct {} = genStruct str
genTopLevel DefSomewhere {} = pure []

--
-- private
--

-- | generate IR for a normal function
-- def foo(a: i32, b: f32) -> i32 { ... }
-- DEF foo(p_a: i32, p_b: f32)
genFunction :: TopLevelDef -> IRGen [IRTopLevel]
genFunction (DefFunction name params retType body isExport) = do
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

-- | generate IR for an override function
-- show(Vec2f) -> show_Vec2f
genOverride :: TopLevelDef -> IRGen [IRTopLevel]
genOverride (DefOverride name params retType body isExport) =
  genFunction (DefFunction name params retType body isExport)
genOverride _ = throwError "genOverride called on non-override"

-- | generate IR for a struct definition and its methods
-- struct Vec2f { x: f32, y: f32 }
-- STRUCT Vec2f { x: f32, y: f32 }
genStruct :: TopLevelDef -> IRGen [IRTopLevel]
genStruct (DefStruct name fields methods) = do
  let irFields = map (\(Field n t) -> (n, astTypeToIRType t)) fields
  modify $ \s -> s {gsStructs = insert name irFields (gsStructs s)}
  methodDefs <- concat <$> mapM (genStructMethod name) methods
  pure $ IRStructDef name irFields : methodDefs
genStruct _ = pure []

-- | generate IR for a struct method
genStructMethod :: String -> TopLevelDef -> IRGen [IRTopLevel]
genStructMethod _ (DefFunction methName params retType body _) =
  genFunction (DefFunction methName params retType body False)
genStructMethod _ (DefOverride methName params retType body _) =
  genFunction (DefFunction methName params retType body False)
genStructMethod _ _ = pure []

--
-- helpers
--

-- | generate IR for a function parameter and register it in the symbol table
genParam :: Parameter -> IRGen (String, IRType)
genParam (Parameter name typ) = do
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
