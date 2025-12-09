module Rune.IR.Generator.GenTopLevel
  ( genTopLevel,
  )
where

import Control.Monad.State (modify)
import Data.Map (empty, insert)
import Rune.AST.Nodes (Field (..), Parameter (..), TopLevelDef (..), Type (..))
import Rune.IR.Generator.GenStatement (genStatement)
import Rune.IR.IRHelpers (astTypeToIRType, mangleMethodName, registerVar)
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

--
-- private
--

-- | generate IR for a normal function
-- def foo(a: i32, b: f32) -> i32 { ... }
-- DEF foo(p_a: i32, p_b: f32)
genFunction :: TopLevelDef -> IRGen [IRTopLevel]
genFunction (DefFunction name params retType body) = do
  resetFunctionState name

  irParams <- mapM genParam params
  bodyInstrs <- concat <$> mapM genStatement body

  let irRetType = astTypeToIRType retType
      cleaned = ensureReturn irRetType bodyInstrs
      func = IRFunction name irParams (Just irRetType) cleaned

  clearFunctionState
  pure [IRFunctionDef func]
genFunction x = error $ "genFunction called on non-function: received " ++ show x

-- | generate IR for an override function
-- show(Vec2f) -> show_Vec2f
genOverride :: TopLevelDef -> IRGen [IRTopLevel]
genOverride (DefOverride name params retType body) = do
  let mangledName = case params of
        (Parameter _ (TypeCustom s) : _) -> mangleMethodName name s
        _ -> name
  genFunction (DefFunction mangledName params retType body)
genOverride _ = error "genOverride called on non-override"

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
-- Vec2f.magnitude() -> magnitude_Vec2f
genStructMethod :: String -> TopLevelDef -> IRGen [IRTopLevel]
genStructMethod structName' (DefFunction methName params retType body) = do
  let mangledName = mangleMethodName structName' methName
      typedParams = map (fixSelfParam structName') params
  genFunction (DefFunction mangledName typedParams retType body)
genStructMethod _ _ = pure []

--
-- helpers
--

-- | generate IR for a function parameter and register it in the symbol table
genParam :: Parameter -> IRGen (String, IRType)
genParam (Parameter name typ) = do
  let irType = astTypeToIRType typ
      finalType = case irType of
        IRStruct s -> IRPtr (IRStruct s)
        _ -> irType
      irName = "p_" ++ name

  registerVar name (IRParam irName finalType) finalType
  pure (irName, finalType)

-- | self: *StructType
fixSelfParam :: String -> Parameter -> Parameter
fixSelfParam sName (Parameter "self" _) = Parameter "self" (TypeCustom sName)
fixSelfParam _ p = p

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
