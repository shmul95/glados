module Rune.IR.Generator.GenTopLevel
  ( genTopLevel,
  )
where

import Control.Monad.State (modify)
import Data.Map (empty, insert)
import Rune.AST.Nodes (Field (..), Parameter (..), Statement, TopLevelDef (..), Type (..))
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
genTopLevel (DefFunction name params retType body) =
  genFunction name params retType body
genTopLevel (DefOverride name params retType body) =
  genOverride name params retType body
genTopLevel (DefStruct name fields methods) = do
  let irFields = map (\(Field n t) -> (n, astTypeToIRType t)) fields
  modify $ \s -> s {gsStructs = insert name irFields (gsStructs s)}
  methodDefs <- concat <$> mapM (genStructMethod name) methods
  pure $ IRStructDef name irFields : methodDefs

--
-- private generators
--

genFunction :: String -> [Parameter] -> Type -> [Statement] -> IRGen [IRTopLevel]
genFunction name params retType body = do
  resetFunctionState name

  irParams <- mapM genParam params
  bodyInstrs <- concat <$> mapM genStatement body

  let irRetType = astTypeToIRType retType
      cleaned = ensureReturn irRetType bodyInstrs
      func = IRFunction name irParams (Just irRetType) cleaned

  clearFunctionState
  pure [IRFunctionDef func]

genOverride :: String -> [Parameter] -> Type -> [Statement] -> IRGen [IRTopLevel]
genOverride name params retType body = do
  let mangledName = case params of
        (Parameter _ (TypeCustom s) : _) -> name ++ "_" ++ s
        _ -> name
  genFunction mangledName params retType body

genStructMethod :: String -> TopLevelDef -> IRGen [IRTopLevel]
genStructMethod structName' (DefFunction methName params retType body) = do
  let mangledName = structName' ++ "_" ++ methName
  let typedParams = map (fixSelfParam structName') params

  genFunction mangledName typedParams retType body
genStructMethod _ _ = pure []

genParam :: Parameter -> IRGen (String, IRType)
genParam (Parameter name typ) = do
  let irType = astTypeToIRType typ
      finalType = case irType of
        IRStruct s -> IRPtr (IRStruct s)
        _ -> irType
      irName = "p_" ++ name

  registerVar name (IRParam irName finalType) finalType
  pure (irName, finalType)

--
-- private helpers
--

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
clearFunctionState =
  modify $ \s -> s {gsCurrentFunc = Nothing}

ensureReturn :: IRType -> [IRInstruction] -> [IRInstruction]
ensureReturn IRVoid instrs =
  case lastOrNothing instrs of
    Just (IRRET _) -> instrs
    _ -> instrs ++ [IRRET Nothing]
ensureReturn _ instrs = instrs

lastOrNothing :: [a] -> Maybe a
lastOrNothing [] = Nothing
lastOrNothing xs = Just (last xs)
