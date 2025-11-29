module Rune.IR.Generator.GenTopLevel
  ( genTopLevel,
  )
where

import Control.Monad.State (modify)
import Data.Map (empty)
import Rune.AST.Nodes (Parameter (..), Statement, TopLevelDef (..), Type)
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
  genFunction name params retType body
genTopLevel (DefStruct _ _ methods) =
  concat <$> mapM genTopLevel methods

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

genParam :: Parameter -> IRGen (String, IRType)
genParam (Parameter name typ) = do
  let irType = astTypeToIRType typ
      irName = "p_" ++ name
  registerVar name (IRParam irName irType) irType
  pure (irName, irType)

--
-- private helpers
--

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
