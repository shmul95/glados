module Rune.IR.Generator (generateIR) where

import Control.Monad.State (runState)
import Data.Map (empty)
import qualified Data.Set as Set
import Rune.AST.Nodes (Program (..))
import Rune.IR.Generator.GenTopLevel (genTopLevel)
import Rune.IR.Nodes (GenState (..), IRFunction (..), IRProgram (..), IRTopLevel (..))

--
-- public
--

generateIR :: Program -> IRProgram
generateIR (Program name defs) =
  let (irDefs, finalState) = runState (mapM genTopLevel defs) initialState

      -- INFO: gather all generated definitions (globals & functions)
      generatedDefs = reverse (gsGlobals finalState) ++ concat irDefs

      -- INFO: only function definitions contribute to defined functions
      definedFuncs = Set.fromList $ concatMap getDefinedFuncName generatedDefs

      -- INFO: gather all called functions
      calledFuncs = gsCalledFuncs finalState

      -- INFO: determine external functions (called but not defined)
      externs = Set.difference calledFuncs definedFuncs
      externDefs = map IRExtern (Set.toList externs)

      allDefs = externDefs ++ generatedDefs
   in IRProgram name allDefs

--
-- private
--

initialState :: GenState
initialState =
  GenState
    { gsTempCounter = 0,
      gsLabelCounter = 0,
      gsStringCounter = 0,
      gsGlobals = [],
      gsCurrentFunc = Nothing,
      gsSymTable = empty,
      gsStructs = empty,
      gsLoopStack = [],
      gsCalledFuncs = Set.empty,
      gsStringMap = empty
    }

getDefinedFuncName :: IRTopLevel -> [String]
getDefinedFuncName (IRFunctionDef (IRFunction n _ _ _)) = [n]
getDefinedFuncName _ = []
