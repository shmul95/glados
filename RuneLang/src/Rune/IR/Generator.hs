{-# OPTIONS_GHC -cpp #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator
  ( generateIR,
    initialState,
    getDefinedFuncName
  )
where
#else
module Rune.IR.Generator (generateIR) where
#endif

import Control.Monad.State (runState)
import Control.Monad.Except (runExceptT)
import Data.Map (empty)
import qualified Data.Set as Set
import Rune.AST.Nodes (Program (..))
import Rune.IR.Generator.GenTopLevel (genTopLevel)
import Rune.IR.Nodes (GenState (..), IRFunction (..), IRProgram (..), IRTopLevel (..))
import Rune.Semantics.Type (FuncStack)

-- NOTE: uncomment for debugging
-- import Rune.AST.Printer (prettyPrint)
-- import Debug.Trace (trace)

--
-- public
--

generateIR :: Program -> FuncStack -> Either String IRProgram
generateIR (Program name defs) fs =
  -- NOTE: uncomment for debugging
  -- trace ("AST: " <> prettyPrint (Program name defs)) $
  let (result, finalState) = runState (runExceptT (mapM genTopLevel defs)) (initialState fs)
   in case result of
        Left err -> Left err
        Right irDefs -> 
          let -- INFO: gather all generated definitions (globals & functions)
              generatedDefs = reverse (gsGlobals finalState) ++ concat irDefs

              -- INFO: only function definitions contribute to defined functions
              definedFuncs = Set.fromList $ concatMap getDefinedFuncName generatedDefs

              -- INFO: gather all called functions
              calledFuncs = gsCalledFuncs finalState

              -- INFO: determine external functions (called but not defined)
              externs = Set.difference calledFuncs definedFuncs
              externDefs = map IRExtern (Set.toList externs)

              allDefs = externDefs ++ generatedDefs
           in Right $ IRProgram name allDefs

--
-- private
--

initialState :: FuncStack -> GenState
initialState fs =
  GenState
    { gsTempCounter = 0,
      gsLabelCounter = 0,
      gsStringCounter = 0,
      gsFloatCounter = 0,
      gsGlobals = [],
      gsCurrentFunc = Nothing,
      gsSymTable = empty,
      gsStructs = empty,
      gsLoopStack = [],
      gsCalledFuncs = Set.empty,
      gsStringMap = empty,
      gsFloatMap = empty,
      gsFuncStack = fs
    }

getDefinedFuncName :: IRTopLevel -> [String]
getDefinedFuncName (IRFunctionDef (IRFunction n _ _ _)) = [n]
getDefinedFuncName _ = []
