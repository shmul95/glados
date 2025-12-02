module Rune.IR.Generator (generateIR) where

import Control.Monad.State (runState)
import Data.Map (empty)
import Rune.AST.Nodes (Program (..))
import Rune.IR.Generator.GenTopLevel (genTopLevel)
import Rune.IR.Nodes (GenState (..), IRProgram (..))

--
-- public
--

generateIR :: Program -> IRProgram
generateIR (Program name defs) =
  let (irDefs, finalState) = runState (mapM genTopLevel defs) initialState
      allDefs = reverse (gsGlobals finalState) ++ concat irDefs
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
      gsLoopStack = []
    }
