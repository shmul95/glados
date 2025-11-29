{-# LANGUAGE LambdaCase #-}

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
  where
    initialState = GenState 0 0 0 [] Nothing empty
