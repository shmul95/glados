module Rune.Backend.Types
  ( Asm,
    Extern,
    Function,
    Global,
    Struct,
    Static,
    AsmState (..),
  )
where

import Control.Monad.State.Strict (State)
import Data.Map.Strict (Map)
import Rune.IR.Nodes (IRFunction, IRGlobalValue, IRType (..), IROperand)
import Rune.AST.Nodes (Expression)

--
-- public
--

type Asm = State AsmState

type Extern = String

type Function = IRFunction

type Global = (String, IRGlobalValue)

type Static = (String, IRType, Maybe IROperand)

type Struct = [(String, IRType, Maybe Expression)]

data AsmState = AsmState
  { asOutput :: [String],
    asStackMap :: Map String Int,
    asAllocMap :: Map String Int,
    asStructs :: Map String Struct,
    asExterns :: [Extern]
  }
