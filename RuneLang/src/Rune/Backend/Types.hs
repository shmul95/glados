module Rune.Backend.Types
  ( Asm,
    Extern,
    Function,
    Global,
    Struct,
    AsmState (..),
  )
where

import Control.Monad.State.Strict (State)
import Data.Map.Strict (Map)
import Rune.IR.Nodes (IRFunction, IRGlobalValue, IRType (..))

--
-- public
--

type Asm = State AsmState

type Extern = String

type Function = IRFunction

type Global = (String, IRGlobalValue)

type Struct = [(String, IRType)]

data AsmState = AsmState
  { asOutput :: [String],
    asStackMap :: Map String Int,
    asAllocMap :: Map String Int,
    asStructs :: Map String Struct,
    asExterns :: [Extern]
  }
