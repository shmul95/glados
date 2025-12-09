module Rune.Backend.Types
  ( Asm,
    Extern,
    Function,
    GlobalString,
    Struct,
    AsmState (..),
  )
where

import Control.Monad.State.Strict (State)
import Data.Map.Strict (Map)
import Rune.IR.Nodes (IRFunction, IRType (..))

--
-- public
--

type Asm = State AsmState

type Extern = String

type Function = IRFunction

type GlobalString = (String, String)

type Struct = [(String, IRType)]

data AsmState = AsmState
  { asOutput :: [String],
    asStackMap :: Map String Int,
    asAllocMap :: Map String Int,
    asStructs :: Map String Struct,
    asExterns :: [Extern]
  }
