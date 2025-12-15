module Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , Stack
  , Templates
  ) where

import Data.HashMap.Strict (HashMap)
import Rune.AST.Nodes (Type, TopLevelDef)

--
-- public
--

type VarStack = HashMap String Type
type FuncStack = HashMap String [(Type, [Type])]
type Stack = (FuncStack, VarStack)

type Templates = HashMap String TopLevelDef
