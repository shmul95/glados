module Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , StructStack
  , Stack
  , Templates
  ) where

import Data.HashMap.Strict (HashMap)
import Rune.AST.Nodes (Parameter, Type, TopLevelDef)

--
-- public
--

type VarStack = HashMap String Type
type FuncStack = HashMap String (Type, [Parameter])
type StructStack = HashMap String TopLevelDef
type Stack = (FuncStack, VarStack, StructStack)

type Templates = HashMap String TopLevelDef
