module Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , StructStack
  , Stack
  , Templates
  ) where

import Data.HashMap.Strict (HashMap)
import Rune.AST.Nodes (Parameter, Type, TopLevelDef, Visibility)

--
-- public
--

type VarStack = HashMap String Type
type FuncStack = HashMap String ((Type, [Parameter]), Visibility, Bool) -- Bool indicates if it's a static or not (True for static, False otherwise)
type StructStack = HashMap String TopLevelDef
type Stack = (FuncStack, VarStack, StructStack)

type Templates = HashMap String TopLevelDef
