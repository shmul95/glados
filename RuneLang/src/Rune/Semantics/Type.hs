module Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , StructStack
  , Stack
  , Templates
  ) where

import Data.HashMap.Strict (HashMap)
import Rune.AST.Nodes (Type, TopLevelDef)

--
-- public
--

type VarStack = HashMap String Type
-- | FuncStack: functionName -> (returnType, paramTypes, variadicType)
-- variadicType is Nothing for non-variadic functions, Just Type for variadic
type FuncStack = HashMap String (Type, [Type], Maybe Type)
type StructStack = HashMap String TopLevelDef
type Stack = (FuncStack, VarStack, StructStack)

type Templates = HashMap String TopLevelDef
