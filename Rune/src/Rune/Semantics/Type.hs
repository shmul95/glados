module Rune.Semantics.Type
  ( VarStack
  , FuncStack
  , Stack
  ) where

import Data.HashMap.Strict (HashMap)

import Rune.AST.Nodes

type VarStack = HashMap String Type
type FuncStack = HashMap String [(Type, [Type])]
type Stack = (FuncStack, VarStack)

