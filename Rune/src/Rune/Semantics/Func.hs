module Rune.Semantics.Func ( findFunc, FuncStack ) where

import Rune.AST.Nodes
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

type FuncStack = HashMap String (Type, [Type])

findFunc :: Program -> FuncStack
findFunc (Program _ defs) = foldl findDefs HM.empty defs

findDefs :: FuncStack -> TopLevelDef -> FuncStack
findDefs s (DefFunction name params rType _) =
    let paramTypes = map paramType params
    in  HM.insert name (rType, paramTypes) s
findDefs s (DefOverride name params rType _) =
    let paramTypes = map paramType params
    in  HM.insert name (rType, paramTypes) s
findDefs s _ = s

