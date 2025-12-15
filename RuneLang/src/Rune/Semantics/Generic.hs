module Rune.Semantics.Generic (instantiate) where

import Rune.AST.Nodes
import Rune.Semantics.Helper (mangleName)

--
-- public
--

instantiate :: TopLevelDef -> [Type] -> Type -> TopLevelDef
instantiate (DefFunction name params _ body) argTypes retType =
    let 
        mangled = mangleName name retType argTypes
        newParams = zipWith replaceParam params argTypes
    in DefFunction mangled newParams retType body
instantiate (DefOverride name params _ body) argTypes retType =
    let 
        mangled = mangleName name retType argTypes
        newParams = zipWith replaceParam params argTypes
    in DefOverride mangled newParams retType body
instantiate def _ _ = def

--
-- private
--

replaceParam :: Parameter -> Type -> Parameter
replaceParam (Parameter n TypeAny) t = Parameter n t
replaceParam p _ = p
