{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Semantics.Generic (
  instantiate,
  replaceParam,
  replaceAny
) where
#else
module Rune.Semantics.Generic (instantiate) where
#endif

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
replaceParam (Parameter n pType) argType = Parameter n (replaceAny pType argType)

replaceAny :: Type -> Type -> Type
replaceAny TypeAny t = t
replaceAny (TypeArray TypeAny) (TypeArray t) = TypeArray t
replaceAny (TypeArray TypeAny) t = TypeArray t
replaceAny t _ = t
