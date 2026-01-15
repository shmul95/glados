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

--
-- public
--

instantiate :: TopLevelDef -> [Type] -> Type -> TopLevelDef
instantiate (DefFunction name params _ body isExport) argTypes retType =
    let newParams = zipWith replaceParam params argTypes
    in DefFunction name newParams retType body isExport
instantiate def _ _ = def

--
-- private
--

replaceParam :: Parameter -> Type -> Parameter
replaceParam (Parameter n pType pDefault) argType = Parameter n (replaceAny pType argType) pDefault

replaceAny :: Type -> Type -> Type
replaceAny TypeAny t = t
replaceAny (TypeArray TypeAny) (TypeArray t) = TypeArray t
replaceAny (TypeArray TypeAny) t = TypeArray t
replaceAny t _ = t
