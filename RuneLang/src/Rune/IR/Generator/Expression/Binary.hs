{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Binary
  ( genBinary,
    getResultType,
    mkInstr
  )
where
#else
module Rune.IR.Generator.Expression.Binary (genBinary) where
#endif

import Rune.AST.Nodes (BinaryOp (..), Expression)
import Rune.IR.IRHelpers (newTemp)
import Rune.IR.Nodes (IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- public
--

genBinary :: GenExprCallback -> BinaryOp -> Expression -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genBinary genExpr op left right = do
  (lInstrs, lOp, lType) <- genExpr left
  (rInstrs, rOp, _) <- genExpr right

  let resultType = getResultType op lType
  resultTemp <- newTemp "t" resultType
  let opInstr = mkInstr op resultTemp lOp rOp lType

  return (lInstrs <> rInstrs <> [opInstr], IRTemp resultTemp resultType, resultType)

--
-- private
--

getResultType :: BinaryOp -> IRType -> IRType
getResultType Eq _ = IRBool
getResultType Neq _ = IRBool
getResultType Lt _ = IRBool
getResultType Lte _ = IRBool
getResultType Gt _ = IRBool
getResultType Gte _ = IRBool
getResultType And _ = IRBool
getResultType Or _ = IRBool
getResultType _ t = t

mkInstr :: BinaryOp -> String -> IROperand -> IROperand -> IRType -> IRInstruction
mkInstr Add = IRADD_OP
mkInstr Sub = IRSUB_OP
mkInstr Mul = IRMUL_OP
mkInstr Div = IRDIV_OP
mkInstr Mod = IRMOD_OP
mkInstr And = IRAND_OP
mkInstr Or = IROR_OP
mkInstr Eq = \r a b _ -> IRCMP_EQ r a b
mkInstr Neq = \r a b _ -> IRCMP_NEQ r a b
mkInstr Lt = \r a b _ -> IRCMP_LT r a b
mkInstr Lte = \r a b _ -> IRCMP_LTE r a b
mkInstr Gt = \r a b _ -> IRCMP_GT r a b
mkInstr Gte = \r a b _ -> IRCMP_GTE r a b
