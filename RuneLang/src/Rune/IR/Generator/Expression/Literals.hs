module Rune.IR.Generator.Expression.Literals
  ( genLitInt,
    genLitFloat,
    genLitChar,
    genLitBool,
    genLitNull,
    genLitString,
  )
where

import Rune.IR.IRHelpers (newStringGlobal)
import Rune.IR.Nodes
  ( IRGen,
    IRInstruction (..),
    IROperand (..),
    IRType (..),
  )

--
-- public
--

genLitInt :: Int -> IRGen ([IRInstruction], IROperand, IRType)
genLitInt n = return ([], IRConstInt n, IRI32)

genLitFloat :: Double -> IRGen ([IRInstruction], IROperand, IRType)
genLitFloat f = return ([], IRConstFloat f, IRF32)

genLitChar :: Char -> IRGen ([IRInstruction], IROperand, IRType)
genLitChar c = return ([], IRConstChar c, IRChar)

genLitBool :: Bool -> IRGen ([IRInstruction], IROperand, IRType)
genLitBool b = return ([], IRConstBool b, IRBool)

genLitNull :: IRGen ([IRInstruction], IROperand, IRType)
genLitNull = return ([], IRConstNull, IRNull)

genLitString :: String -> IRGen ([IRInstruction], IROperand, IRType)
genLitString s = do
  stringName <- newStringGlobal s
  return ([], IRGlobal stringName (IRPtr IRChar), IRPtr IRChar)
