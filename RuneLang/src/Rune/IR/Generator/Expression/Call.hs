{-# OPTIONS_GHC -cpp #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Call
  ( genCall,
    prepareArg
  )
where
#else
module Rune.IR.Generator.Expression.Call (genCall) where
#endif

import Control.Monad.State (gets)
import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (registerCall, newTemp, selectReturnType)
import Rune.IR.Nodes (GenState(..), IRGen, IRInstruction (..), IROperand (..), IRType (..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- public
--

genCall :: GenExprCallback -> String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genCall genExpr funcName args = do
  argsData <- mapM genExpr args
  fs       <- gets gsFuncStack

  let (instrs, ops) = unzip $ map prepareArg argsData
      allInstrs = concat instrs
      argTypes = map (\(_, _, t) -> t) argsData
      retType = selectReturnType fs funcName argTypes

  registerCall funcName
  retTemp <- newTemp "t" retType
  let callInstr = IRCALL retTemp funcName ops $ Just retType

  return (allInstrs ++ [callInstr], IRTemp retTemp retType, retType)

--
-- private
--

prepareArg :: ([IRInstruction], IROperand, IRType) -> ([IRInstruction], IROperand)
prepareArg (i, IRTemp n t, IRStruct _) = (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
prepareArg (i, IRTemp n t, IRPtr (IRStruct _)) = (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
prepareArg (i, op, _) = (i, op)
