module Rune.IR.Generator.Statement.ControlFlow
  ( genIfNoElse,
    genIfElse,
    genStop,
    genNext,
  )
where

import Rune.AST.Nodes (Expression, Statement (..))
import Rune.IR.IRHelpers
  ( endsWithRet,
    getCurrentLoop,
    makeLabel,
    nextLabelIndex,
  )
import Rune.IR.Nodes
  ( IRGen,
    IRInstruction (..),
    IROperand (..),
    IRType (..),
  )

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

type GenBlockCallback = [Statement] -> IRGen [IRInstruction]

--
-- public
--

genIfNoElse :: GenExprCallback -> GenBlockCallback -> Expression -> [Statement] -> IRGen [IRInstruction]
genIfNoElse genExpr genBlock cond thenBlock = do
  (condInstrs, condOp, _) <- genExpr cond
  idx <- nextLabelIndex
  let endLbl = makeLabel "end" idx

  thenInstrs <- genBlock thenBlock

  pure $
    mconcat
      [ condInstrs,
        [IRJUMP_FALSE condOp endLbl],
        thenInstrs,
        [IRLABEL endLbl]
      ]

genIfElse :: GenExprCallback -> GenBlockCallback -> Expression -> [Statement] -> [Statement] -> IRGen [IRInstruction]
genIfElse genExpr genBlock cond thenBlock elseBlock = do
  (condInstrs, condOp, _) <- genExpr cond
  idx <- nextLabelIndex
  let elseLbl = makeLabel "else" idx
      endLbl = makeLabel "end" idx

  thenInstrs <- genBlock thenBlock
  elseInstrs <- genBlock elseBlock

  let jumpEndIfNeeded = if endsWithRet thenInstrs then [] else [IRJUMP endLbl]

  pure $
    mconcat
      [ condInstrs,
        [IRJUMP_FALSE condOp elseLbl],
        thenInstrs,
        jumpEndIfNeeded,
        [IRLABEL elseLbl],
        elseInstrs,
        [IRLABEL endLbl]
      ]

genStop :: IRGen [IRInstruction]
genStop = do
  ctx <- getCurrentLoop
  case ctx of
    Just (_, end) -> pure [IRJUMP end]
    Nothing -> pure []

genNext :: IRGen [IRInstruction]
genNext = do
  ctx <- getCurrentLoop
  case ctx of
    Just (header, _) -> pure [IRJUMP header]
    Nothing -> pure []
