{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.GenStatement
  ( genStatement,
    genBlock,
    genVarDecl,
    genVarType,
    genAssignment,
    genReturnExpr,
    genExprStmt
  )
where
#else
module Rune.IR.Generator.GenStatement
  ( genStatement,
  )
where
#endif

import Rune.AST.Nodes (Expression(..), Statement (..), Type(..), UnaryOp(..))
import Rune.IR.Generator.GenExpression (genExpression)
import Rune.IR.Generator.Statement.ControlFlow (genIfElse, genIfNoElse, genNext, genStop)
import Rune.IR.Generator.Statement.Loops (genForEach, genForTo, genLoop)
import Rune.IR.Generator.Expression.Array (genIndexAssign)
import Rune.IR.Generator.Expression.Struct (genAccessAssign)
import Rune.IR.IRHelpers (astTypeToIRType, registerVar, newFloatGlobal)
import Rune.IR.Nodes
  ( IRGen,
    IRInstruction (..),
    IROperand (..),
    IRType (..),
  )

--
-- public
--

genStatement :: Statement -> IRGen [IRInstruction]
genStatement (StmtVarDecl _ n t e) = genVarDecl n t e
genStatement (StmtAssignment _ l r) = genAssignment l r
genStatement (StmtReturn _ Nothing) = pure [IRRET Nothing]
genStatement (StmtReturn _ (Just e)) = genReturnExpr e
genStatement (StmtIf _ cond t Nothing) = genIfNoElse genExpression genBlock cond t
genStatement (StmtIf _ cond t (Just e)) = genIfElse genExpression genBlock cond t e
genStatement (StmtFor _ v t s e b) = genForTo genExpression genBlock v t s e b
genStatement (StmtForEach _ v _ it b) = genForEach genExpression genBlock v it b
genStatement (StmtLoop _ body) = genLoop genBlock body
genStatement (StmtStop _) = genStop
genStatement (StmtNext _) = genNext
genStatement (StmtExpr _ expr) = genExprStmt expr

--
-- private
--

genBlock :: [Statement] -> IRGen [IRInstruction]
genBlock = fmap concat . mapM genStatement


genVarDecl :: String -> Maybe Type -> Expression -> IRGen [IRInstruction]
genVarDecl name (Just TypeF64) (ExprLitFloat _ f) = do
  glName <- newFloatGlobal f IRF64
  let finalType = IRF64
      op        = IRGlobal glName IRF64
      assignInstr = IRASSIGN name op finalType
  registerVar name (IRTemp name finalType) finalType
  pure [assignInstr]
genVarDecl name maybeType expr = do
  (instrs, op, inferredType) <- genExpression expr

  let finalType = genVarType maybeType inferredType
      assignInstr = IRASSIGN name op finalType

  registerVar name (IRTemp name finalType) finalType
  pure (instrs <> [assignInstr])


genVarType :: Maybe Type -> IRType -> IRType
genVarType (Just (TypeArray elemType)) _ = IRPtr (IRArray (astTypeToIRType elemType) 0)
genVarType (Just t) _ = astTypeToIRType t
genVarType Nothing inferred = inferred


genAssignment :: Expression -> Expression -> IRGen [IRInstruction]
genAssignment (ExprIndex _ target idx) rvalue = genIndexAssign genExpression target idx rvalue
genAssignment (ExprAccess _ target field) rvalue = genAccessAssign genExpression target field rvalue
genAssignment (ExprUnary _ Deref ptrExpr) rvalue = do
  (pInstrs, pOp, _) <- genExpression ptrExpr
  (rInstrs, rOp, _) <- genExpression rvalue
  pure $ pInstrs <> rInstrs <> [IRSTORE pOp rOp]
genAssignment lvalue rvalue = do
  (lInstrs, lOp, _) <- genExpression lvalue
  (rInstrs, rOp, rType) <- genExpression rvalue
  case lOp of
    IRTemp lname _ -> pure $ lInstrs <> rInstrs <> [IRASSIGN lname rOp rType]
    _ -> pure $ lInstrs <> rInstrs


genReturnExpr :: Expression -> IRGen [IRInstruction]
genReturnExpr expr = do
  (instrs, op, opType) <- genExpression expr
  case opType of
    IRNull -> pure (instrs <> [IRRET Nothing])
    _ -> pure (instrs <> [IRRET $ Just op])


genExprStmt :: Expression -> IRGen [IRInstruction]
genExprStmt expr = do
  (instrs, _, _) <- genExpression expr
  pure instrs
