module Rune.IR.Generator.GenStatement
  ( genStatement,
  )
where

import Rune.AST.Nodes (Expression, Statement (..), Type)
import Rune.IR.Generator.GenExpression (genExpression)
import Rune.IR.Generator.Statement.ControlFlow (genIfElse, genIfNoElse, genNext, genStop)
import Rune.IR.Generator.Statement.Loops (genForEach, genForTo, genLoop)
import Rune.IR.IRHelpers (astTypeToIRType, registerVar)
import Rune.IR.Nodes
  ( IRGen,
    IRInstruction (..),
    IROperand (..),
    IRType (IRNull),
  )

--
-- public
--

genStatement :: Statement -> IRGen [IRInstruction]
genStatement (StmtVarDecl n t e) = genVarDecl n t e
genStatement (StmtAssignment l r) = genAssignment l r
genStatement (StmtReturn Nothing) = pure [IRRET Nothing]
genStatement (StmtReturn (Just e)) = genReturnExpr e
genStatement (StmtIf cond t Nothing) = genIfNoElse genExpression genBlock cond t
genStatement (StmtIf cond t (Just e)) = genIfElse genExpression genBlock cond t e
genStatement (StmtFor v _ s e b) = genForTo genExpression genBlock v s e b
genStatement (StmtForEach v _ it b) = genForEach genExpression genBlock v it b
genStatement (StmtLoop body) = genLoop genBlock body
genStatement StmtStop = genStop
genStatement StmtNext = genNext
genStatement (StmtExpr expr) = genExprStmt expr

--
-- private
--

genBlock :: [Statement] -> IRGen [IRInstruction]
genBlock = fmap concat . mapM genStatement

genVarDecl :: String -> Maybe Type -> Expression -> IRGen [IRInstruction]
genVarDecl name maybeType expr = do
  (instrs, op, inferredType) <- genExpression expr

  let finalType = genVarType maybeType inferredType

  case op of
    IRTemp _ _ -> do
      registerVar name op finalType
      pure instrs
    _ -> do
      let assignInstr = IRASSIGN name op finalType
      registerVar name (IRTemp name finalType) finalType
      pure (instrs ++ [assignInstr])

genVarType :: Maybe Type -> IRType -> IRType
genVarType (Just t) _ = astTypeToIRType t
genVarType Nothing inferred = inferred

genAssignment :: Expression -> Expression -> IRGen [IRInstruction]
genAssignment lvalue rvalue = do
  (lInstrs, lOp, _) <- genExpression lvalue
  (rInstrs, rOp, rType) <- genExpression rvalue
  case lOp of
    IRTemp lname _ -> pure $ lInstrs ++ rInstrs ++ [IRASSIGN lname rOp rType]
    _ -> pure $ lInstrs ++ rInstrs

genReturnExpr :: Expression -> IRGen [IRInstruction]
genReturnExpr expr = do
  (instrs, op, opType) <- genExpression expr
  case opType of
    IRNull -> pure (instrs ++ [IRRET Nothing])
    _ -> pure (instrs ++ [IRRET $ Just op])

genExprStmt :: Expression -> IRGen [IRInstruction]
genExprStmt expr = do
  (instrs, _, _) <- genExpression expr
  pure instrs
