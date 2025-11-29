module Rune.IR.Generator.GenStatement
  ( genStatement,
  )
where

import Rune.AST.Nodes (Expression, Statement (..))
import Rune.IR.Generator.GenExpression (genExpression)
import Rune.IR.IRHelpers
  ( endsWithRet,
    makeLabel,
    newTemp,
    nextLabelIndex,
    registerVar,
  )
import Rune.IR.Nodes
  ( IRGen,
    IRInstruction (..),
    IRLabel (..),
    IROperand (..),
    IRType (..),
  )

--
-- public
--

genStatement :: Statement -> IRGen [IRInstruction]
genStatement (StmtVarDecl n _ e) = genVarDecl n e
genStatement (StmtAssignment l r) = genAssignment l r
genStatement (StmtReturn Nothing) = pure [IRRET Nothing]
genStatement (StmtReturn (Just e)) = genReturnExpr e
genStatement (StmtIf cond t Nothing) = genIfNoElse cond t
genStatement (StmtIf cond t (Just e)) = genIfElse cond t e
genStatement (StmtFor v _ s e b) = genFor v s e b
genStatement (StmtForEach v _ it b) = genForEach v it b
genStatement (StmtLoop body) = genLoop body
genStatement StmtStop = pure [IRJUMP (IRLabel ".L.loop_end")]
genStatement StmtNext = pure [IRJUMP (IRLabel ".L.loop_header")]
genStatement (StmtExpr expr) = genExprStmt expr

--
-- private generators
--

genBlock :: [Statement] -> IRGen [IRInstruction]
genBlock = fmap concat . mapM genStatement

genVarDecl :: String -> Expression -> IRGen [IRInstruction]
genVarDecl name expr = do
  (instrs, op, typ) <- genExpression expr
  case op of
    IRTemp _ _ -> do
      registerVar name op typ
      pure instrs
    _ -> do
      let assignInstr = IRASSIGN name op typ
      registerVar name (IRTemp name typ) typ
      pure (instrs ++ [assignInstr])

genAssignment :: Expression -> Expression -> IRGen [IRInstruction]
genAssignment lvalue rvalue = do
  (lInstrs, lOp, _) <- genExpression lvalue
  (rInstrs, rOp, rType) <- genExpression rvalue
  case lOp of
    IRTemp lname _ -> pure $ lInstrs ++ rInstrs ++ [IRASSIGN lname rOp rType]
    _ -> pure $ lInstrs ++ rInstrs

genReturnExpr :: Expression -> IRGen [IRInstruction]
genReturnExpr expr = do
  (instrs, op, _) <- genExpression expr
  pure (instrs ++ [IRRET $ Just op])

--
-- gen conditional
--

genIfNoElse :: Expression -> [Statement] -> IRGen [IRInstruction]
genIfNoElse cond thenBlock = do
  (condInstrs, condOp, _) <- genExpression cond
  idx <- nextLabelIndex
  let endLbl = makeLabel "end" idx
  thenInstrs <- genBlock thenBlock
  pure $
    condInstrs
      ++ [IRJUMP_FALSE condOp endLbl]
      ++ thenInstrs
      ++ [IRLABEL endLbl]

genIfElse :: Expression -> [Statement] -> [Statement] -> IRGen [IRInstruction]
genIfElse cond thenBlock elseBlock = do
  (condInstrs, condOp, _) <- genExpression cond
  idx <- nextLabelIndex
  let elseLbl = makeLabel "else" idx
      endLbl = makeLabel "end" idx

  thenInstrs <- genBlock thenBlock
  elseInstrs <- genBlock elseBlock

  let jumpEndIfNeeded =
        if endsWithRet thenInstrs then [] else [IRJUMP endLbl]

  pure $
    condInstrs
      ++ [IRJUMP_FALSE condOp elseLbl]
      ++ thenInstrs
      ++ jumpEndIfNeeded
      ++ [IRLABEL elseLbl]
      ++ elseInstrs
      ++ [IRLABEL endLbl]

--
-- gen loops
--

--
-- for to
--

--
-- for i = 0 to 10 {
--     ++i;
-- }
--
-- .L.loop_header<n>:
--     cmp<n> = CMP_LT i, 10
--     JUMP_FALSE cmp<n>, .L.loop_end<n>
-- .L.body<n>:
--     INC i
--     JUMP.L.loop_header<n>
-- .L.loop_end<n>:
--
genFor :: String -> Maybe Expression -> Expression -> [Statement] -> IRGen [IRInstruction]
genFor var start end body = do
  idx <- nextLabelIndex
  let headerLbl = makeLabel "loop_header" idx
      bodyLbl = makeLabel "body" idx
      endLbl = makeLabel "loop_end" idx

  initInstrs <- case start of
    Just startExpr -> do
      (i, op, typ) <- genExpression startExpr
      pure (i ++ [IRASSIGN var op typ])
    Nothing ->
      pure [IRASSIGN var (IRConstInt 0) IRI32]

  registerVar var (IRTemp var IRI32) IRI32

  (endInstrs, endOp, _) <- genExpression end
  cmpTemp <- newTemp "cmp" IRI32
  bodyInstrs <- genBlock body

  pure $
    initInstrs
      ++ [IRLABEL headerLbl]
      ++ endInstrs
      ++ [IRCMP_LT cmpTemp (IRTemp var IRI32) endOp]
      ++ [IRJUMP_FALSE (IRTemp cmpTemp IRI32) endLbl]
      ++ [IRLABEL bodyLbl]
      ++ bodyInstrs
      ++ [IRJUMP headerLbl]
      ++ [IRLABEL endLbl]

--
-- foreach
--

--
--  for item in iterable {
--      do_something(item);
--  }
--
-- .L.loop_header<n>:
--      item = *p_ptr;
-- .L.loop_check<n>:
--      JUMP_EQ0 item, .L.loop_end<n>
-- .L.body<n>:
--      CALL do_something(item)
--      INC p_ptr;
--      JUMP .L.loop_header<n>
-- .L.loop_end<n>:
--
genForEach :: String -> Expression -> [Statement] -> IRGen [IRInstruction]
genForEach var iterable body = do
  idx <- nextLabelIndex
  let headerLbl = makeLabel "loop_header" idx
      checkLbl = makeLabel "loop_check" idx
      bodyLbl = makeLabel "body" idx
      endLbl = makeLabel "loop_end" idx

  (iterInstrs, iterOp, _) <- genExpression iterable
  ptrTemp <- newTemp "p_ptr" (IRPtr IRU8)

  registerVar var (IRTemp var IRU8) IRU8

  bodyInstrs <- genBlock body

  pure $
    iterInstrs
      ++ [IRASSIGN ptrTemp iterOp (IRPtr IRU8)]
      ++ [IRLABEL headerLbl]
      ++ [IRDEREF var (IRTemp ptrTemp (IRPtr IRU8)) IRU8]
      ++ [IRLABEL checkLbl]
      ++ [IRJUMP_EQ0 (IRTemp var IRU8) endLbl]
      ++ [IRLABEL bodyLbl]
      ++ bodyInstrs
      ++ [IRINC (IRTemp ptrTemp (IRPtr IRU8))]
      ++ [IRJUMP headerLbl]
      ++ [IRLABEL endLbl]

--
-- loop
--

--
-- k: i32 = 0;
--
-- loop {
--     k += 2;
--     if k > 10 {
--         stop;
--     }
-- }
--
-- .L.loop_header<n>:
--     t<n>: i32 = ADD k, 2
--     k: i32 = t1
--     t2 = CMP_GT k, 10
--     JUMP_FALSE t2, .L.loop_end<n>
--     JUMP .L.loop_header<n>
-- .L.loop_end<n>:
--     JUMP .L.loop_header<n>
-- .L.Loop_end<n>:
--
genLoop :: [Statement] -> IRGen [IRInstruction]
genLoop body = do
  idx <- nextLabelIndex
  let headerLbl = makeLabel "loop_header" idx
      endLbl = makeLabel "loop_end" idx
  bodyInstrs <- genBlock body
  pure $
    [IRLABEL headerLbl]
      ++ bodyInstrs
      ++ [IRJUMP headerLbl]
      ++ [IRLABEL endLbl]

--
-- expression statement
--

genExprStmt :: Expression -> IRGen [IRInstruction]
genExprStmt expr = do
  (instrs, _, _) <- genExpression expr
  pure instrs
