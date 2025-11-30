module Rune.IR.Generator.Statement.Loops
  ( genForTo,
    genForEach,
    genLoop,
  )
where

import Rune.AST.Nodes (Expression, Statement)
import Rune.IR.IRHelpers
  ( makeLabel,
    newTemp,
    nextLabelIndex,
    popLoopContext,
    pushLoopContext,
    registerVar,
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
--     JUMP .L.loop_header<n>
-- .L.loop_end<n>:
--
genForTo :: GenExprCallback -> GenBlockCallback -> String -> Maybe Expression -> Expression -> [Statement] -> IRGen [IRInstruction]
genForTo genExpr genBlock var start end body = do
  idx <- nextLabelIndex
  let headerLbl = makeLabel "loop_header" idx
      bodyLbl = makeLabel "body" idx
      endLbl = makeLabel "loop_end" idx

  initInstrs <- case start of
    Just startExpr -> do
      (i, op, typ) <- genExpr startExpr
      pure (i ++ [IRASSIGN var op typ])
    Nothing ->
      pure [IRASSIGN var (IRConstInt 0) IRI32]

  registerVar var (IRTemp var IRI32) IRI32

  (endInstrs, endOp, _) <- genExpr end
  cmpTemp <- newTemp "cmp" IRI32

  pushLoopContext headerLbl endLbl
  bodyInstrs <- genBlock body
  popLoopContext

  pure $
    mconcat
      [ initInstrs,
        [IRLABEL headerLbl],
        endInstrs,
        [IRCMP_LT cmpTemp (IRTemp var IRI32) endOp],
        [IRJUMP_FALSE (IRTemp cmpTemp IRI32) endLbl],
        [IRLABEL bodyLbl],
        bodyInstrs,
        [IRJUMP headerLbl],
        [IRLABEL endLbl]
      ]

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
genForEach :: GenExprCallback -> GenBlockCallback -> String -> Expression -> [Statement] -> IRGen [IRInstruction]
genForEach genExpr genBlock var iterable body = do
  idx <- nextLabelIndex
  let headerLbl = makeLabel "loop_header" idx
      checkLbl = makeLabel "loop_check" idx
      bodyLbl = makeLabel "body" idx
      endLbl = makeLabel "loop_end" idx

  (iterInstrs, iterOp, _) <- genExpr iterable
  ptrTemp <- newTemp "p_ptr" (IRPtr IRU8)

  registerVar var (IRTemp var IRU8) IRU8

  pushLoopContext headerLbl endLbl
  bodyInstrs <- genBlock body
  popLoopContext

  pure $
    mconcat
      [ iterInstrs,
        [IRASSIGN ptrTemp iterOp (IRPtr IRU8)],
        [IRLABEL headerLbl],
        [IRDEREF var (IRTemp ptrTemp (IRPtr IRU8)) IRU8],
        [IRLABEL checkLbl],
        [IRJUMP_EQ0 (IRTemp var IRU8) endLbl],
        [IRLABEL bodyLbl],
        bodyInstrs,
        [IRINC (IRTemp ptrTemp (IRPtr IRU8))],
        [IRJUMP headerLbl],
        [IRLABEL endLbl]
      ]

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
genLoop :: GenBlockCallback -> [Statement] -> IRGen [IRInstruction]
genLoop genBlock body = do
  idx <- nextLabelIndex
  let headerLbl = makeLabel "loop_header" idx
      endLbl = makeLabel "loop_end" idx

  pushLoopContext headerLbl endLbl
  bodyInstrs <- genBlock body
  popLoopContext

  pure $
    mconcat
      [ [IRLABEL headerLbl],
        bodyInstrs,
        [IRJUMP headerLbl],
        [IRLABEL endLbl]
      ]
