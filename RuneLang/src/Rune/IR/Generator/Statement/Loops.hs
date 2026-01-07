module Rune.IR.Generator.Statement.Loops
  ( genForTo,
    genForEach,
    genLoop,
  )
where

import Rune.AST.Nodes (Expression, Statement, Type)
import Rune.IR.IRHelpers
  ( makeLabel,
    newTemp,
    nextLabelIndex,
    popLoopContext,
    pushLoopContext,
    registerVar,
    astTypeToIRType,
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
genForTo :: GenExprCallback -> GenBlockCallback -> String -> Maybe Type -> Maybe Expression -> Expression -> [Statement] -> IRGen [IRInstruction]
genForTo genExpr genBlock var varType start end body = do
  idx <- nextLabelIndex
  let headerLbl = makeLabel "loop_header" idx
      bodyLbl = makeLabel "body" idx
      endLbl = makeLabel "loop_end" idx

  -- Determine the type of the loop variable
  let loopVarType = maybe IRI32 astTypeToIRType varType

  initInstrs <- case start of
    Just startExpr -> do
      (i, op, _) <- genExpr startExpr
      pure (i <> [IRASSIGN var op loopVarType])
    Nothing ->
      pure [IRASSIGN var (IRConstInt 0) loopVarType]

  registerVar var (IRTemp var loopVarType) loopVarType

  (endInstrs, endOp, _) <- genExpr end
  cmpTemp <- newTemp "cmp" IRBool

  pushLoopContext headerLbl endLbl
  bodyInstrs <- genBlock body
  popLoopContext

  -- Extract start value to determine comparison direction
  let startVal = case initInstrs of
        (_ : IRASSIGN _ (IRConstInt s) _ : _) -> Just s
        (IRASSIGN _ (IRConstInt s) _ : _) -> Just s
        _ -> Nothing
      
      -- Determine comparison direction based on start and end values
      cmpInstr = case (startVal, endOp) of
        (Just s, IRConstInt e) | s > e -> IRCMP_GT cmpTemp (IRTemp var loopVarType) endOp
        (Nothing, IRConstInt e) | e < 0 -> IRCMP_GT cmpTemp (IRTemp var loopVarType) endOp
        _ -> IRCMP_LT cmpTemp (IRTemp var loopVarType) endOp

  pure $
    mconcat
      [ initInstrs,
        [IRLABEL headerLbl],
        endInstrs,
        [cmpInstr],
        [IRJUMP_FALSE (IRTemp cmpTemp IRBool) endLbl],
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

  (iterInstrs, iterOp, iterType) <- genExpr iterable
  
  -- deduce element type from iterator type
  --   strings: IRPtr IRChar -> element is IRChar
  --   arrays:  IRPtr (IRArray elemType _) -> element is elemType
  let elemType = case iterType of
                   IRPtr IRChar -> IRChar
                   IRPtr (IRArray t _) -> t
                   _ -> IRChar
      ptrType = IRPtr elemType
  
  ptrTemp <- newTemp "p_ptr" ptrType
  registerVar var (IRTemp var elemType) elemType

  pushLoopContext headerLbl endLbl
  bodyInstrs <- genBlock body
  popLoopContext

  pure $
    mconcat
      [ iterInstrs,
        [IRASSIGN ptrTemp iterOp ptrType],
        [IRLABEL headerLbl],
        [IRDEREF var (IRTemp ptrTemp ptrType) elemType],
        [IRLABEL checkLbl],
        [IRJUMP_EQ0 (IRTemp var elemType) endLbl],
        [IRLABEL bodyLbl],
        bodyInstrs,
        [IRINC (IRTemp ptrTemp ptrType)],
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
