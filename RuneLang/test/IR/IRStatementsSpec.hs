module IR.IRStatementsSpec (irStatementsTests) where

import Rune.AST.Nodes
import Rune.IR.Generator (generateIR)
import Rune.IR.Nodes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

--
-- public
--

irStatementsTests :: TestTree
irStatementsTests =
  testGroup
    "Rune.IR.Generator Integration Tests"
    [ testBinaryOperations,
      testUnaryOperations,
      testControlFlow,
      testLoopStatements,
      testStructOperations,
      testComplexExpressions,
      testNestedStructures
    ]

--
-- private
--

testBinaryOperations :: TestTree
testBinaryOperations =
  testGroup
    "Binary operations"
    [ testCase "Addition" $
        let expr = ExprBinary Add (ExprLitInt 5) (ExprLitInt 3)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRADD_OP" (hasInstruction prog isAddOp),
      testCase "Subtraction" $
        let expr = ExprBinary Sub (ExprLitInt 10) (ExprLitInt 3)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRSUB_OP" (hasInstruction prog isSubOp),
      testCase "Multiplication" $
        let expr = ExprBinary Mul (ExprLitInt 4) (ExprLitInt 5)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRMUL_OP" (hasInstruction prog isMulOp),
      testCase "Division" $
        let expr = ExprBinary Div (ExprLitInt 20) (ExprLitInt 4)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRDIV_OP" (hasInstruction prog isDivOp),
      testCase "Modulo" $
        let expr = ExprBinary Mod (ExprLitInt 7) (ExprLitInt 3)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRMOD_OP" (hasInstruction prog isModOp),
      testCase "Comparison Equal" $
        let expr = ExprBinary Eq (ExprLitInt 5) (ExprLitInt 5)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeBool [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRCMP_EQ" (hasInstruction prog isCmpEq),
      testCase "Comparison Not Equal" $
        let expr = ExprBinary Neq (ExprLitInt 5) (ExprLitInt 3)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeBool [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRCMP_NEQ" (hasInstruction prog isCmpNeq),
      testCase "Comparison Less Than" $
        let expr = ExprBinary Lt (ExprLitInt 3) (ExprLitInt 5)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeBool [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRCMP_LT" (hasInstruction prog isCmpLt),
      testCase "Logical And" $
        let expr = ExprBinary And (ExprLitBool True) (ExprLitBool False)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeBool [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRAND_OP" (hasInstruction prog isAndOp),
      testCase "Logical Or" $
        let expr = ExprBinary Or (ExprLitBool True) (ExprLitBool False)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeBool [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IROR_OP" (hasInstruction prog isOrOp)
    ]

testUnaryOperations :: TestTree
testUnaryOperations =
  testGroup
    "Unary operations"
    [ testCase "Negate" $
        let expr = ExprUnary Negate (ExprLitInt 42)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRSUB_OP for negation" (hasInstruction prog isSubOp),
      testCase "Prefix Increment" $
        let varDecl = StmtVarDecl "x" (Just TypeI32) (ExprLitInt 5)
            expr = ExprUnary PrefixInc (ExprVar "x")
            retStmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [varDecl, retStmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRINC" (hasInstruction prog isInc),
      testCase "Prefix Decrement" $
        let varDecl = StmtVarDecl "x" (Just TypeI32) (ExprLitInt 5)
            expr = ExprUnary PrefixDec (ExprVar "x")
            retStmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [varDecl, retStmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRDEC" (hasInstruction prog isDec),
      testCase "Postfix Increment" $
        let varDecl = StmtVarDecl "x" (Just TypeI32) (ExprLitInt 5)
            expr = ExprUnary PostfixInc (ExprVar "x")
            retStmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [varDecl, retStmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRINC and IRASSIGN" (hasInstruction prog isInc && hasInstruction prog isAssign),
      testCase "Postfix Decrement" $
        let varDecl = StmtVarDecl "x" (Just TypeI32) (ExprLitInt 5)
            expr = ExprUnary PostfixDec (ExprVar "x")
            retStmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [varDecl, retStmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRDEC and IRASSIGN" (hasInstruction prog isDec && hasInstruction prog isAssign)
    ]

testControlFlow :: TestTree
testControlFlow =
  testGroup
    "Control flow"
    [ testCase "If statement without else" $
        let cond = ExprLitBool True
            thenBlock = [StmtReturn (Just (ExprLitInt 1))]
            ifStmt = StmtIf cond thenBlock Nothing
            func = DefFunction "test" [] TypeI32 [ifStmt, StmtReturn (Just (ExprLitInt 0))]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRJUMP_FALSE" (hasInstruction prog isJumpFalse),
      testCase "If statement with else" $
        let cond = ExprBinary Lt (ExprLitInt 5) (ExprLitInt 10)
            thenBlock = [StmtReturn (Just (ExprLitInt 1))]
            elseBlock = [StmtReturn (Just (ExprLitInt 0))]
            ifStmt = StmtIf cond thenBlock (Just elseBlock)
            func = DefFunction "test" [] TypeI32 [ifStmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRJUMP_FALSE and labels" (hasInstruction prog isJumpFalse && hasInstruction prog isLabel),
      testCase "Nested if statements" $
        let innerCond = ExprLitBool False
            innerIf = StmtIf innerCond [StmtReturn (Just (ExprLitInt 2))] Nothing
            outerCond = ExprLitBool True
            outerIf = StmtIf outerCond [innerIf] Nothing
            func = DefFunction "test" [] TypeI32 [outerIf, StmtReturn (Just (ExprLitInt 0))]
            prog = generateIR (Program "test" [func])
         in assertBool "Has multiple IRJUMP_FALSE" (countInstruction prog isJumpFalse >= 2)
    ]

testLoopStatements :: TestTree
testLoopStatements =
  testGroup
    "Loop statements"
    [ testCase "For loop with start" $
        let start = Just (ExprLitInt 0)
            end = ExprLitInt 10
            body = [StmtExpr (ExprVar "i")]
            forStmt = StmtFor "i" (Just TypeI32) start end body
            func = DefFunction "test" [] TypeNull [forStmt, StmtReturn Nothing]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRCMP_LT and IRJUMP" (hasInstruction prog isCmpLt && hasInstruction prog isJump),
      testCase "For loop without start" $
        let end = ExprLitInt 5
            body = [StmtExpr (ExprVar "i")]
            forStmt = StmtFor "i" (Just TypeI32) Nothing end body
            func = DefFunction "test" [] TypeNull [forStmt, StmtReturn Nothing]
            prog = generateIR (Program "test" [func])
         in assertBool "Has default start" (hasInstruction prog isAssign),
      testCase "Infinite loop" $
        let body = [StmtStop]
            loopStmt = StmtLoop body
            func = DefFunction "test" [] TypeNull [loopStmt, StmtReturn Nothing]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRJUMP for loop" (hasInstruction prog isJump),
      testCase "Loop with next statement" $
        let body = [StmtNext, StmtReturn Nothing]
            loopStmt = StmtLoop body
            func = DefFunction "test" [] TypeNull [loopStmt, StmtReturn Nothing]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRJUMP for next" (countInstruction prog isJump >= 2)
    ]

testStructOperations :: TestTree
testStructOperations =
  testGroup
    "Struct operations"
    [ testCase "Struct definition" $
        let structDef =
              DefStruct
                "Point"
                [Rune.AST.Nodes.Field "x" TypeI32, Rune.AST.Nodes.Field "y" TypeI32]
                []
            prog = generateIR (Program "test" [structDef])
         in assertBool "Has struct definition" (hasStructDef prog "Point"),
      testCase "Struct initialization" $
        let structDef =
              DefStruct
                "Point"
                [Rune.AST.Nodes.Field "x" TypeI32, Rune.AST.Nodes.Field "y" TypeI32]
                []
            initExpr = ExprStructInit "Point" [("x", ExprLitInt 10), ("y", ExprLitInt 20)]
            stmt = StmtVarDecl "p" Nothing initExpr
            func = DefFunction "test" [] TypeNull [stmt, StmtReturn Nothing]
            prog = generateIR (Program "test" [structDef, func])
         in assertBool "Has IRALLOC and IRSET_FIELD" (hasInstruction prog isAlloc && hasInstruction prog isSetField),
      testCase "Field access" $
        let structDef =
              DefStruct
                "Point"
                [Field "x" TypeI32, Field "y" TypeI32]
                []
            initExpr = ExprStructInit "Point" [("x", ExprLitInt 10), ("y", ExprLitInt 20)]
            varDecl = StmtVarDecl "p" Nothing initExpr
            accessExpr = ExprAccess (ExprVar "p") "x"
            retStmt = StmtReturn (Just accessExpr)
            func = DefFunction "test" [] TypeI32 [varDecl, retStmt]
            prog = generateIR (Program "test" [structDef, func])
         in assertBool "Has IRGET_FIELD" (hasInstruction prog isGetField)
    ]

testComplexExpressions :: TestTree
testComplexExpressions =
  testGroup
    "Complex expressions"
    [ testCase "Nested binary operations" $
        let expr = ExprBinary Add (ExprBinary Mul (ExprLitInt 2) (ExprLitInt 3)) (ExprLitInt 5)
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeI32 [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has IRMUL_OP and IRADD_OP" (hasInstruction prog isMulOp && hasInstruction prog isAddOp),
      testCase "Mixed operations" $
        let expr =
              ExprBinary
                And
                (ExprBinary Lt (ExprLitInt 5) (ExprLitInt 10))
                (ExprBinary Gt (ExprLitInt 20) (ExprLitInt 15))
            stmt = StmtReturn (Just expr)
            func = DefFunction "test" [] TypeBool [stmt]
            prog = generateIR (Program "test" [func])
         in assertBool "Has comparisons and AND" (hasInstruction prog isCmpLt && hasInstruction prog isAndOp)
    ]

testNestedStructures :: TestTree
testNestedStructures =
  testCase "Nested if with loops" $
    let cond = ExprBinary Lt (ExprLitInt 5) (ExprLitInt 10)
        loopBody = [StmtExpr (ExprLitInt 1)]
        forStmt = StmtFor "i" (Just TypeI32) (Just (ExprLitInt 0)) (ExprLitInt 5) loopBody
        thenBlock = [forStmt]
        ifStmt = StmtIf cond thenBlock Nothing
        func = DefFunction "test" [] TypeNull [ifStmt, StmtReturn Nothing]
        prog = generateIR (Program "test" [func])
     in assertBool "Has if and loop constructs" (hasInstruction prog isJumpFalse && hasInstruction prog isCmpLt)

-- Helper functions
hasInstruction :: IRProgram -> (IRInstruction -> Bool) -> Bool
hasInstruction (IRProgram _ defs) predicate =
  any checkDef defs
  where
    checkDef (IRFunctionDef (IRFunction _ _ _ body)) = any predicate body
    checkDef _ = False

countInstruction :: IRProgram -> (IRInstruction -> Bool) -> Int
countInstruction (IRProgram _ defs) predicate =
  sum $ map countDef defs
  where
    countDef (IRFunctionDef (IRFunction _ _ _ body)) = length $ filter predicate body
    countDef _ = 0

hasStructDef :: IRProgram -> String -> Bool
hasStructDef (IRProgram _ defs) name =
  any checkDef defs
  where
    checkDef (IRStructDef n _) = n == name
    checkDef _ = False

isAddOp :: IRInstruction -> Bool
isAddOp (IRADD_OP _ _ _ _) = True
isAddOp _ = False

isSubOp :: IRInstruction -> Bool
isSubOp (IRSUB_OP _ _ _ _) = True
isSubOp _ = False

isMulOp :: IRInstruction -> Bool
isMulOp (IRMUL_OP _ _ _ _) = True
isMulOp _ = False

isDivOp :: IRInstruction -> Bool
isDivOp (IRDIV_OP _ _ _ _) = True
isDivOp _ = False

isModOp :: IRInstruction -> Bool
isModOp (IRMOD_OP _ _ _ _) = True
isModOp _ = False

isAndOp :: IRInstruction -> Bool
isAndOp (IRAND_OP _ _ _ _) = True
isAndOp _ = False

isOrOp :: IRInstruction -> Bool
isOrOp (IROR_OP _ _ _ _) = True
isOrOp _ = False

isCmpEq :: IRInstruction -> Bool
isCmpEq (IRCMP_EQ _ _ _) = True
isCmpEq _ = False

isCmpNeq :: IRInstruction -> Bool
isCmpNeq (IRCMP_NEQ _ _ _) = True
isCmpNeq _ = False

isCmpLt :: IRInstruction -> Bool
isCmpLt (IRCMP_LT _ _ _) = True
isCmpLt _ = False

isInc :: IRInstruction -> Bool
isInc (IRINC _) = True
isInc _ = False

isDec :: IRInstruction -> Bool
isDec (IRDEC _) = True
isDec _ = False

isAssign :: IRInstruction -> Bool
isAssign (IRASSIGN _ _ _) = True
isAssign _ = False

isJumpFalse :: IRInstruction -> Bool
isJumpFalse (IRJUMP_FALSE _ _) = True
isJumpFalse _ = False

isJump :: IRInstruction -> Bool
isJump (IRJUMP _) = True
isJump _ = False

isLabel :: IRInstruction -> Bool
isLabel (IRLABEL _) = True
isLabel _ = False

isAlloc :: IRInstruction -> Bool
isAlloc (IRALLOC _ _) = True
isAlloc _ = False

isSetField :: IRInstruction -> Bool
isSetField (IRSET_FIELD _ _ _ _) = True
isSetField _ = False

isGetField :: IRInstruction -> Bool
isGetField (IRGET_FIELD _ _ _ _ _) = True
isGetField _ = False
