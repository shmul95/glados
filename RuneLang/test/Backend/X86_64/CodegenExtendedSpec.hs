module Backend.X86_64.CodegenExtendedSpec (codegenExtendedTests) where

import Rune.Backend.X86_64.Codegen
import Rune.IR.Nodes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

codegenExtendedTests :: TestTree
codegenExtendedTests =
  testGroup
    "Rune.Backend.X86_64.Codegen Extended Tests"
    [ testMoreArithmeticOps,
      testMoreComparisonOps,
      testMoreControlFlow,
      testMemoryInstructions,
      testComplexFunctions
    ]

testMoreArithmeticOps :: TestTree
testMoreArithmeticOps =
  testGroup
    "More arithmetic operations"
    [ testCase "IRDIV_OP" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRDIV_OP "result" (IRConstInt 20) (IRConstInt 4) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains TODO DIV" (any ("TODO" `isInfixOf`) (lines asm)),
      testCase "IRMOD_OP" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRMOD_OP "result" (IRConstInt 7) (IRConstInt 3) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains TODO MOD" (any ("TODO" `isInfixOf`) (lines asm))
    ]

testMoreComparisonOps :: TestTree
testMoreComparisonOps =
  testGroup
    "More comparison operations"
    [ testCase "IRCMP_LTE" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRCMP_LTE "result" (IRConstInt 5) (IRConstInt 10), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains setle" (any ("setle" `isInfixOf`) (lines asm)),
      testCase "IRCMP_GT" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRCMP_GT "result" (IRConstInt 10) (IRConstInt 5), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains setg" (any ("setg" `isInfixOf`) (lines asm)),
      testCase "IRCMP_GTE" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRCMP_GTE "result" (IRConstInt 10) (IRConstInt 10), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains setge" (any ("setge" `isInfixOf`) (lines asm)),
      testCase "IRCMP_NEQ" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRCMP_NEQ "result" (IRConstInt 5) (IRConstInt 10), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains setne" (any ("setne" `isInfixOf`) (lines asm))
    ]

testMoreControlFlow :: TestTree
testMoreControlFlow =
  testGroup
    "More control flow"
    [ testCase "IRJUMP_FALSE" $
        let func =
              IRFunction
                "test"
                [("flag", IRI32)]
                Nothing
                [ IRJUMP_FALSE (IRParam "flag" IRI32) (IRLabel ".L.false"),
                  IRLABEL (IRLabel ".L.false"),
                  IRRET Nothing
                ]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in do
              assertBool "contains je" (any ("je" `isInfixOf`) (lines asm))
              assertBool "contains label" (".L.false:" `elem` lines asm),
      testCase "IRJUMP_EQ0" $
        let func =
              IRFunction
                "test"
                [("val", IRI32)]
                Nothing
                [ IRJUMP_EQ0 (IRParam "val" IRI32) (IRLabel ".L.zero"),
                  IRLABEL (IRLabel ".L.zero"),
                  IRRET Nothing
                ]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains je for zero check" (any ("je" `isInfixOf`) (lines asm))
    ]

testMemoryInstructions :: TestTree
testMemoryInstructions =
  testGroup
    "Memory instructions"
    [ testCase "IRALLOC" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRALLOC "x" IRI32, IRASSIGN "x" (IRConstInt 10) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "allocates space" (any ("sub rsp" `isInfixOf`) (lines asm)),
      testCase "IRLOAD" $
        let func =
              IRFunction
                "test"
                [("ptr", IRPtr IRI32)]
                Nothing
                [IRLOAD "val" (IRParam "ptr" (IRPtr IRI32)) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "loads from pointer" (any ("mov" `isInfixOf`) (lines asm)),
      testCase "IRDEREF" $
        let func =
              IRFunction
                "test"
                [("ptr", IRPtr IRI32)]
                Nothing
                [IRDEREF "val" (IRParam "ptr" (IRPtr IRI32)) IRI32, IRRET (Just (IRTemp "val" IRI32))]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "dereferences pointer" (any ("mov" `isInfixOf`) (lines asm)),
      testCase "IRADDR" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRALLOC "x" IRI32, IRADDR "p" "x" (IRPtr IRI32), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "gets address" (any ("lea" `isInfixOf`) (lines asm)),
      testCase "IRASSIGN with temp" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRASSIGN "x" (IRConstInt 5) IRI32, IRASSIGN "y" (IRTemp "x" IRI32) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "moves temp to temp" (length (filter ("mov" `isInfixOf`) (lines asm)) >= 2),
      testCase "IRASSIGN with param" $
        let func =
              IRFunction
                "test"
                [("p", IRI32)]
                Nothing
                [IRASSIGN "x" (IRParam "p" IRI32) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "moves param" (any ("mov" `isInfixOf`) (lines asm))
    ]

testComplexFunctions :: TestTree
testComplexFunctions =
  testGroup
    "Complex functions"
    [ testCase "Function with multiple parameters" $
        let func =
              IRFunction
                "add3"
                [("a", IRI32), ("b", IRI32), ("c", IRI32)]
                (Just IRI32)
                [ IRADD_OP "t1" (IRParam "a" IRI32) (IRParam "b" IRI32) IRI32,
                  IRADD_OP "t2" (IRTemp "t1" IRI32) (IRParam "c" IRI32) IRI32,
                  IRRET (Just (IRTemp "t2" IRI32))
                ]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in do
              assertBool "has parameter setup" (any ("mov" `isInfixOf`) (lines asm))
              assertBool "has add operations" (length (filter ("add" `isInfixOf`) (lines asm)) >= 2),
      testCase "Function calling another function" $
        let func1 =
              IRFunction
                "main"
                []
                (Just IRI32)
                [ IRCALL "result" "helper" [IRConstInt 5, IRConstInt 10] (Just IRI32),
                  IRRET (Just (IRTemp "result" IRI32))
                ]
            func2 =
              IRFunction
                "helper"
                [("x", IRI32), ("y", IRI32)]
                (Just IRI32)
                [IRADD_OP "sum" (IRParam "x" IRI32) (IRParam "y" IRI32) IRI32, IRRET (Just (IRTemp "sum" IRI32))]
            irProg = IRProgram "test" [IRFunctionDef func1, IRFunctionDef func2]
            asm = emitAssembly irProg
         in do
              assertBool "has call" (any ("call helper" `isInfixOf`) (lines asm))
              assertBool "has both functions" ("main:" `elem` lines asm && "helper:" `elem` lines asm),
      testCase "Function with char operations" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRASSIGN "c" (IRConstChar 'A') IRChar, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "handles char" (any (show (fromEnum 'A') `isInfixOf`) (lines asm)),
      testCase "Function with multiple calls" $
        let func =
              IRFunction
                "test"
                []
                (Just IRI32)
                [ IRCALL "r1" "foo" [IRConstInt 1] (Just IRI32),
                  IRCALL "r2" "bar" [IRTemp "r1" IRI32] (Just IRI32),
                  IRRET (Just (IRTemp "r2" IRI32))
                ]
            irProg = IRProgram "test" [IRExtern "foo", IRExtern "bar", IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "has multiple calls" (length (filter ("call" `isInfixOf`) (lines asm)) >= 2),
      testCase "Function with logical operations" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [ IRAND_OP "r1" (IRConstInt 1) (IRConstInt 1) IRI32,
                  IROR_OP "r2" (IRConstInt 0) (IRConstInt 1) IRI32,
                  IRRET Nothing
                ]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in do
              assertBool "has and" (any ("and" `isInfixOf`) (lines asm))
              assertBool "has or" (any ("or" `isInfixOf`) (lines asm))
    ]

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : xs') = xs : tails xs'
