module Backend.X86_64.CodegenSpec (codegenTests) where

import Rune.Backend.X86_64.Codegen
import Rune.IR.Nodes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

codegenTests :: TestTree
codegenTests =
  testGroup
    "Rune.Backend.X86_64.Codegen Specs"
    [ testEmptyProgram,
      testSimpleFunction,
      testFunctionWithExterns,
      testFunctionWithGlobalStrings,
      testFunctionWithParameters,
      testFunctionWithReturn,
      testArithmeticInstructions,
      testComparisonInstructions,
      testControlFlow,
      testFunctionCalls,
      testMemoryOperations
    ]

testEmptyProgram :: TestTree
testEmptyProgram =
  testCase "emit assembly for empty program" $
    let irProg = IRProgram "empty" []
        asm = emitAssembly irProg
     in asm @?= ""

testSimpleFunction :: TestTree
testSimpleFunction =
  testCase "emit assembly for simple function" $
    let func = IRFunction "main" [] Nothing [IRRET Nothing]
        irProg = IRProgram "test" [IRFunctionDef func]
        asm = emitAssembly irProg
     in do
          assertBool "contains global main" ("global main" `elem` lines asm)
          assertBool "contains main:" ("main:" `elem` lines asm)
          assertBool "contains push rbp" (any ("push rbp" `isInfixOf`) (lines asm))
          assertBool "contains ret" (any ("ret" `isInfixOf`) (lines asm))

testFunctionWithExterns :: TestTree
testFunctionWithExterns =
  testCase "emit assembly with extern declarations" $
    let func = IRFunction "main" [] Nothing [IRRET Nothing]
        irProg = IRProgram "test" [IRExtern "printf", IRExtern "malloc", IRFunctionDef func]
        asm = emitAssembly irProg
     in do
          assertBool "contains extern printf" ("extern printf" `elem` lines asm)
          assertBool "contains extern malloc" ("extern malloc" `elem` lines asm)

testFunctionWithGlobalStrings :: TestTree
testFunctionWithGlobalStrings =
  testCase "emit assembly with global strings" $
    let func = IRFunction "main" [] Nothing [IRRET Nothing]
        irProg = IRProgram "test" [IRGlobalString "str0" "hello", IRFunctionDef func]
        asm = emitAssembly irProg
     in do
          assertBool "contains section .data" ("section .data" `elem` lines asm)
          assertBool "contains string definition" (any ("str0" `isInfixOf`) (lines asm))

testFunctionWithParameters :: TestTree
testFunctionWithParameters =
  testCase "emit assembly for function with parameters" $
    let func = IRFunction "add" [("a", IRI32), ("b", IRI32)] (Just IRI32) [IRRET Nothing]
        irProg = IRProgram "test" [IRFunctionDef func]
        asm = emitAssembly irProg
     in do
          assertBool "contains function prologue" (any ("push rbp" `isInfixOf`) (lines asm))
          assertBool "contains stack setup" (any ("sub rsp" `isInfixOf`) (lines asm))

testFunctionWithReturn :: TestTree
testFunctionWithReturn =
  testCase "emit assembly for function with return value" $
    let func =
          IRFunction
            "getValue"
            []
            (Just IRI32)
            [IRASSIGN "result" (IRConstInt 42) IRI32, IRRET (Just (IRTemp "result" IRI32))]
        irProg = IRProgram "test" [IRFunctionDef func]
        asm = emitAssembly irProg
     in do
          assertBool "contains mov instruction" (any ("mov" `isInfixOf`) (lines asm))
          assertBool "contains return jump" (any ("jmp" `isInfixOf`) (lines asm))

testArithmeticInstructions :: TestTree
testArithmeticInstructions =
  testGroup
    "Arithmetic instructions"
    [ testCase "IRADD_OP" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRADD_OP "result" (IRConstInt 5) (IRConstInt 3) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains add instruction" (any ("add" `isInfixOf`) (lines asm)),
      testCase "IRSUB_OP" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRSUB_OP "result" (IRConstInt 10) (IRConstInt 3) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains sub instruction" (any ("sub" `isInfixOf`) (lines asm)),
      testCase "IRMUL_OP" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRMUL_OP "result" (IRConstInt 4) (IRConstInt 5) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains imul instruction" (any ("imul" `isInfixOf`) (lines asm))
    ]

testComparisonInstructions :: TestTree
testComparisonInstructions =
  testGroup
    "Comparison instructions"
    [ testCase "IRCMP_EQ" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRCMP_EQ "result" (IRConstInt 5) (IRConstInt 5), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in do
              assertBool "contains cmp instruction" (any ("cmp" `isInfixOf`) (lines asm))
              assertBool "contains sete instruction" (any ("sete" `isInfixOf`) (lines asm)),
      testCase "IRCMP_LT" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRCMP_LT "result" (IRConstInt 3) (IRConstInt 5), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains setl instruction" (any ("setl" `isInfixOf`) (lines asm)),
      testCase "IRCMP_GT" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRCMP_GT "result" (IRConstInt 5) (IRConstInt 3), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains setg instruction" (any ("setg" `isInfixOf`) (lines asm))
    ]

testControlFlow :: TestTree
testControlFlow =
  testGroup
    "Control flow instructions"
    [ testCase "IRLABEL" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRLABEL (IRLabel ".L.loop0"), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains label" (".L.loop0:" `elem` lines asm),
      testCase "IRJUMP" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRJUMP (IRLabel ".L.end"), IRLABEL (IRLabel ".L.end"), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains jmp instruction" (any ("jmp .L.end" `isInfixOf`) (lines asm)),
      testCase "IRJUMP_TRUE" $
        let func =
              IRFunction
                "test"
                [("flag", IRI32)]
                Nothing
                [ IRJUMP_TRUE (IRParam "flag" IRI32) (IRLabel ".L.true"),
                  IRLABEL (IRLabel ".L.true"),
                  IRRET Nothing
                ]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in do
              assertBool "contains test instruction" (any ("test" `isInfixOf`) (lines asm))
              assertBool "contains jne instruction" (any ("jne" `isInfixOf`) (lines asm))
    ]

testFunctionCalls :: TestTree
testFunctionCalls =
  testCase "IRCALL instruction" $
    let func =
          IRFunction
            "main"
            []
            Nothing
            [IRCALL "result" "helper" [IRConstInt 10, IRConstInt 20] (Just IRI32), IRRET Nothing]
        irProg = IRProgram "test" [IRExtern "helper", IRFunctionDef func]
        asm = emitAssembly irProg
     in do
          assertBool "contains call instruction" (any ("call helper" `isInfixOf`) (lines asm))
          assertBool "moves result from rax" (any ("mov" `isInfixOf`) (lines asm))

testMemoryOperations :: TestTree
testMemoryOperations =
  testGroup
    "Memory operations"
    [ testCase "IRASSIGN with constant" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRASSIGN "x" (IRConstInt 42) IRI32, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains mov with constant" (any ("42" `isInfixOf`) (lines asm)),
      testCase "IRASSIGN with char" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRASSIGN "c" (IRConstChar 'A') IRChar, IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains mov byte" (any ("mov byte" `isInfixOf`) (lines asm)),
      testCase "IRINC instruction" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRASSIGN "p" (IRConstInt 100) (IRPtr IRI32), IRINC (IRTemp "p" (IRPtr IRI32)), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains add instruction" (any ("add" `isInfixOf`) (lines asm)),
      testCase "IRDEC instruction" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRASSIGN "p" (IRConstInt 100) (IRPtr IRI32), IRDEC (IRTemp "p" (IRPtr IRI32)), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains sub instruction" (any ("sub" `isInfixOf`) (lines asm)),
      testCase "IRASSIGN with IRConstNull" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRASSIGN "ptr" IRConstNull (IRPtr IRI32), IRRET Nothing]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains mov qword ... 0" (any ("mov qword" `isInfixOf`) (lines asm) && any (", 0" `isInfixOf`) (lines asm)),
      testCase "IRRET with IRConstNull" $
        let func =
              IRFunction
                "test"
                []
                (Just (IRPtr IRI32))
                [IRRET (Just IRConstNull)]
            irProg = IRProgram "test" [IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains mov rax, 0" (any ("mov rax, 0" `isInfixOf`) (lines asm)),
      testCase "IRCALL with IRConstNull argument" $
        let func =
              IRFunction
                "test"
                []
                Nothing
                [IRCALL "res" "foo" [IRConstNull] (Just (IRPtr IRChar)), IRRET Nothing]
            irProg = IRProgram "test" [IRExtern "foo", IRFunctionDef func]
            asm = emitAssembly irProg
         in assertBool "contains mov rdi, 0" (any ("mov rdi, 0" `isInfixOf`) (lines asm))
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
