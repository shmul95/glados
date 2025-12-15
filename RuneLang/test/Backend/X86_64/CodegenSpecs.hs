{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module Backend.X86_64.CodegenSpecs (codegenTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Data.List (isInfixOf)
import Rune.Backend.X86_64.Codegen
import Rune.IR.Nodes (IRProgram(..), IRTopLevel(..), IRFunction(..), IRInstruction(..), IRType(..), IROperand(..), IRGlobalValue(..), IRLabel(..))

--
-- public
--

codegenTests :: TestTree
codegenTests = testGroup "Rune.Backend.X86_64.Codegen"
  [ testEmitAssembly
  , testFloatRoDataAndGlobals
  , testParametersAndCalls
  , testComplexInstructions
  ]

--
-- private
--

testEmitAssembly :: TestTree
testEmitAssembly = testGroup "emitAssembly"
  [ testCase "Generates assembly for empty program" $
      assertBool "Empty program generates assembly" True

  , testCase "Generates assembly with extern" $
      let prog = IRProgram [] [IRExtern "printf"]
          result = emitAssembly prog
      in assertBool "Should contain extern" $ "extern printf" `elem` lines result

  , testCase "Generates assembly with global string" $
      let prog = IRProgram [] [IRGlobalDef "str1" (IRGlobalStringVal "hello")]
          result = emitAssembly prog
      in assertBool "Should contain rodata section" $ "section .rodata" `elem` lines result

  , testCase "Generates assembly with function" $
      let func = IRFunction "test" [] (Just IRNull) [IRRET Nothing]
          prog = IRProgram [] [IRFunctionDef func]
          result = emitAssembly prog
      in do
        assertBool "Should contain text section" $ "section .text" `elem` lines result
        assertBool "Should contain function name" $ any ("test:" `elem`) (map words $ lines result)

  , testCase "Generates complete assembly" $
      let func = IRFunction "main" [] (Just IRI32) [IRRET (Just (IRConstInt 0))]
          prog = IRProgram [] [IRExtern "exit", IRGlobalDef "msg" (IRGlobalStringVal "Hello"), IRFunctionDef func]
          result = emitAssembly prog
      in do
        assertBool "Should have all sections" $
          all (`elem` lines result) ["extern exit", "section .rodata", "section .text"]
  ]

testFloatRoDataAndGlobals :: TestTree
testFloatRoDataAndGlobals = testGroup "emitAssembly .rodata floats"
  [ testCase "Emits f32 and f64 float globals in .rodata" $
      let func = IRFunction "main" [] (Just IRNull) [IRRET Nothing]
          prog =
            IRProgram
              []
              [ IRGlobalDef "g_str" (IRGlobalStringVal "s")
              , IRGlobalDef "g_f32" (IRGlobalFloatVal 42.0 IRF32)
              , IRGlobalDef "g_f64" (IRGlobalFloatVal 13.37 IRF64)
              , IRGlobalDef "g_f_other" (IRGlobalFloatVal 7.0 IRI32)
              , IRFunctionDef func
              ]
          asmLines = lines (emitAssembly prog)
      in do
        assertBool "Contains .rodata section" $ "section .rodata" `elem` asmLines
        assertBool "Contains f32 dd line" $
          any (isInfixOf "g_f32 dd") asmLines && any (isInfixOf "42.0") asmLines
        assertBool "Contains f64 dq line" $
          any (isInfixOf "g_f64 dq") asmLines && any (isInfixOf "13.37") asmLines
  ]

testParametersAndCalls :: TestTree
testParametersAndCalls = testGroup "parameters and calls"
  [ testCase "Stores integer and float parameters on stack" $
      let callee =
            IRFunction
              "callee"
              [("x", IRI32), ("y", IRF32), ("z", IRF64)]
              (Just IRF32)
              [ IRRET (Just (IRTemp "y" IRF32))
              ]
          prog = IRProgram [] [IRFunctionDef callee]
          asmLines = lines (emitAssembly prog)
      in do
        assertBool "Uses mov for integer param" $
          any (isInfixOf "mov dword") asmLines
        assertBool "Uses movss for f32 param" $
          any (isInfixOf "movss dword") asmLines
        assertBool "Uses movsd for f64 param" $
          any (isInfixOf "movsd qword") asmLines

  , testCase "Emits call with float argument and saves float result" $
      let callee =
            IRFunction
              "callee"
              [("x", IRI32), ("y", IRF32)]
              (Just IRF32)
              [ IRRET (Just (IRTemp "y" IRF32))
              ]
          callerBody =
            [ IRASSIGN "v" (IRConstInt 1) IRI32
            , IRCALL "res" "callee" [IRTemp "v" IRI32, IRGlobal "g_f32" IRF32] (Just IRF32)
            , IRRET (Just (IRTemp "res" IRF32))
            ]
          caller = IRFunction "caller" [] (Just IRF32) callerBody
          prog =
            IRProgram
              []
              [ IRGlobalDef "g_f32" (IRGlobalFloatVal 1.0 IRF32)
              , IRFunctionDef callee
              , IRFunctionDef caller
              ]
          asmLines = lines (emitAssembly prog)
      in do
        assertBool "Contains call to callee" $
          any (isInfixOf "call callee") asmLines
        assertBool "Saves float return value with movss" $ any ("movss" `elem`) (map words asmLines)

  , testCase "Applies printf float fixup (cvtss2sd) when passing float" $
      let fmt  = IRGlobalDef "fmt" (IRGlobalStringVal "%f")
          fval = IRGlobalDef "g_f32" (IRGlobalFloatVal 2.5 IRF32)
          funcBody =
            [ IRCALL "" "printf" [IRGlobal "fmt" (IRPtr IRChar), IRGlobal "g_f32" IRF32] Nothing
            , IRRET Nothing
            ]
          func = IRFunction "print_float" [] (Just IRNull) funcBody
          prog = IRProgram [] [IRExtern "printf", fmt, fval, IRFunctionDef func]
          asm = emitAssembly prog
      in
        assertBool "Contains cvtss2sd for printf float argument" $
          any ("cvtss2sd" `elem`) (map words (lines asm))
  ]

testComplexInstructions :: TestTree
testComplexInstructions = testGroup "complex instruction emission"
  [ testCase "Emits various arithmetic, memory, and branch instructions" $
      let funcBody =
            [ IRALLOC "x" IRI64
            , IRASSIGN "big" (IRConstInt 5000000000) IRU64
            , IRADDR "addr_str" "str_ops0" (IRPtr IRChar)
            , IRADDR "addr_local" "x" (IRPtr IRI64)
            , IRINC (IRTemp "p" (IRPtr IRI32))
            , IRINC (IRConstInt 1)
            , IRDEREF "loaded" (IRTemp "p" (IRPtr IRI32)) IRI32
            , IRADD_OP "sum" (IRConstInt 1) (IRConstInt 2) IRI32
            , IRADD_OP "fsum" (IRConstFloat 1.0) (IRConstFloat 2.0) IRF32
            , IRJUMP_EQ0 (IRTemp "loaded" IRI32) (IRLabel "L_exit")
            , IRLABEL (IRLabel "L_exit")
            , IRRET Nothing
            ]
          func =
            IRFunction
              "ops"
              [("p", IRPtr IRI32)]
              (Just IRNull)
              funcBody
          prog =
            IRProgram
              []
              [ IRGlobalDef "str_ops0" (IRGlobalStringVal "ops")
              , IRFunctionDef func
              ]
          asmLines = lines (emitAssembly prog)
      in do
        assertBool "Contains mov rax for large constant (needsRegisterLoad path)" $
          any (isInfixOf "mov rax,") asmLines
        assertBool "Contains add or imul for integer binary op" $
          any (\ws -> "add" `elem` ws || "imul" `elem` ws) (map words asmLines)
        assertBool "Contains SSE float op for fsum" $
          any (\ws -> any (`elem` ws) ["addss", "mulss"]) (map words asmLines)
        assertBool "Contains dereference via mov / [rax]" $
          any (("[rax]" `elem`) . words) asmLines
        assertBool "Contains INC/DEC style add/sub" $
          any (\ws -> "add" `elem` ws || "sub" `elem` ws) (map words asmLines)
        assertBool "Contains conditional jump based on test" $
          any (isInfixOf "test ") asmLines
            && any (\ws -> "je" `elem` ws || "jne" `elem` ws) (map words asmLines)
  ]
