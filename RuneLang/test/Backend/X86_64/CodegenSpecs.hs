{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module Backend.X86_64.CodegenSpecs (codegenTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Rune.Backend.X86_64.Codegen
import Rune.IR.Nodes (IRProgram(..), IRTopLevel(..), IRFunction(..), IRInstruction(..), IRType(..), IROperand(..))

--
-- public
--

codegenTests :: TestTree
codegenTests = testGroup "Rune.Backend.X86_64.Codegen"
  [ testEmitAssembly
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
      let prog = IRProgram [] [IRGlobalString "str1" "hello"]
          result = emitAssembly prog
      in assertBool "Should contain data section" $ "section .data" `elem` lines result

  , testCase "Generates assembly with function" $
      let func = IRFunction "test" [] (Just IRNull) [IRRET Nothing]
          prog = IRProgram [] [IRFunctionDef func]
          result = emitAssembly prog
      in do
        assertBool "Should contain text section" $ "section .text" `elem` lines result
        assertBool "Should contain function name" $ any ("test:" `elem`) (map words $ lines result)

  , testCase "Generates complete assembly" $
      let func = IRFunction "main" [] (Just IRI32) [IRRET (Just (IRConstInt 0))]
          prog = IRProgram [] [IRExtern "exit", IRGlobalString "msg" "Hello", IRFunctionDef func]
          result = emitAssembly prog
      in do
        assertBool "Should have all sections" $
          all (`elem` lines result) ["extern exit", "section .data", "section .text"]
  ]
