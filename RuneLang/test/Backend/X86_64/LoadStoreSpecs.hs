{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module Backend.X86_64.LoadStoreSpecs (loadStoreTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Rune.Backend.X86_64.LoadStore
import Rune.IR.Nodes (IRType(..), IROperand(..))
import qualified Data.Map.Strict as Map

--
-- public
--

loadStoreTests :: TestTree
loadStoreTests = testGroup "Rune.Backend.X86_64.LoadStore"
  [ testStackAddr
  , testOperandAddr
  , testNeedsRegisterLoad
  , testGetTestReg
  , testStoreReg
  , testMoveStackToStack
  , testLoadReg
  , testLoadRegWithExt
  , testVarStackAddr
  , testExtendVar
  , testLoadVarReg
  ]

--
-- private
--

testStackAddr :: TestTree
testStackAddr = testGroup "stackAddr"
  [ testCase "Returns address for positive offset" $
      let sm = Map.singleton "x" 8
      in stackAddr sm "x" @?= "[rbp8]"

  , testCase "Returns address for negative offset" $
      let sm = Map.singleton "y" (-8)
      in stackAddr sm "y" @?= "[rbp-8]"
  ]

testOperandAddr :: TestTree
testOperandAddr = testGroup "operandAddr"
  [ testCase "Returns const int string" $
      operandAddr Map.empty (IRConstInt 42) @?= "42"
  
  , testCase "Returns const char code" $
      operandAddr Map.empty (IRConstChar 'A') @?= "65"

  , testCase "Returns const bool 1" $
      operandAddr Map.empty (IRConstBool True) @?= "1"

  , testCase "Returns const null 0" $
      operandAddr Map.empty IRConstNull @?= "0"

  , testCase "Returns stack address for temp" $
      let sm = Map.singleton "t" (-4)
      in operandAddr sm (IRTemp "t" IRI32) @?= "[rbp-4]"
  ]

testNeedsRegisterLoad :: TestTree
testNeedsRegisterLoad = testGroup "needsRegisterLoad"
  [ testCase "Returns true for large i64" $
      needsRegisterLoad 5000000000 IRI64 @?= True
  
  , testCase "Returns false for small i64" $
      needsRegisterLoad 42 IRI64 @?= False

  , testCase "Returns false for i32" $
      needsRegisterLoad 5000000000 IRI32 @?= False
  ]

testGetTestReg :: TestTree
testGetTestReg = testGroup "getTestReg"
  [ testCase "Returns rax defaults" $
      getTestReg (IRConstInt 0) @?= "rax"
  
  , testCase "Returns sized register for temp" $
      getTestReg (IRTemp "t" IRI32) @?= "eax"
  ]

testStoreReg :: TestTree
testStoreReg = testGroup "storeReg"
  [ testCase "Stores 32-bit register" $
      let sm = Map.singleton "dest" (-4)
      in storeReg sm "dest" "rax" IRI32 @?= "    mov dword [rbp-4], eax"
  
  , testCase "Stores 64-bit register" $
      let sm = Map.singleton "dest" (-8)
      in storeReg sm "dest" "rax" IRI64 @?= "    mov qword [rbp-8], rax"
  ]

testMoveStackToStack :: TestTree
testMoveStackToStack = testGroup "moveStackToStack"
  [ testCase "Moves i32 between stack slots" $
      let sm = Map.fromList [("src", -4), ("dest", -8)]
          instrs = moveStackToStack sm "dest" "src" IRI32
      in instrs @?= ["    mov eax, dword [rbp-4]", "    mov dword [rbp-8], eax"]
  ]

testLoadReg :: TestTree
testLoadReg = testGroup "loadReg"
  [ testCase "Loads constant" $
      loadReg Map.empty "rax" (IRConstInt 10) @?= ["    mov rax, 10"]
  
  , testCase "Loads global address" $
      loadReg Map.empty "rax" (IRGlobal "g" IRI64) @?= ["    mov rax, g"]
  
  , testCase "Loads stack variable" $
      let sm = Map.singleton "x" (-8)
      in loadReg sm "rax" (IRTemp "x" IRI64) @?= ["    mov rax, qword [rbp-8]"]
  ]

testLoadRegWithExt :: TestTree
testLoadRegWithExt = testGroup "loadRegWithExt"
  [ testCase "Loads with sign extension i8" $
      let sm = Map.singleton "x" (-1)
          instrs = loadRegWithExt sm ("rdi", IRTemp "x" IRI8)
      in instrs @?= ["    movsx rdi, byte [rbp-1]"]
  
  , testCase "Loads with zero extension u8" $
      let sm = Map.singleton "x" (-1)
          instrs = loadRegWithExt sm ("rdi", IRTemp "x" IRU8)
      in instrs @?= ["    movzx rdi, byte [rbp-1]"]
  ]

testVarStackAddr :: TestTree
testVarStackAddr = testGroup "varStackAddr"
  [ testCase "Returns stack addr for temp" $
      let sm = Map.singleton "t" (-8)
      in varStackAddr sm (IRTemp "t" IRI32) @?= "[rbp-8]"
  ]

testExtendVar :: TestTree
testExtendVar = testGroup "extendVar"
  [ testCase "Extends i32 to 64 bit reg" $
      let sm = Map.singleton "x" (-4)
          instrs = extendVar sm "rax" (IRTemp "x" IRI32) IRI32
      in instrs @?= ["    movsxd rax, dword [rbp-4]"]
  ]

testLoadVarReg :: TestTree
testLoadVarReg = testGroup "loadVarReg"
  [ testCase "Loads var to reg with correct size" $
      let sm = Map.singleton "x" (-4)
          instrs = loadVarReg sm "rax" (IRTemp "x" IRI32) IRI32
      in instrs @?= ["    mov eax, dword [rbp-4]"]
  ]
