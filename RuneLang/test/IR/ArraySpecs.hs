{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.ArraySpecs (arrayTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Rune.IR.Nodes (IRType(..), IRInstruction(..), IROperand(..))
import Rune.IR.Printer (printType, printInstruction, printOperand)

--
-- public
--

arrayTests :: TestTree
arrayTests = testGroup "Rune.IR.Array"
  [ testArrayType
  , testArrayInstructions
  , testArrayPrinter
  ]

--
-- private
--

testArrayType :: TestTree
testArrayType = testGroup "IRType - IRArray"
  [ testCase "IRArray i32 with length 5" $
      let arrType = IRArray IRI32 5
      in arrType @?= IRArray IRI32 5

  , testCase "IRArray char with length 10" $
      let arrType = IRArray IRChar 10
      in arrType @?= IRArray IRChar 10

  , testCase "Pointer to IRArray" $
      let ptrArrType = IRPtr (IRArray IRI32 3)
      in ptrArrType @?= IRPtr (IRArray IRI32 3)

  , testCase "Nested array types" $
      let nestedArr = IRArray (IRArray IRI32 3) 2
      in nestedArr @?= IRArray (IRArray IRI32 3) 2
  ]

testArrayInstructions :: TestTree
testArrayInstructions = testGroup "Array instructions"
  [ testCase "IRALLOC_ARRAY with integers" $
      let instr = IRALLOC_ARRAY "%arr" IRI32 [IRConstInt 1, IRConstInt 2, IRConstInt 3]
      in instr @?= IRALLOC_ARRAY "%arr" IRI32 [IRConstInt 1, IRConstInt 2, IRConstInt 3]

  , testCase "IRGET_ELEM instruction" $
      let instr = IRGET_ELEM "%tmp" (IRTemp "%arr" (IRPtr (IRArray IRI32 3))) (IRConstInt 1) IRI32
      in instr @?= IRGET_ELEM "%tmp" (IRTemp "%arr" (IRPtr (IRArray IRI32 3))) (IRConstInt 1) IRI32

  , testCase "IRSET_ELEM instruction" $
      let instr = IRSET_ELEM (IRTemp "%arr" (IRPtr (IRArray IRI32 3))) (IRConstInt 2) (IRConstInt 42)
      in instr @?= IRSET_ELEM (IRTemp "%arr" (IRPtr (IRArray IRI32 3))) (IRConstInt 2) (IRConstInt 42)

  , testCase "IRALLOC_ARRAY with chars" $
      let instr = IRALLOC_ARRAY "%str" IRChar [IRConstChar 'a', IRConstChar 'b', IRConstChar 'c']
      in instr @?= IRALLOC_ARRAY "%str" IRChar [IRConstChar 'a', IRConstChar 'b', IRConstChar 'c']

  , testCase "IRALLOC_ARRAY empty array" $
      let instr = IRALLOC_ARRAY "%empty" IRI32 []
      in instr @?= IRALLOC_ARRAY "%empty" IRI32 []
  ]

testArrayPrinter :: TestTree
testArrayPrinter = testGroup "Array printer"
  [ testCase "printType for IRArray i32" $
      printType (IRArray IRI32 5) @?= "[i32 x 5]"

  , testCase "printType for IRArray char" $
      printType (IRArray IRChar 10) @?= "[char x 10]"

  , testCase "printType for pointer to IRArray" $
      printType (IRPtr (IRArray IRI32 3)) @?= "*[i32 x 3]"

  , testCase "printType for nested arrays" $
      printType (IRArray (IRArray IRI32 3) 2) @?= "[[i32 x 3] x 2]"

  , testCase "printInstruction IRALLOC_ARRAY" $
      let instr = IRALLOC_ARRAY "%arr" IRI32 [IRConstInt 1, IRConstInt 2, IRConstInt 3]
          expected = "%arr: *[i32 x 3] = ALLOC_ARRAY i32 [1, 2, 3]"
      in printInstruction instr @?= expected

  , testCase "printInstruction IRGET_ELEM" $
      let instr = IRGET_ELEM "%tmp" (IRTemp "%arr" (IRPtr (IRArray IRI32 3))) (IRConstInt 1) IRI32
          expected = "%tmp: i32 = GET_ELEM %arr[1]"
      in printInstruction instr @?= expected

  , testCase "printInstruction IRSET_ELEM" $
      let instr = IRSET_ELEM (IRTemp "%arr" (IRPtr (IRArray IRI32 3))) (IRConstInt 2) (IRConstInt 42)
          expected = "SET_ELEM %arr[2] = 42"
      in printInstruction instr @?= expected

  , testCase "printInstruction IRALLOC_ARRAY with chars" $
      let instr = IRALLOC_ARRAY "%str" IRChar [IRConstChar 'a', IRConstChar 'b']
          expected = "%str: *[char x 2] = ALLOC_ARRAY char ['a', 'b']"
      in printInstruction instr @?= expected

  , testCase "printInstruction IRALLOC_ARRAY empty" $
      let instr = IRALLOC_ARRAY "%empty" IRI32 []
          expected = "%empty: *[i32 x 0] = ALLOC_ARRAY i32 []"
      in printInstruction instr @?= expected

  , testCase "printOperand for array temp" $
      printOperand (IRTemp "%arr" (IRPtr (IRArray IRI32 5))) @?= "%arr"
  ]
