module Backend.X86_64.CompareSpec (compareTests) where

import qualified Data.Map.Strict as Map
import Rune.Backend.X86_64.Compare
import Rune.IR.Nodes (IROperand (..), IRType (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- public
--

compareTests :: TestTree
compareTests =
  testGroup
    "Rune.Backend.X86_64.Compare Specs"
    [ testGetCompareSetOp,
      testEmitCompare,
      testFloatComparisons,
      testUnsignedComparisons,
      testSignedComparisons
    ]

--
-- private
--

testGetCompareSetOp :: TestTree
testGetCompareSetOp =
  testGroup
    "getCompareSetOp"
    [ testCase "CmpEQ returns sete" $
        getCompareSetOp CmpEQ IRI32 @?= "sete",
      testCase "CmpNEQ returns setne" $
        getCompareSetOp CmpNEQ IRI32 @?= "setne",
      testCase "CmpLT with signed type returns setl" $
        getCompareSetOp CmpLT IRI32 @?= "setl",
      testCase "CmpLT with unsigned type returns setb" $
        getCompareSetOp CmpLT IRU32 @?= "setb",
      testCase "CmpLT with float type returns setb" $
        getCompareSetOp CmpLT IRF32 @?= "setb",
      testCase "CmpLTE with signed type returns setle" $
        getCompareSetOp CmpLTE IRI64 @?= "setle",
      testCase "CmpLTE with unsigned type returns setbe" $
        getCompareSetOp CmpLTE IRU64 @?= "setbe",
      testCase "CmpLTE with float type returns setbe" $
        getCompareSetOp CmpLTE IRF64 @?= "setbe",
      testCase "CmpGT with signed type returns setg" $
        getCompareSetOp CmpGT IRI16 @?= "setg",
      testCase "CmpGT with unsigned type returns seta" $
        getCompareSetOp CmpGT IRU16 @?= "seta",
      testCase "CmpGT with float type returns seta" $
        getCompareSetOp CmpGT IRF32 @?= "seta",
      testCase "CmpGTE with signed type returns setge" $
        getCompareSetOp CmpGTE IRI8 @?= "setge",
      testCase "CmpGTE with unsigned type returns setae" $
        getCompareSetOp CmpGTE IRU8 @?= "setae",
      testCase "CmpGTE with float type returns setae" $
        getCompareSetOp CmpGTE IRF64 @?= "setae"
    ]

testEmitCompare :: TestTree
testEmitCompare =
  testGroup
    "emitCompare"
    [ testCase "emit integer comparison" $ do
        let sm = Map.fromList [("t1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpEQ (IRTemp "t1" IRI32) (IRConstInt 42)
        length instrs > 0 @?= True,
      testCase "emit char comparison" $ do
        let sm = Map.fromList [("c1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpEQ (IRTemp "c1" IRChar) (IRConstChar 'a')
        length instrs > 0 @?= True,
      testCase "emit bool comparison" $ do
        let sm = Map.fromList [("b1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpEQ (IRTemp "b1" IRBool) (IRConstBool True)
        length instrs > 0 @?= True
    ]

testFloatComparisons :: TestTree
testFloatComparisons =
  testGroup
    "float comparisons"
    [ testCase "F32 comparison uses ucomiss" $ do
        let sm = Map.fromList [("f1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpLT (IRTemp "f1" IRF32) (IRTemp "f1" IRF32)
        any (elem "ucomiss" . words) instrs @?= True,
      testCase "F64 comparison uses ucomisd" $ do
        let sm = Map.fromList [("d1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpGT (IRTemp "d1" IRF64) (IRTemp "d1" IRF64)
        any (elem "ucomisd" . words) instrs @?= True,
      testCase "F32 CmpEQ" $ do
        let sm = Map.fromList [("f1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpEQ (IRTemp "f1" IRF32) (IRTemp "f1" IRF32)
        length instrs > 0 @?= True,
      testCase "F32 CmpNEQ" $ do
        let sm = Map.fromList [("f1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpNEQ (IRTemp "f1" IRF32) (IRTemp "f1" IRF32)
        length instrs > 0 @?= True,
      testCase "F32 CmpLTE" $ do
        let sm = Map.fromList [("f1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpLTE (IRTemp "f1" IRF32) (IRTemp "f1" IRF32)
        length instrs > 0 @?= True,
      testCase "F32 CmpGTE" $ do
        let sm = Map.fromList [("f1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpGTE (IRTemp "f1" IRF32) (IRTemp "f1" IRF32)
        length instrs > 0 @?= True,
      testCase "F64 CmpLT" $ do
        let sm = Map.fromList [("d1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpLT (IRTemp "d1" IRF64) (IRTemp "d1" IRF64)
        length instrs > 0 @?= True,
      testCase "F64 CmpLTE" $ do
        let sm = Map.fromList [("d1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpLTE (IRTemp "d1" IRF64) (IRTemp "d1" IRF64)
        length instrs > 0 @?= True,
      testCase "F64 CmpGTE" $ do
        let sm = Map.fromList [("d1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpGTE (IRTemp "d1" IRF64) (IRTemp "d1" IRF64)
        length instrs > 0 @?= True
    ]

testUnsignedComparisons :: TestTree
testUnsignedComparisons =
  testGroup
    "unsigned type comparisons"
    [ testCase "U8 CmpLT" $ do
        let sm = Map.fromList [("u1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpLT (IRTemp "u1" IRU8) (IRConstInt 100)
        length instrs > 0 @?= True,
      testCase "U16 CmpLTE" $ do
        let sm = Map.fromList [("u1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpLTE (IRTemp "u1" IRU16) (IRConstInt 1000)
        length instrs > 0 @?= True,
      testCase "U32 CmpGT" $ do
        let sm = Map.fromList [("u1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpGT (IRTemp "u1" IRU32) (IRConstInt 50000)
        length instrs > 0 @?= True,
      testCase "U64 CmpGTE" $ do
        let sm = Map.fromList [("u1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpGTE (IRTemp "u1" IRU64) (IRConstInt 123456)
        length instrs > 0 @?= True,
      testCase "U8 CmpEQ" $ do
        let sm = Map.fromList [("u1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpEQ (IRTemp "u1" IRU8) (IRConstInt 255)
        length instrs > 0 @?= True,
      testCase "U32 CmpNEQ" $ do
        let sm = Map.fromList [("u1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpNEQ (IRTemp "u1" IRU32) (IRConstInt 0)
        length instrs > 0 @?= True
    ]

testSignedComparisons :: TestTree
testSignedComparisons =
  testGroup
    "signed type comparisons"
    [ testCase "I8 CmpLT" $ do
        let sm = Map.fromList [("i1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpLT (IRTemp "i1" IRI8) (IRConstInt (-10))
        length instrs > 0 @?= True,
      testCase "I16 CmpLTE" $ do
        let sm = Map.fromList [("i1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpLTE (IRTemp "i1" IRI16) (IRConstInt 500)
        length instrs > 0 @?= True,
      testCase "I32 CmpGT" $ do
        let sm = Map.fromList [("i1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpGT (IRTemp "i1" IRI32) (IRConstInt 0)
        length instrs > 0 @?= True,
      testCase "I64 CmpGTE" $ do
        let sm = Map.fromList [("i1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpGTE (IRTemp "i1" IRI64) (IRConstInt 999999)
        length instrs > 0 @?= True,
      testCase "I8 CmpEQ" $ do
        let sm = Map.fromList [("i1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpEQ (IRTemp "i1" IRI8) (IRConstInt 127)
        length instrs > 0 @?= True,
      testCase "I16 CmpNEQ" $ do
        let sm = Map.fromList [("i1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpNEQ (IRTemp "i1" IRI16) (IRConstInt (-1))
        length instrs > 0 @?= True,
      testCase "IRChar CmpLT (unsigned)" $ do
        let sm = Map.fromList [("c1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpLT (IRTemp "c1" IRChar) (IRConstChar 'z')
        length instrs > 0 @?= True,
      testCase "IRBool comparison" $ do
        let sm = Map.fromList [("b1", -8), ("result", -16)]
            instrs = emitCompare sm "result" CmpGT (IRTemp "b1" IRBool) (IRConstBool False)
        length instrs > 0 @?= True
    ]
