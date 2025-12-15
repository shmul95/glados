{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module Backend.X86_64.CompareSpecs (compareTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.Backend.X86_64.Compare
import Rune.IR.Nodes (IRType(..), IROperand(..))
import qualified Data.Map.Strict as Map

--
-- public
--

compareTests :: TestTree
compareTests = testGroup "Rune.Backend.X86_64.Compare"
  [ testGetCompareSetOp
  , testIsSignedType
  , testIsUnsignedType
  , testIsFloatType
  , testGetFloatRegs
  , testStackAddr
  , testLoadOperand
  , testLoadFloatOperand
  , testGetSizeSpec
  , testEmitCompare
  ]

--
-- private
--

testGetCompareSetOp :: TestTree
testGetCompareSetOp = testGroup "getCompareSetOp"
  [ testCase "CmpEQ returns sete" $
      getCompareSetOp CmpEQ IRI32 @?= "sete"

  , testCase "CmpNEQ returns setne" $
      getCompareSetOp CmpNEQ IRI32 @?= "setne"

  , testCase "CmpLT for signed returns setl" $
      getCompareSetOp CmpLT IRI32 @?= "setl"

  , testCase "CmpLT for unsigned returns setb" $
      getCompareSetOp CmpLT IRU32 @?= "setb"

  , testCase "CmpLT for float returns setb" $
      getCompareSetOp CmpLT IRF32 @?= "setb"

  , testCase "CmpLTE for signed returns setle" $
      getCompareSetOp CmpLTE IRI32 @?= "setle"

  , testCase "CmpLTE for unsigned returns setbe" $
      getCompareSetOp CmpLTE IRU32 @?= "setbe"

  , testCase "CmpGT for signed returns setg" $
      getCompareSetOp CmpGT IRI32 @?= "setg"

  , testCase "CmpGT for unsigned returns seta" $
      getCompareSetOp CmpGT IRU32 @?= "seta"

  , testCase "CmpGTE for signed returns setge" $
      getCompareSetOp CmpGTE IRI32 @?= "setge"

  , testCase "CmpGTE for unsigned returns setae" $
      getCompareSetOp CmpGTE IRU32 @?= "setae"
  ]

testIsSignedType :: TestTree
testIsSignedType = testGroup "isSignedType"
  [ testCase "Returns true for signed types" $ do
      isSignedType IRI8 @?= True
      isSignedType IRI16 @?= True
      isSignedType IRI32 @?= True
      isSignedType IRI64 @?= True

  , testCase "Returns false for unsigned types" $ do
      isSignedType IRU8 @?= False
      isSignedType IRU16 @?= False
      isSignedType IRU32 @?= False
      isSignedType IRU64 @?= False

  , testCase "Returns false for char" $
      isSignedType IRChar @?= False

  , testCase "Returns false for bool" $
      isSignedType IRBool @?= False

  , testCase "Returns false for float types" $ do
      isSignedType IRF32 @?= False
      isSignedType IRF64 @?= False
  ]

testIsUnsignedType :: TestTree
testIsUnsignedType = testGroup "isUnsignedType"
  [ testCase "Returns true for unsigned types" $ do
      isUnsignedType IRU8 @?= True
      isUnsignedType IRU16 @?= True
      isUnsignedType IRU32 @?= True
      isUnsignedType IRU64 @?= True

  , testCase "Returns true for char and bool" $ do
      isUnsignedType IRChar @?= True
      isUnsignedType IRBool @?= True

  , testCase "Returns false for signed types" $ do
      isUnsignedType IRI8 @?= False
      isUnsignedType IRI32 @?= False
  ]

testIsFloatType :: TestTree
testIsFloatType = testGroup "isFloatType"
  [ testCase "Returns true for float types" $ do
      isFloatType IRF32 @?= True
      isFloatType IRF64 @?= True

  , testCase "Returns false for integer types" $ do
      isFloatType IRI32 @?= False
      isFloatType IRU64 @?= False
  ]

testGetFloatRegs :: TestTree
testGetFloatRegs = testGroup "getFloatRegs"
  [ testCase "Returns xmm0, xmm1 for IRF32" $
      getFloatRegs IRF32 @?= ("xmm0", "xmm1")

  , testCase "Returns xmm0, xmm1 for IRF64" $
      getFloatRegs IRF64 @?= ("xmm0", "xmm1")

  , testCase "Returns xmm0, xmm1 for other types" $
      getFloatRegs IRI32 @?= ("xmm0", "xmm1")
  ]

testStackAddr :: TestTree
testStackAddr = testGroup "stackAddr"
  [ testCase "Returns address for positive offset" $
      let sm = Map.singleton "x" 8
      in stackAddr sm "x" @?= "[rbp8]"

  , testCase "Returns address for negative offset" $
      let sm = Map.singleton "y" (-8)
      in stackAddr sm "y" @?= "[rbp-8]"

  , testCase "Returns address for zero offset" $
      let sm = Map.singleton "z" 0
      in stackAddr sm "z" @?= "[rbp0]"
  ]

testLoadOperand :: TestTree
testLoadOperand = testGroup "loadOperand"
  [ testCase "Loads constant int" $
      let result = loadOperand Map.empty "rax" (IRConstInt 42) IRI32
      in result @?= ["    mov rax, 42"]

  , testCase "Loads constant char" $
      let result = loadOperand Map.empty "rax" (IRConstChar 'A') IRChar
      in result @?= ["    mov rax, 65"]

  , testCase "Loads constant bool true" $
      let result = loadOperand Map.empty "rax" (IRConstBool True) IRBool
      in result @?= ["    mov rax, 1"]

  , testCase "Loads constant bool false" $
      let result = loadOperand Map.empty "rax" (IRConstBool False) IRBool
      in result @?= ["    mov rax, 0"]

  , testCase "Loads temp from stack" $
      let sm = Map.singleton "x" (-8)
          result = loadOperand sm "rax" (IRTemp "x" IRI32) IRI32
      in assertBool "Should load from stack" $ not $ null result

  , testCase "Handles unknown operand" $
      let result = loadOperand Map.empty "rax" IRConstNull IRNull
      in result @?= ["    mov rax, 0"]
  ]

testLoadFloatOperand :: TestTree
testLoadFloatOperand = testGroup "loadFloatOperand"
  [ testCase "Loads F32 temp with movss" $
      let sm = Map.singleton "f" (-8)
          result = loadFloatOperand sm "xmm0" (IRTemp "f" IRF32) IRF32
      in assertBool "Should use movss" $ any ("movss" `elem`) (map words result)

  , testCase "Loads F64 temp with movsd" $
      let sm = Map.singleton "d" (-8)
          result = loadFloatOperand sm "xmm0" (IRTemp "d" IRF64) IRF64
      in assertBool "Should use movsd" $ any ("movsd" `elem`) (map words result)

  , testCase "Handles unknown float operand" $
      let result = loadFloatOperand Map.empty "xmm0" IRConstNull IRF32
      in assertBool "Should use xorps" $ any ("xorps" `elem`) (map words result)
  ]

testGetSizeSpec :: TestTree
testGetSizeSpec = testGroup "getSizeSpec"
  [ testCase "Returns byte for 1-byte types" $ do
      getSizeSpec IRI8 @?= "byte"
      getSizeSpec IRU8 @?= "byte"
      getSizeSpec IRChar @?= "byte"
      getSizeSpec IRBool @?= "byte"

  , testCase "Returns word for 2-byte types" $ do
      getSizeSpec IRI16 @?= "word"
      getSizeSpec IRU16 @?= "word"

  , testCase "Returns dword for 4-byte types" $ do
      getSizeSpec IRI32 @?= "dword"
      getSizeSpec IRU32 @?= "dword"
      getSizeSpec IRF32 @?= "dword"

  , testCase "Returns qword for 8-byte types" $ do
      getSizeSpec IRI64 @?= "qword"
      getSizeSpec IRU64 @?= "qword"
      getSizeSpec IRF64 @?= "qword"

  , testCase "Returns qword for pointers" $
      getSizeSpec (IRPtr IRI32) @?= "qword"
  ]

testEmitCompare :: TestTree
testEmitCompare = testGroup "emitCompare"
  [ testCase "Generates integer comparison" $
      let sm = Map.singleton "result" (-8)
          result = emitCompare sm "result" CmpEQ (IRConstInt 1) (IRConstInt 2)
      in assertBool "Should generate comparison" $ not $ null result

  , testCase "Generates comparison with temps" $
      let sm = Map.fromList [("x", (-8)), ("y", (-16)), ("result", (-4))]
          result = emitCompare sm "result" CmpLT (IRTemp "x" IRI32) (IRTemp "y" IRI32)
      in assertBool "Should generate comparison" $ not $ null result
  ]
