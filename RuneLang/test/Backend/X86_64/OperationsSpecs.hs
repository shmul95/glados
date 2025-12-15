module Backend.X86_64.OperationsSpecs (operationsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Rune.Backend.X86_64.Operations
import Rune.IR.Nodes as I
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)

--
-- public
--

operationsTests :: TestTree
operationsTests = testGroup "Rune.Backend.X86_64.Operations"
  [ testEmitBinaryOp
  , testEmitDivOp
  , testEmitModOp
  , testEmitFloatBinaryOp
  , testEmitFloatDivOp
  , testEmitIntDivOp
  , testEmitIntModOp
  , testEmitSmallMul
  ]

--
-- private
--

testEmitBinaryOp :: TestTree
testEmitBinaryOp = testGroup "emitBinaryOp"
  [ testCase "Emits integer add" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitBinaryOp sm "dest" "add" (IRConstInt 1) (IRConstInt 2) IRI32
      in do
        assertBool "Contains add instruction" $ any ("add eax, ebx" ==) (map (drop 4) instrs)
        assertBool "Stores result" $ any ("mov dword [rbp-4], eax" ==) (map (drop 4) instrs)

  , testCase "Redirects to float op" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitBinaryOp sm "dest" "add" (IRConstFloat 1.0) (IRConstFloat 2.0) IRF32
      in assertBool "Uses addss" $ any (isInfixOf "addss") instrs

  , testCase "Redirects to small mul (IRI16)" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitBinaryOp sm "dest" "imul" (IRConstInt 1) (IRConstInt 2) IRI16
      in assertBool "Uses imul eax, edx" $ elem "imul eax, edx" (map (drop 4) instrs)

  , testCase "Redirects to small mul (IRU16)" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitBinaryOp sm "dest" "imul" (IRConstInt 1) (IRConstInt 2) IRU16
      in assertBool "Uses imul eax, edx" $ elem "imul eax, edx" (map (drop 4) instrs)
  ]

testEmitDivOp :: TestTree
testEmitDivOp = testGroup "emitDivOp"
  [ testCase "Emits integer div" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRI32
      in assertBool "Uses idiv" $ elem "idiv ebx" (map (drop 4) instrs)

  , testCase "Redirects to float div" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitDivOp sm "dest" (IRConstFloat 10.0) (IRConstFloat 2.0) IRF32
      in assertBool "Uses divss" $ any (isInfixOf "divss") instrs
  ]

testEmitModOp :: TestTree
testEmitModOp = testGroup "emitModOp"
  [ testCase "Emits integer mod" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitModOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRI32
      in assertBool "Uses idiv" $ any ("idiv" `isInfixOf`) instrs

  , testCase "Float mod returns TODO" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitModOp sm "dest" (IRConstFloat 10.0) (IRConstFloat 2.0) IRF32
      in assertBool "Returns TODO" $ any ("TODO" `isInfixOf`) instrs
  ]

testEmitFloatBinaryOp :: TestTree
testEmitFloatBinaryOp = testGroup "emitFloatBinaryOp"
  [ testCase "Emits addss for IRF32" $
      let sm = Map.fromList [("dest", -4), ("a", -8), ("b", -12)] -- FIX: Added "a", "b"
          instrs = emitFloatBinaryOp sm "dest" "add" (IRTemp "a" IRF32) (IRTemp "b" IRF32) IRF32
      in do
        assertBool "Uses addss" $ elem "addss xmm0, xmm1" (map (drop 4) instrs)
        assertBool "Stores dword" $ any ("movss dword" `isInfixOf`) (map (drop 4) instrs)

  , testCase "Emits subsd for IRF64" $
      let sm = Map.fromList [("dest", -8), ("a", -16), ("b", -24)] -- FIX: Added "a", "b"
          instrs = emitFloatBinaryOp sm "dest" "sub" (IRTemp "a" IRF64) (IRTemp "b" IRF64) IRF64
      in do
        assertBool "Uses subsd" $ elem "subsd xmm0, xmm1" (map (drop 4) instrs)
        assertBool "Stores qword" $ any ("movsd qword" `isInfixOf`) (map (drop 4) instrs)

  , testCase "Emits mulss for IRF32" $
      let sm = Map.fromList [("dest", -4), ("a", -8), ("b", -12)] -- FIX: Added "a", "b"
          instrs = emitFloatBinaryOp sm "dest" "imul" (IRTemp "a" IRF32) (IRTemp "b" IRF32) IRF32
      in assertBool "Uses mulss" $ elem "mulss xmm0, xmm1" (map (drop 4) instrs)

  , testCase "Emits unsupported float binary op" $
      let sm = Map.fromList [("dest", -4), ("a", -8), ("b", -12)] -- FIX: Added "a", "b"
          instrs = emitFloatBinaryOp sm "dest" "xor" (IRTemp "a" IRF32) (IRTemp "b" IRF32) IRF32
      in assertBool "Uses movss fallback" $ elem "movss xmm0, xmm1" (map (drop 4) instrs)

  , testCase "Unsupported float binary result type returns TODO" $
      let sm = Map.fromList [("dest", -8), ("a", -16), ("b", -24)] -- FIX: Added "a", "b"
          instrs = emitFloatBinaryOp sm "dest" "add" (IRTemp "a" IRF64) (IRTemp "b" IRF64) (IRPtr IRF64)
      in assertBool "Returns TODO" $ any ("TODO" `isInfixOf`) instrs
  ]

testEmitFloatDivOp :: TestTree
testEmitFloatDivOp = testGroup "emitFloatDivOp"
  [ testCase "Emits divss for IRF32" $
      let sm = Map.fromList [("dest", -4), ("a", -8), ("b", -12)] -- FIX: Added "a", "b"
          instrs = emitFloatDivOp sm "dest" (IRTemp "a" IRF32) (IRTemp "b" IRF32) IRF32
      in do
        assertBool "Uses divss" $ elem "divss xmm0, xmm1" (map (drop 4) instrs)
        assertBool "Stores dword" $ any ("movss dword" `isInfixOf`) (map (drop 4) instrs)

  , testCase "Emits divsd for IRF64" $
      let sm = Map.fromList [("dest", -8), ("a", -16), ("b", -24)] -- FIX: Added "a", "b"
          instrs = emitFloatDivOp sm "dest" (IRTemp "a" IRF64) (IRTemp "b" IRF64) IRF64
      in do
        assertBool "Uses divsd" $ elem "divsd xmm0, xmm1" (map (drop 4) instrs)
        assertBool "Stores qword" $ any ("movsd qword" `isInfixOf`) (map (drop 4) instrs)

  , testCase "Unsupported float div result type returns TODO" $
      let sm = Map.fromList [("dest", -8), ("a", -16), ("b", -24)] -- FIX: Added "a", "b"
          instrs = emitFloatDivOp sm "dest" (IRTemp "a" IRF64) (IRTemp "b" IRF64) (IRPtr IRF64)
      in assertBool "Returns TODO" $ any ("TODO" `isInfixOf`) instrs
  ]

testEmitIntDivOp :: TestTree
testEmitIntDivOp = testGroup "emitIntDivOp"
  [ testCase "Emits small div for i8" $
      let sm = Map.singleton "dest" (-1)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRI8
      in do
        assertBool "Uses idiv ecx" $ elem "idiv ecx" (map (drop 4) instrs)
        assertBool "Uses cdq" $ elem "cdq" (map (drop 4) instrs)

  , testCase "Emits unsigned small div for u8" $
      let sm = Map.singleton "dest" (-1)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRU8
      in do
        assertBool "Uses div ecx" $ elem "div ecx" (map (drop 4) instrs)
        assertBool "Uses xor edx, edx" $ elem "xor edx, edx" (map (drop 4) instrs)

  , testCase "Emits signed small div for i16" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRI16
      in do
        assertBool "Uses idiv ecx" $ elem "idiv ecx" (map (drop 4) instrs)
        assertBool "Uses cdq" $ elem "cdq" (map (drop 4) instrs)

  , testCase "Emits unsigned small div for u16" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRU16
      in do
        assertBool "Uses div ecx" $ elem "div ecx" (map (drop 4) instrs)
        assertBool "Uses xor edx, edx" $ elem "xor edx, edx" (map (drop 4) instrs)

  , testCase "Emits 32-bit signed div" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRI32
      in assertBool "Uses cdq" $ elem "cdq" (map (drop 4) instrs)

  , testCase "Emits unsigned 32-bit div" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRU32
      in do
        assertBool "Uses xor edx, edx" $ elem "xor edx, edx" (map (drop 4) instrs)
        assertBool "Uses div ebx" $ elem "div ebx" (map (drop 4) instrs)

  , testCase "Emits 64-bit signed div" $
      let sm = Map.singleton "dest" (-8)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRI64
      in do
        assertBool "Uses cqo" $ elem "cqo" (map (drop 4) instrs)
        assertBool "Uses idiv rbx" $ elem "idiv rbx" (map (drop 4) instrs)

  , testCase "Emits unsigned 64-bit div" $
      let sm = Map.singleton "dest" (-8)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) IRU64
      in do
        assertBool "Uses xor rdx" $ elem "xor rdx, rdx" (map (drop 4) instrs)
        assertBool "Uses div rbx" $ elem "div rbx" (map (drop 4) instrs)

  , testCase "Unsupported integer division type returns TODO" $
      let sm = Map.singleton "dest" (-1)
          instrs = emitIntDivOp sm "dest" (IRConstInt 10) (IRConstInt 2) I.IRBool
      in assertBool "Returns TODO" $ any ("TODO" `isInfixOf`) instrs
  ]

testEmitIntModOp :: TestTree
testEmitIntModOp = testGroup "emitIntModOp"
  [ testCase "Emits small mod for i8" $
      let sm = Map.singleton "dest" (-1)
          instrs = emitIntModOp sm "dest" (IRConstInt 10) (IRConstInt 3) IRI8
      in do
        assertBool "Uses idiv" $ any ("idiv ecx" `isInfixOf`) instrs
        assertBool "Stores remainder" $ any ("mov byte [rbp-1], dl" ==) (map (drop 4) instrs)

  , testCase "Emits unsigned small mod for u8" $
      let sm = Map.singleton "dest" (-1)
          instrs = emitIntModOp sm "dest" (IRConstInt 10) (IRConstInt 3) IRU8
      in do
        assertBool "Uses div" $ any ("div ecx" `isInfixOf`) instrs
        assertBool "Uses xor edx, edx" $ any ("xor edx, edx" `isInfixOf`) instrs
        assertBool "Stores remainder" $ any ("mov byte [rbp-1], dl" ==) (map (drop 4) instrs)

  , testCase "Emits signed small mod for i16" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitIntModOp sm "dest" (IRConstInt 10) (IRConstInt 3) IRI16
      in do
        assertBool "Uses idiv" $ any ("idiv ecx" `isInfixOf`) instrs
        assertBool "Uses cdq" $ any ("cdq" `isInfixOf`) instrs
        assertBool "Stores remainder" $ any ("mov word [rbp-2], dx" ==) (map (drop 4) instrs)

  , testCase "Emits unsigned small mod for u16" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitIntModOp sm "dest" (IRConstInt 10) (IRConstInt 3) IRU16
      in do
        assertBool "Uses div" $ any ("div ecx" `isInfixOf`) instrs
        assertBool "Uses xor edx, edx" $ any ("xor edx, edx" `isInfixOf`) instrs
        assertBool "Stores remainder" $ any ("mov word [rbp-2], dx" ==) (map (drop 4) instrs)

  , testCase "Emits mod for i32" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitIntModOp sm "dest" (IRConstInt 10) (IRConstInt 3) IRI32
      in do
        assertBool "Uses cdq" $ elem "cdq" (map (drop 4) instrs)
        assertBool "Uses idiv" $ elem "idiv ebx" (map (drop 4) instrs)
        assertBool "Stores edx" $ elem "mov dword [rbp-4], edx" (map (drop 4) instrs)

  , testCase "Emits mod for u32" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitIntModOp sm "dest" (IRConstInt 10) (IRConstInt 3) IRU32
      in do
        assertBool "Uses xor edx" $ elem "xor edx, edx" (map (drop 4) instrs)
        assertBool "Uses div" $ elem "div ebx" (map (drop 4) instrs)
        assertBool "Stores edx" $ elem "mov dword [rbp-4], edx" (map (drop 4) instrs)

  , testCase "Emits mod for i64" $
      let sm = Map.singleton "dest" (-8)
          instrs = emitIntModOp sm "dest" (IRConstInt 10) (IRConstInt 3) IRI64
      in do
        assertBool "Uses cqo" $ elem "cqo" (map (drop 4) instrs)
        assertBool "Uses idiv" $ elem "idiv rbx" (map (drop 4) instrs)
        assertBool "Stores rdx" $ elem "mov qword [rbp-8], rdx" (map (drop 4) instrs)

  , testCase "Emits mod for u64" $
      let sm = Map.singleton "dest" (-8)
          instrs = emitIntModOp sm "dest" (IRConstInt 10) (IRConstInt 3) IRU64
      in do
        assertBool "Uses xor rdx" $ elem "xor rdx, rdx" (map (drop 4) instrs)
        assertBool "Uses div" $ elem "div rbx" (map (drop 4) instrs)
        assertBool "Stores rdx" $ elem "mov qword [rbp-8], rdx" (map (drop 4) instrs)

  , testCase "Unsupported integer modulo type returns TODO" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitModOp sm "dest" (IRConstInt 10) (IRConstInt 2) (IRPtr IRI32)
      in assertBool "Returns TODO" $ any ("TODO" `isInfixOf`) instrs
  ]

testEmitSmallMul :: TestTree
testEmitSmallMul = testGroup "emitSmallMul"
  [ testCase "Emits mul for i8" $
      let sm = Map.singleton "dest" (-1)
          instrs = emitSmallMul sm "dest" (IRConstInt 10) (IRConstInt 2) IRI8
      in do
        assertBool "Uses movsx" $ any ("movsx eax, al" `isInfixOf`) (map (drop 4) instrs)
        assertBool "Stores result" $ any ("mov byte [rbp-1], al" ==) (map (drop 4) instrs)

  , testCase "Emits mul for u8" $
      let sm = Map.singleton "dest" (-1)
          instrs = emitSmallMul sm "dest" (IRConstInt 10) (IRConstInt 2) IRU8
      in do
        assertBool "Uses movzx" $ any ("movzx eax, al" `isInfixOf`) (map (drop 4) instrs)
        assertBool "Stores result" $ any ("mov byte [rbp-1], al" ==) (map (drop 4) instrs)

  , testCase "Emits mul for i16" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitSmallMul sm "dest" (IRConstInt 10) (IRConstInt 2) IRI16
      in do
        assertBool "Uses movsx" $ any ("movsx eax, ax" `isInfixOf`) (map (drop 4) instrs)
        assertBool "Uses imul eax, edx" $ elem "imul eax, edx" (map (drop 4) instrs)
        assertBool "Stores result" $ any ("mov word [rbp-2], ax" ==) (map (drop 4) instrs)

  , testCase "Emits mul for u16" $
      let sm = Map.singleton "dest" (-2)
          instrs = emitSmallMul sm "dest" (IRConstInt 10) (IRConstInt 2) IRU16
      in do
        assertBool "Uses movzx" $ any ("movzx eax, ax" `isInfixOf`) (map (drop 4) instrs)
        assertBool "Uses imul eax, edx" $ elem "imul eax, edx" (map (drop 4) instrs)
        assertBool "Stores result" $ any ("mov word [rbp-2], ax" ==) (map (drop 4) instrs)

  , testCase "Non-small integer mul returns empty list" $
      let sm = Map.singleton "dest" (-4)
          instrs = emitSmallMul sm "dest" (IRConstInt 10) (IRConstInt 2) IRI32
      in assertBool "Returns []" $ null instrs
  ]
