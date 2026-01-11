{-# LANGUAGE OverloadedStrings #-}

module Backend.X86_64.StructSpecs (structTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import qualified Data.Map.Strict as Map
import Rune.Backend.X86_64.Struct
import Rune.IR.Nodes (IRType(..), IROperand(..))

structTests :: TestTree
structTests = testGroup "Rune.Backend.X86_64.Struct"
  [ testSizeOfStruct
  , testGetFieldOffset
  , testEmitGetField
  , testEmitSetField
  ]

-- Mock data
mockStructs :: Map.Map String [(String, IRType)]
mockStructs = Map.fromList
  [ ("Point", [("x", IRI32), ("y", IRI32)])
  , ("Mixed", [("a", IRI8), ("b", IRI64), ("c", IRI32)]) -- a:0, padding:1-7, b:8, c:16 -> size 24 (aligned to 8)
  , ("FloatStruct", [("f", IRF32), ("d", IRF64)])
  ]

mockStackMap :: Map.Map String Int
mockStackMap = Map.fromList [("base", -8), ("dest", -16)]

testSizeOfStruct :: TestTree
testSizeOfStruct = testGroup "sizeOfStruct"
  [ testCase "Calculates simple struct size" $
      sizeOfStruct mockStructs "Point" @?= 8
  , testCase "Calculates mixed struct size with padding" $
      sizeOfStruct mockStructs "Mixed" @?= 24
  , testCase "Calculates float struct size" $
      sizeOfStruct mockStructs "FloatStruct" @?= 16
  ]

testGetFieldOffset :: TestTree
testGetFieldOffset = testGroup "getFieldOffset"
  [ testCase "Gets first field offset" $
      getFieldOffset mockStructs "Point" "x" @?= 0
  , testCase "Gets second field offset" $
      getFieldOffset mockStructs "Point" "y" @?= 4
  , testCase "Gets offset with alignment padding" $
      getFieldOffset mockStructs "Mixed" "b" @?= 8
  , testCase "Gets last field offset" $
      getFieldOffset mockStructs "Mixed" "c" @?= 16
  ]

testEmitGetField :: TestTree
testEmitGetField = testGroup "emitGetField"
  [ testCase "Gets integer field" $
      let result = emitGetField mockStackMap mockStructs "dest" (IRTemp "base" (IRPtr (IRStruct "Point"))) "Point" "y" IRI32
      in assertBool "should load base, offset, and store" $
           any (== "    mov rdi, qword [rbp-8]") result &&
           any (== "    mov eax, dword [rdi + 4]") result &&
           any (== "    mov dword [rbp-16], eax") result
  , testCase "Gets float field" $
      let result = emitGetField mockStackMap mockStructs "dest" (IRTemp "base" (IRPtr (IRStruct "FloatStruct"))) "FloatStruct" "f" IRF32
      in assertBool "should use float registers" $
           any (== "    movss xmm0, dword [rdi + 0]") result &&
           any (== "    movss dword [rbp-16], xmm0") result
  ]

testEmitSetField :: TestTree
testEmitSetField = testGroup "emitSetField"
  [ testCase "Sets integer field" $
      let result = emitSetField mockStackMap mockStructs (IRTemp "base" (IRPtr (IRStruct "Point"))) "Point" "y" (IRConstInt 42)
      in assertBool "should store to offset" $
           any (== "    mov dword [rdi + 4], eax") result
  , testCase "Sets float field" $
      let result = emitSetField mockStackMap mockStructs (IRTemp "base" (IRPtr (IRStruct "FloatStruct"))) "FloatStruct" "d" (IRConstFloat 3.14)
      in assertBool "should use float registers" $
           any (== "    movsd qword [rdi + 8], xmm0") result
  ]
