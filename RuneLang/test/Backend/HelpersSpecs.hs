{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module Backend.HelpersSpecs (backendHelpersTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, Assertion)
import Rune.Backend.Helpers
import Rune.IR.Nodes (IRTopLevel(..), IRFunction(..), IRInstruction(..), IRType(..), IROperand(..), IRLabel(..), IRGlobalValue(..))
import qualified Data.Map.Strict as Map

--
-- public
--

backendHelpersTests :: TestTree
backendHelpersTests = testGroup "Rune.Backend.Helpers"
  [ testEmit
  , testEscapeString
  , testCollectTopLevels
  , testCollectIRVars
  , testCalculateStackMap
  , testAlignUp
  , testCollectTopLevel
  , testCollectVars
  , testAccumulateOffset
  , testEncodeCharacter
  , testMakeRbpOffset
  ]

--
-- private
--

testEmit :: TestTree
testEmit = testGroup "emit"
  [ testCase "Emits with zero indentation" $
      emit 0 "test" @?= "test"

  , testCase "Emits with one level indentation" $
      emit 1 "test" @?= "    test"

  , testCase "Emits with two levels indentation" $
      emit 2 "test" @?= "        test"

  , testCase "Emits with three levels indentation" $
      emit 3 "test" @?= "            test"
  ]

testEscapeString :: TestTree
testEscapeString = testGroup "escapeString"
  [ testCase "Escapes newline" $
      escapeString "hello\nworld" @?= "\"hello\",10,\"world\""

  , testCase "Escapes carriage return" $
      escapeString "test\rvalue" @?= "\"test\",13,\"value\""

  , testCase "Escapes tab" $
      escapeString "a\tb" @?= "\"a\",9,\"b\""

  , testCase "Escapes null" $
      escapeString "x\0y" @?= "\"x\",0,\"y\""

  , testCase "Handles plain string" $
      escapeString "hello" @?= "\"hello\""

  , testCase "Handles empty string" $
      escapeString "" @?= ""

  , testCase "Handles multiple escapes" $
      escapeString "\n\t\r" @?= "10,9,13"
  ]

testCollectTopLevels :: TestTree
testCollectTopLevels = testGroup "collectTopLevels"
  [ testCase "Collects externs" $
      let tls = [IRExtern "printf", IRExtern "malloc"]
          (externs, _, _) = collectTopLevels tls
      in externs @?= ["printf", "malloc"]

  , testCase "Collects global strings" $
      let tls = [IRGlobalDef "str1" (IRGlobalStringVal "hello"), IRGlobalDef "str2" (IRGlobalStringVal "world")]
          (_, globals, _) = collectTopLevels tls
      in length globals @?= 2

  , testCase "Collects functions" $
      let func = IRFunction "test" [] (Just IRNull) []
          tls = [IRFunctionDef func]
          (_, _, funcs) = collectTopLevels tls
      in length funcs @?= 1

  , testCase "Filters duplicates in externs" $
      let tls = [IRExtern "printf", IRExtern "printf"]
          (externs, _, _) = collectTopLevels tls
      in externs @?= ["printf"]

  , testCase "Handles mixed top levels" $
      let func = IRFunction "f" [] (Just IRNull) []
          tls = [IRExtern "e", IRGlobalDef "s" (IRGlobalStringVal "v"), IRFunctionDef func]
          (externs, globals, funcs) = collectTopLevels tls
      in do
        externs @?= ["e"]
        globals @?= [("s", IRGlobalStringVal "v")]
        length funcs @?= 1
  ]

testCollectIRVars :: TestTree
testCollectIRVars = testGroup "collectIRVars"
  [ testCase "Collects from params" $
      let func = IRFunction "test" [("x", IRI32), ("y", IRI32)] (Just IRNull) []
          vars = collectIRVars func
      in Map.size vars @?= 2

  , testCase "Collects from IRASSIGN" $
      let func = IRFunction "test" [] (Just IRNull) [IRASSIGN "x" (IRConstInt 1) IRI32]
          vars = collectIRVars func
      in Map.member "x" vars @?= True

  , testCase "Collects from IRALLOC" $
      let func = IRFunction "test" [] (Just IRNull) [IRALLOC "buf" (IRPtr IRChar)]
          vars = collectIRVars func
      in Map.lookup "buf" vars @?= Just (IRPtr IRChar)

  , testCase "Collects from multiple instructions" $
      let func = IRFunction "test" [] (Just IRNull)
            [ IRALLOC "x" IRI32
            , IRASSIGN "y" (IRConstInt 2) IRI32
            , IRADD_OP "z" (IRConstInt 1) (IRConstInt 2) IRI32
            ]
          vars = collectIRVars func
      in Map.size vars @?= 3
  ]

testCalculateStackMap :: TestTree
testCalculateStackMap = testGroup "calculateStackMap"
  [ testCase "Calculates for empty function" $
      let func = IRFunction "empty" [] (Just IRNull) []
          (stackMap, frameSize) = calculateStackMap func
      in do
        Map.size stackMap @?= 0
        frameSize @?= 0

  , testCase "Aligns frame size to 16 bytes" $
      let func = IRFunction "test" [("x", IRI32)] (Just IRNull) []
          (_, frameSize) = calculateStackMap func
      in frameSize `mod` 16 @?= 0

  , testCase "Calculates offsets for variables" $
      let func = IRFunction "test" [("x", IRI32), ("y", IRI64)] (Just IRNull) []
          (stackMap, _) = calculateStackMap func
      in Map.size stackMap @?= 2
  ]

testAlignUp :: TestTree
testAlignUp = testGroup "alignUp"
  [ testCase "Aligns 0 to 16" $
      alignUp 0 16 @?= 0

  , testCase "Aligns 1 to 16" $
      alignUp 1 16 @?= 16

  , testCase "Aligns 15 to 16" $
      alignUp 15 16 @?= 16

  , testCase "Aligns 16 to 16" $
      alignUp 16 16 @?= 16

  , testCase "Aligns 17 to 16" $
      alignUp 17 16 @?= 32

  , testCase "Aligns to 8" $
      alignUp 5 8 @?= 8
  ]

testCollectTopLevel :: TestTree
testCollectTopLevel = testGroup "collectTopLevel"
  [ testCase "Adds extern" $
      let result = collectTopLevel (IRExtern "printf") ([], [], [])
      in result @?= (["printf"], [], [])

  , testCase "Adds global string" $
      let result = collectTopLevel (IRGlobalDef "s" (IRGlobalStringVal "val")) ([], [], [])
      in result @?= ([], [("s", IRGlobalStringVal "val")], [])

  , testCase "Adds function" $
      let func = IRFunction "f" [] (Just IRNull) []
          result = collectTopLevel (IRFunctionDef func) ([], [], [])
      in case result of
        ([], [], [_]) -> return ()
        _ -> assertBool "Expected one function" False

  , testCase "Ignores struct def" $
      let result = collectTopLevel (IRStructDef "S" []) ([], [], [])
      in result @?= ([], [], [])
  ]

testCollectVars :: TestTree
testCollectVars = testGroup "collectVars"
  [ testCase "IRASSIGN: insert var with type" $ 
      check "x" (IRASSIGN "x" op1 IRI32) IRI32
  , testCase "IRALLOC: insert var with type" $ 
      check "ptr" (IRALLOC "ptr" IRI32) IRI32
  , testCase "IRLOAD: insert var with type" $ 
      check "val" (IRLOAD "val" op1 IRI32) IRI32
  , testCase "IRDEREF: insert var with type" $ 
      check "d" (IRDEREF "d" op1 IRI32) IRI32
  , testCase "IRGET_FIELD: insert var with type" $ 
      check "f" (IRGET_FIELD "f" op1 "Struct" "field" IRI32) IRI32
  , testCase "IRADDR: insert var with type" $ 
      check "addr" (IRADDR "addr" "global" (IRPtr IRI32)) (IRPtr IRI32)

  , testCase "IRADD_OP" $ check "res" (IRADD_OP "res" op1 op2 IRI32) IRI32
  , testCase "IRSUB_OP" $ check "res" (IRSUB_OP "res" op1 op2 IRI32) IRI32
  , testCase "IRMUL_OP" $ check "res" (IRMUL_OP "res" op1 op2 IRI32) IRI32
  , testCase "IRDIV_OP" $ check "res" (IRDIV_OP "res" op1 op2 IRI32) IRI32
  , testCase "IRMOD_OP" $ check "res" (IRMOD_OP "res" op1 op2 IRI32) IRI32
  , testCase "IRAND_OP" $ check "res" (IRAND_OP "res" op1 op2 IRI32) IRI32
  , testCase "IROR_OP"  $ check "res" (IROR_OP "res" op1 op2 IRI32) IRI32

  , testCase "IRCMP_EQ -> Bool"  $ check "c" (IRCMP_EQ "c" op1 op2) IRBool
  , testCase "IRCMP_NEQ -> Bool" $ check "c" (IRCMP_NEQ "c" op1 op2) IRBool
  , testCase "IRCMP_LT -> Bool"  $ check "c" (IRCMP_LT "c" op1 op2) IRBool
  , testCase "IRCMP_LTE -> Bool" $ check "c" (IRCMP_LTE "c" op1 op2) IRBool
  , testCase "IRCMP_GT -> Bool"  $ check "c" (IRCMP_GT "c" op1 op2) IRBool
  , testCase "IRCMP_GTE -> Bool" $ check "c" (IRCMP_GTE "c" op1 op2) IRBool

  , testCase "IRCALL (Just Type): use returned type" $
      check "ret" (IRCALL "ret" "func" [] (Just IRF64)) IRF64

  , testCase "IRCALL (Nothing) with Name: defaults to IRI64" $
      check "ret" (IRCALL "ret" "func" [] Nothing) IRI64

  , testCase "IRCALL (Nothing) without Name: ignored" $
      checkEmpty (IRCALL "" "func" [] Nothing)

  , testCase "IRINC ignored" $ checkEmpty (IRINC op1)
  , testCase "IRDEC ignored" $ checkEmpty (IRDEC op1)
  , testCase "IRSTORE ignored" $ checkEmpty (IRSTORE op1 op2)
  , testCase "IRSET_FIELD ignored" $ checkEmpty (IRSET_FIELD op1 "S" "f" op2)
  , testCase "IRRET ignored" $ checkEmpty (IRRET Nothing)
  , testCase "IRJUMP ignored" $ checkEmpty (IRJUMP (IRLabel "L"))
  , testCase "IRLABEL ignored" $ checkEmpty (IRLABEL (IRLabel "L"))
  ]
  where
    op1 = IRConstInt 1
    op2 = IRConstInt 2

    check :: String -> IRInstruction -> IRType -> Assertion
    check name instr expectedType =
      let result = collectVars Map.empty instr
      in Map.lookup name result @?= Just expectedType

    checkEmpty :: IRInstruction -> Assertion
    checkEmpty instr =
      let result = collectVars Map.empty instr
      in Map.null result @?= True

testAccumulateOffset :: TestTree
testAccumulateOffset = testGroup "accumulateOffset"
  [ testCase "Aligns i32 to 4 bytes" $
      let varsMap = Map.singleton "x" IRI32
          (offset, _) = accumulateOffset varsMap (0, Map.empty) ("x", IRI32)
      in offset @?= 4

  , testCase "Aligns i64 to 8 bytes" $
      let varsMap = Map.singleton "x" IRI64
          (offset, _) = accumulateOffset varsMap (0, Map.empty) ("x", IRI64)
      in offset @?= 8

  , testCase "Handles alignment padding" $
      let varsMap = Map.fromList [("a", IRI8), ("b", IRI32)]
          (offset1, map1) = accumulateOffset varsMap (0, Map.empty) ("a", IRI8)
          (offset2, _) = accumulateOffset varsMap (offset1, map1) ("b", IRI32)
      in offset2 @?= 8
  ]

testEncodeCharacter :: TestTree
testEncodeCharacter = testGroup "encodeCharacter"
  [ testCase "Encodes newline" $
      encodeCharacter "\n" @?= ["10"]

  , testCase "Encodes carriage return" $
      encodeCharacter "\r" @?= ["13"]

  , testCase "Encodes tab" $
      encodeCharacter "\t" @?= ["9"]

  , testCase "Encodes null" $
      encodeCharacter "\0" @?= ["0"]

  , testCase "Encodes printable string" $
      encodeCharacter "hello" @?= ["\"hello\""]

  , testCase "Encodes empty string" $
      encodeCharacter "" @?= []

  , testCase "Encodes mixed" $
      encodeCharacter "a\nb" @?= ["\"a\"", "10", "\"b\""]
  ]

testMakeRbpOffset :: TestTree
testMakeRbpOffset = testGroup "makeRbpOffset"
  [ testCase "Calculates offset from start" $
      makeRbpOffset 16 0 @?= (-16)

  , testCase "Calculates offset from middle" $
      makeRbpOffset 16 8 @?= (-8)

  , testCase "Calculates offset from end" $
      makeRbpOffset 16 16 @?= 0

  , testCase "Handles different total sizes" $
      makeRbpOffset 32 16 @?= (-16)
  ]
