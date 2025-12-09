module Backend.HelpersSpec (backendHelpersTests) where

import qualified Data.Map.Strict as Map
import Rune.Backend.Helpers
import Rune.IR.Nodes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

backendHelpersTests :: TestTree
backendHelpersTests =
  testGroup
    "Rune.Backend.Helpers Specs"
    [ testEmit,
      testEscapeString,
      testCollectTopLevels,
      testCalculateStackMap,
      testCollectIRVars
    ]

testEmit :: TestTree
testEmit =
  testGroup
    "emit"
    [ testCase "emit at level 0" $ emit 0 "mov rax, 0" @?= "mov rax, 0",
      testCase "emit at level 1" $ emit 1 "mov rax, 0" @?= "    mov rax, 0",
      testCase "emit at level 2" $ emit 2 "mov rax, 0" @?= "        mov rax, 0",
      testCase "emit at level 3" $ emit 3 "ret" @?= "            ret"
    ]

testEscapeString :: TestTree
testEscapeString =
  testGroup
    "escapeString"
    [ testCase "simple string" $ escapeString "hello" @?= "\"hello\"",
      testCase "string with newline" $ escapeString "hello\nworld" @?= "\"hello\",10,\"world\"",
      testCase "string with tab" $ escapeString "a\tb" @?= "\"a\",9,\"b\"",
      testCase "string with carriage return" $ escapeString "x\ry" @?= "\"x\",13,\"y\"",
      testCase "string with null" $ escapeString "a\0b" @?= "\"a\",0,\"b\"",
      testCase "empty string" $ escapeString "" @?= "",
      testCase "only special chars" $ escapeString "\n\t\r" @?= "10,9,13"
    ]

testCollectTopLevels :: TestTree
testCollectTopLevels =
  testGroup
    "collectTopLevels"
    [ testCase "empty list" $
        let (externs, globals, funcs) = collectTopLevels []
         in do
              externs @?= []
              globals @?= []
              funcs @?= [],
      testCase "collect externs" $
        let topLevels = [IRExtern "printf", IRExtern "malloc", IRExtern "printf"]
            (externs, globals, funcs) = collectTopLevels topLevels
         in do
              length externs @?= 2
              globals @?= []
              funcs @?= [],
      testCase "collect global strings" $
        let topLevels = [IRGlobalString "str0" "hello", IRGlobalString "str1" "world"]
            (externs, globals, funcs) = collectTopLevels topLevels
         in do
              externs @?= []
              length globals @?= 2
              funcs @?= [],
      testCase "collect functions" $
        let func1 = IRFunction "main" [] Nothing []
            func2 = IRFunction "helper" [] Nothing []
            topLevels = [IRFunctionDef func1, IRFunctionDef func2]
            (externs, globals, funcs) = collectTopLevels topLevels
         in do
              externs @?= []
              globals @?= []
              length funcs @?= 2,
      testCase "mixed top levels" $
        let func = IRFunction "main" [] Nothing []
            topLevels =
              [ IRExtern "printf",
                IRGlobalString "str0" "test",
                IRFunctionDef func
              ]
            (externs, globals, funcs) = collectTopLevels topLevels
         in do
              length externs @?= 1
              length globals @?= 1
              length funcs @?= 1
    ]

testCalculateStackMap :: TestTree
testCalculateStackMap =
  testGroup
    "calculateStackMap"
    [ testCase "function with no variables" $
        let func = IRFunction "empty" [] Nothing []
            (stackMap, totalSize) = calculateStackMap func
         in do
              Map.null stackMap @?= True
              totalSize @?= 0,
      testCase "function with parameters only" $
        let func = IRFunction "test" [("a", IRI32), ("b", IRI32)] Nothing []
            (sm, totalSize) = calculateStackMap func
         in do
              Map.size sm @?= 2
              totalSize >= 8 @?= True,
      testCase "function with IRALLOC" $
        let body = [IRALLOC "x" IRI64, IRALLOC "y" IRI32]
            func = IRFunction "test" [] Nothing body
            (stackMap, totalSize) = calculateStackMap func
         in do
              Map.size stackMap @?= 2
              totalSize >= 12 @?= True,
      testCase "alignment to 16 bytes" $
        let body = [IRALLOC "x" IRI8]
            func = IRFunction "test" [] Nothing body
            (_, totalSize) = calculateStackMap func
         in totalSize `mod` 16 @?= 0
    ]

testCollectIRVars :: TestTree
testCollectIRVars =
  testGroup
    "collectIRVars"
    [ testCase "parameters only" $
        let func = IRFunction "test" [("a", IRI32), ("b", IRI64)] Nothing []
            varsMap = collectIRVars func
         in do
              Map.size varsMap @?= 2
              Map.lookup "a" varsMap @?= Just IRI32
              Map.lookup "b" varsMap @?= Just IRI64,
      testCase "IRASSIGN variables" $
        let body = [IRASSIGN "temp" (IRConstInt 5) IRI32]
            func = IRFunction "test" [] Nothing body
            varsMap = collectIRVars func
         in Map.lookup "temp" varsMap @?= Just IRI32,
      testCase "IRLOAD variables" $
        let body = [IRLOAD "loaded" (IRTemp "ptr" (IRPtr IRI32)) IRI32]
            func = IRFunction "test" [] Nothing body
            varsMap = collectIRVars func
         in Map.lookup "loaded" varsMap @?= Just IRI32,
      testCase "arithmetic operations" $
        let body =
              [ IRADD_OP "r1" (IRConstInt 1) (IRConstInt 2) IRI32,
                IRSUB_OP "r2" (IRConstInt 5) (IRConstInt 3) IRI32,
                IRMUL_OP "r3" (IRConstInt 2) (IRConstInt 3) IRI32,
                IRDIV_OP "r4" (IRConstInt 6) (IRConstInt 2) IRI32,
                IRMOD_OP "r5" (IRConstInt 7) (IRConstInt 3) IRI32
              ]
            func = IRFunction "test" [] Nothing body
            varsMap = collectIRVars func
         in Map.size varsMap @?= 5,
      testCase "comparison operations" $
        let body =
              [ IRCMP_EQ "c1" (IRConstInt 1) (IRConstInt 1),
                IRCMP_NEQ "c2" (IRConstInt 1) (IRConstInt 2),
                IRCMP_LT "c3" (IRConstInt 1) (IRConstInt 2),
                IRCMP_LTE "c4" (IRConstInt 1) (IRConstInt 1),
                IRCMP_GT "c5" (IRConstInt 2) (IRConstInt 1),
                IRCMP_GTE "c6" (IRConstInt 2) (IRConstInt 2)
              ]
            func = IRFunction "test" [] Nothing body
            varsMap = collectIRVars func
         in do
              Map.size varsMap @?= 6
              all (\k -> Map.lookup k varsMap == Just IRBool) ["c1", "c2", "c3", "c4", "c5", "c6"] @?= True
    ]
