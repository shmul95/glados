module IR.IRHelpersSpec (irHelpersTests) where

import Control.Monad.State (evalState, runState)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Rune.AST.Nodes (Type (..))
import Rune.IR.IRHelpers
import Rune.IR.Nodes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

irHelpersTests :: TestTree
irHelpersTests =
  testGroup
    "Rune.IR.IRHelpers Specs"
    [ testAstTypeConversion,
      testSizeOfIRType,
      testRegisterVar,
      testRegisterCall,
      testNewTemp,
      testNextLabelIndex,
      testMakeLabel,
      testNewStringGlobal,
      testGenFormatString,
      testEndsWithRet,
      testLoopContext,
      testMangleMethodName
    ]

initialState :: GenState
initialState =
  GenState
    { gsTempCounter = 0,
      gsLabelCounter = 0,
      gsStringCounter = 0,
      gsGlobals = [],
      gsCurrentFunc = Nothing,
      gsSymTable = Map.empty,
      gsStructs = Map.empty,
      gsLoopStack = [],
      gsCalledFuncs = Set.empty,
      gsStringMap = Map.empty
    }

testAstTypeConversion :: TestTree
testAstTypeConversion =
  testGroup
    "astTypeToIRType"
    [ testCase "TypeI8" $ astTypeToIRType TypeI8 @?= IRI8,
      testCase "TypeI16" $ astTypeToIRType TypeI16 @?= IRI16,
      testCase "TypeI32" $ astTypeToIRType TypeI32 @?= IRI32,
      testCase "TypeI64" $ astTypeToIRType TypeI64 @?= IRI64,
      testCase "TypeU8" $ astTypeToIRType TypeU8 @?= IRU8,
      testCase "TypeU16" $ astTypeToIRType TypeU16 @?= IRU16,
      testCase "TypeU32" $ astTypeToIRType TypeU32 @?= IRU32,
      testCase "TypeU64" $ astTypeToIRType TypeU64 @?= IRU64,
      testCase "TypeChar" $ astTypeToIRType TypeChar @?= IRChar,
      testCase "TypeF32" $ astTypeToIRType TypeF32 @?= IRF32,
      testCase "TypeF64" $ astTypeToIRType TypeF64 @?= IRF64,
      testCase "TypeBool" $ astTypeToIRType TypeBool @?= IRBool,
      testCase "TypeNull" $ astTypeToIRType TypeNull @?= IRNull,
      testCase "TypeString" $ astTypeToIRType TypeString @?= IRPtr IRChar,
      testCase "TypeCustom" $ astTypeToIRType (TypeCustom "MyStruct") @?= IRStruct "MyStruct"
    ]

testSizeOfIRType :: TestTree
testSizeOfIRType =
  testGroup
    "sizeOfIRType"
    [ testCase "IRI8" $ sizeOfIRType IRI8 @?= 1,
      testCase "IRI16" $ sizeOfIRType IRI16 @?= 2,
      testCase "IRI32" $ sizeOfIRType IRI32 @?= 4,
      testCase "IRI64" $ sizeOfIRType IRI64 @?= 8,
      testCase "IRF32" $ sizeOfIRType IRF32 @?= 4,
      testCase "IRF64" $ sizeOfIRType IRF64 @?= 8,
      testCase "IRU8" $ sizeOfIRType IRU8 @?= 1,
      testCase "IRChar" $ sizeOfIRType IRChar @?= 1,
      testCase "IRU16" $ sizeOfIRType IRU16 @?= 2,
      testCase "IRU32" $ sizeOfIRType IRU32 @?= 4,
      testCase "IRU64" $ sizeOfIRType IRU64 @?= 8,
      testCase "IRBool" $ sizeOfIRType IRBool @?= 1,
      testCase "IRPtr" $ sizeOfIRType (IRPtr IRI32) @?= 8,
      testCase "IRStruct" $ sizeOfIRType (IRStruct "Vec") @?= 8,
      testCase "IRNull" $ sizeOfIRType IRNull @?= 8
    ]

testRegisterVar :: TestTree
testRegisterVar =
  testCase "registerVar adds variable to symbol table" $
    let action = registerVar "x" (IRConstInt 10) IRI32
        (_, finalState) = runState action initialState
        symTable = gsSymTable finalState
     in Map.lookup "x" symTable @?= Just (IRConstInt 10, IRI32)

testRegisterCall :: TestTree
testRegisterCall =
  testCase "registerCall adds function to called set" $
    let action = registerCall "printf"
        (_, finalState) = runState action initialState
        calledFuncs = gsCalledFuncs finalState
     in Set.member "printf" calledFuncs @?= True

testNewTemp :: TestTree
testNewTemp =
  testGroup
    "newTemp"
    [ testCase "newTemp generates temp with counter" $
        let (temp, finalState) = runState (newTemp "t" IRI32) initialState
         in do
              temp @?= "t0"
              gsTempCounter finalState @?= 1,
      testCase "newTemp increments counter" $
        let action = do
              t1 <- newTemp "t" IRI32
              t2 <- newTemp "t" IRI32
              return (t1, t2)
            ((temp1, temp2), finalState) = runState action initialState
         in do
              temp1 @?= "t0"
              temp2 @?= "t1"
              gsTempCounter finalState @?= 2
    ]

testNextLabelIndex :: TestTree
testNextLabelIndex =
  testCase "nextLabelIndex increments label counter" $
    let action = do
          idx1 <- nextLabelIndex
          idx2 <- nextLabelIndex
          return (idx1, idx2)
        ((i1, i2), finalState) = runState action initialState
     in do
          i1 @?= 0
          i2 @?= 1
          gsLabelCounter finalState @?= 2

testMakeLabel :: TestTree
testMakeLabel =
  testCase "makeLabel creates proper label" $
    do
      makeLabel "loop" 0 @?= IRLabel ".L.loop0"
      makeLabel "end" 5 @?= IRLabel ".L.end5"

testNewStringGlobal :: TestTree
testNewStringGlobal =
  testGroup
    "newStringGlobal"
    [ testCase "newStringGlobal creates global string" $
        let (name, finalState) = runState (newStringGlobal "hello") initialState
         in do
              name @?= "str_global0"
              gsStringCounter finalState @?= 1
              length (gsGlobals finalState) @?= 1,
      testCase "newStringGlobal with function context" $
        let state = initialState {gsCurrentFunc = Just "main"}
            (name, finalState) = runState (newStringGlobal "world") state
         in do
              name @?= "str_main0"
              gsStringCounter finalState @?= 1
    ]

testGenFormatString :: TestTree
testGenFormatString =
  testCase "genFormatString generates format string without temporary" $
    let (instrs, op) = evalState (genFormatString "%d") initialState
     in do
          length instrs @?= 0
          case op of
            IRGlobal name _ -> name @?= "str_global0"
            _ -> error "Expected IRGlobal operand"

testEndsWithRet :: TestTree
testEndsWithRet =
  testGroup
    "endsWithRet"
    [ testCase "empty list" $ endsWithRet [] @?= False,
      testCase "ends with IRRET" $ endsWithRet [IRALLOC "x" IRI32, IRRET Nothing] @?= True,
      testCase "ends with IRJUMP" $ endsWithRet [IRALLOC "x" IRI32, IRJUMP (IRLabel "L1")] @?= True,
      testCase "ends with other instruction" $ endsWithRet [IRALLOC "x" IRI32] @?= False
    ]

testLoopContext :: TestTree
testLoopContext =
  testGroup
    "Loop context management"
    [ testCase "getCurrentLoop on empty stack" $
        let result = evalState getCurrentLoop initialState
         in result @?= Nothing,
      testCase "pushLoopContext and getCurrentLoop" $
        let action = do
              pushLoopContext (IRLabel "L_header") (IRLabel "L_end")
              getCurrentLoop
            result = evalState action initialState
         in result @?= Just (IRLabel "L_header", IRLabel "L_end"),
      testCase "push and pop loop context" $
        let action = do
              pushLoopContext (IRLabel "L1") (IRLabel "L2")
              pushLoopContext (IRLabel "L3") (IRLabel "L4")
              popLoopContext
              getCurrentLoop
            result = evalState action initialState
         in result @?= Just (IRLabel "L1", IRLabel "L2")
    ]

testMangleMethodName :: TestTree
testMangleMethodName =
  testCase "mangleMethodName combines struct and method" $
    do
      mangleMethodName "Vec2" "length" @?= "Vec2_length"
      mangleMethodName "Point" "distance" @?= "Point_distance"
