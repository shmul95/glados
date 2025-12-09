module IR.IRNodesSpecs (irNodesTests) where

import Control.Monad.State (evalState)
import Data.Map (empty, insert)
import qualified Data.Set as Set
import Rune.IR.Nodes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- public
--

irNodesTests :: TestTree
irNodesTests =
  testGroup
    "Rune.IR.Nodes Specs"
    [ testIRTypes,
      testIRBinaryOp,
      testIRCompareOp,
      testIROperand,
      testIRLabel,
      testGenState,
      testIRInstruction,
      testIRFunction,
      testIRTopLevel,
      testIRProgram
    ]

--
-- helpers constants
--

op_const_int :: IROperand
op_const_int = IRConstInt 42

op_temp :: IROperand
op_temp = IRTemp "t1" IRI32

op_param :: IROperand
op_param = IRParam "p_x" IRI32

op_global :: IROperand
op_global = IRGlobal "GLOBAL_str" (IRPtr IRU8)

lbl :: IRLabel
lbl = IRLabel "L_start"

--
-- private
--

testIRTypes :: TestTree
testIRTypes =
  testGroup
    "IRType"
    [ testCase "All IRType constructors" $
        let types = [IRI32, IRI64, IRF32, IRF64, IRU8, IRPtr IRI32, IRStruct "Vec2f", IRNull]
            expected = [IRI32, IRI64, IRF32, IRF64, IRU8, IRPtr IRI32, IRStruct "Vec2f", IRNull]
         in types @?= expected,
      testCase "Deriving Show/Eq" $ show IRI32 @?= "IRI32"
    ]

testIRBinaryOp :: TestTree
testIRBinaryOp =
  testGroup
    "IRBinaryOp"
    [ testCase "All IRBinaryOp constructors" $
        let ops = [IRADD, IRSUB, IRMUL, IRDIV, IRMOD]
            expected = [IRADD, IRSUB, IRMUL, IRDIV, IRMOD]
         in ops @?= expected,
      testCase "Deriving Show/Eq" $ show IRADD @?= "IRADD"
    ]

testIRCompareOp :: TestTree
testIRCompareOp =
  testGroup
    "IRCompareOp"
    [ testCase "All IRCompareOp constructors" $
        let ops = [IREQ, IRNEQ, IRLT, IRLTE, IRGT, IRGTE]
            expected = [IREQ, IRNEQ, IRLT, IRLTE, IRGT, IRGTE]
         in ops @?= expected,
      testCase "Deriving Show/Eq" $ show IREQ @?= "IREQ"
    ]

testIROperand :: TestTree
testIROperand =
  testGroup
    "IROperand"
    [ testCase "All IROperand constructors" $
        let operands = [op_const_int, IRConstFloat 3.14, IRConstChar 'a', op_temp, op_param, op_global, IRConstNull]
            expected = [IRConstInt 42, IRConstFloat 3.14, IRConstChar 'a', IRTemp "t1" IRI32, IRParam "p_x" IRI32, IRGlobal "GLOBAL_str" $ IRPtr IRU8, IRConstNull]
         in operands @?= expected,
      testCase "Deriving Show/Eq" $ show op_temp @?= "IRTemp \"t1\" IRI32"
    ]

testIRLabel :: TestTree
testIRLabel =
  testCase "IRLabel and deriving" $
    do
      lbl @?= IRLabel "L_start"
      show lbl @?= "IRLabel \"L_start\""

testGenState :: TestTree
testGenState =
  testGroup
    "GenState and IRGen (Record Accessors)"
    [ testCase "Accessors" $
        let symTable = insert "x" (IRConstInt 5, IRI32) empty
            structTable = empty
            loopStack = []
            initialState =
              GenState
                { gsTempCounter = 5,
                  gsLabelCounter = 3,
                  gsStringCounter = 1,
                  gsGlobals = [IRGlobalString "s0" "str"],
                  gsCurrentFunc = Just "main",
                  gsSymTable = symTable,
                  gsStructs = structTable,
                  gsLoopStack = loopStack,
                  gsCalledFuncs = Set.empty,
                  gsStringMap = empty
                }
            dummyOp :: IRGen Int
            dummyOp = return 10
         in do
              gsTempCounter initialState @?= 5
              gsLabelCounter initialState @?= 3
              gsStringCounter initialState @?= 1
              gsGlobals initialState @?= [IRGlobalString "s0" "str"]
              gsCurrentFunc initialState @?= Just "main"
              gsSymTable initialState @?= symTable
              gsStructs initialState @?= structTable
              gsLoopStack initialState @?= loopStack
              evalState dummyOp initialState @?= 10,
      testCase "Deriving Show/Eq" $
        let state1 = GenState 0 0 0 [] Nothing empty empty [] Set.empty empty
            state2 = GenState 0 0 0 [] Nothing empty empty [] Set.empty empty
         in state1 @?= state2
    ]

testIRInstruction :: TestTree
testIRInstruction =
  testGroup
    "IRInstruction"
    [ -- memory operations
      testCase "IRALLOC" $ IRALLOC "x" IRI32 @?= IRALLOC "x" IRI32,
      testCase "IRSTORE" $ IRSTORE op_const_int op_temp @?= IRSTORE op_const_int op_temp,
      testCase "IRLOAD" $ IRLOAD "t2" op_param IRI32 @?= IRLOAD "t2" op_param IRI32,
      testCase "IRDEREF" $ IRDEREF "t3" op_global (IRPtr IRI32) @?= IRDEREF "t3" op_global (IRPtr IRI32),
      -- struct operations
      testCase "IRGET_FIELD" $ IRGET_FIELD "t1" op_temp "S" "f" IRI32 @?= IRGET_FIELD "t1" op_temp "S" "f" IRI32,
      testCase "IRSET_FIELD" $ IRSET_FIELD op_temp "S" "f" op_const_int @?= IRSET_FIELD op_temp "S" "f" op_const_int,
      -- arithmetic operations
      testCase "IRADD_OP" $ IRADD_OP "t4" op_temp op_const_int IRI32 @?= IRADD_OP "t4" op_temp op_const_int IRI32,
      testCase "IRSUB_OP" $ IRSUB_OP "t5" op_temp op_const_int IRI32 @?= IRSUB_OP "t5" op_temp op_const_int IRI32,
      testCase "IRMUL_OP" $ IRMUL_OP "t6" op_temp op_const_int IRI32 @?= IRMUL_OP "t6" op_temp op_const_int IRI32,
      testCase "IRDIV_OP" $ IRDIV_OP "t7" op_temp op_const_int IRI32 @?= IRDIV_OP "t7" op_temp op_const_int IRI32,
      testCase "IRMOD_OP" $ IRMOD_OP "t8" op_temp op_const_int IRI32 @?= IRMOD_OP "t8" op_temp op_const_int IRI32,
      -- comparison operations
      testCase "IRCMP_EQ" $ IRCMP_EQ "t9" op_temp op_const_int @?= IRCMP_EQ "t9" op_temp op_const_int,
      testCase "IRCMP_NEQ" $ IRCMP_NEQ "t10" op_temp op_const_int @?= IRCMP_NEQ "t10" op_temp op_const_int,
      testCase "IRCMP_LT" $ IRCMP_LT "t11" op_temp op_const_int @?= IRCMP_LT "t11" op_temp op_const_int,
      testCase "IRCMP_LTE" $ IRCMP_LTE "t12" op_temp op_const_int @?= IRCMP_LTE "t12" op_temp op_const_int,
      testCase "IRCMP_GT" $ IRCMP_GT "t13" op_temp op_const_int @?= IRCMP_GT "t13" op_temp op_const_int,
      testCase "IRCMP_GTE" $ IRCMP_GTE "t14" op_temp op_const_int @?= IRCMP_GTE "t14" op_temp op_const_int,
      -- control flow
      testCase "IRLABEL" $ IRLABEL lbl @?= IRLABEL lbl,
      testCase "IRJUMP" $ IRJUMP lbl @?= IRJUMP lbl,
      testCase "IRJUMP_TRUE" $ IRJUMP_TRUE op_temp lbl @?= IRJUMP_TRUE op_temp lbl,
      testCase "IRJUMP_FALSE" $ IRJUMP_FALSE op_temp lbl @?= IRJUMP_FALSE op_temp lbl,
      testCase "IRJUMP_EQ0" $ IRJUMP_EQ0 op_temp lbl @?= IRJUMP_EQ0 op_temp lbl,
      -- function operations
      testCase "IRCALL (with result)" $ IRCALL "t15" "foo" [op_temp, op_const_int] (Just IRI32) @?= IRCALL "t15" "foo" [op_temp, op_const_int] (Just IRI32),
      testCase "IRCALL (null)" $ IRCALL "" "bar" [op_temp] Nothing @?= IRCALL "" "bar" [op_temp] Nothing,
      testCase "IRRET (with value)" $ IRRET (Just op_const_int) @?= IRRET (Just op_const_int),
      testCase "IRRET (null)" $ IRRET Nothing @?= IRRET Nothing,
      -- utility operations
      testCase "IRADDR" $ IRADDR "t16" "globalVar" (IRPtr IRI32) @?= IRADDR "t16" "globalVar" (IRPtr IRI32),
      testCase "IRINC" $ IRINC op_temp @?= IRINC op_temp,
      testCase "IRDEC" $ IRDEC op_temp @?= IRDEC op_temp,
      testCase "IRASSIGN" $ IRASSIGN "t17" op_const_int IRI32 @?= IRASSIGN "t17" op_const_int IRI32,
      testCase "Deriving Show/Eq" $ show (IRALLOC "x" IRI32) @?= "IRALLOC \"x\" IRI32"
    ]

testIRFunction :: TestTree
testIRFunction =
  testGroup
    "IRFunction (Record Accessors)"
    [ testCase "Accessors" $
        let params = [("a", IRI32), ("b", IRI32)]
            body = [IRADD_OP "t1" op_param op_param IRI32, IRRET (Just op_temp)]
            func =
              IRFunction
                { irFuncName = "calculate",
                  irFuncParams = params,
                  irFuncRetType = Just IRI32,
                  irFuncBody = body
                }
         in do
              irFuncName func @?= "calculate"
              irFuncParams func @?= params
              irFuncRetType func @?= Just IRI32
              irFuncBody func @?= body,
      testCase "Deriving Show/Eq" $
        let func1 = IRFunction "f" [] Nothing []
            func2 = IRFunction "f" [] Nothing []
         in do
              show func1 @?= "IRFunction {irFuncName = \"f\", irFuncParams = [], irFuncRetType = Nothing, irFuncBody = []}"
              func1 @?= func2
    ]

testIRTopLevel :: TestTree
testIRTopLevel =
  testGroup
    "IRTopLevel"
    [ testCase "IRGlobalString" $ IRGlobalString "str0" "hello world" @?= IRGlobalString "str0" "hello world",
      testCase "IRFunctionDef" $
        let func =
              IRFunction
                { irFuncName = "main",
                  irFuncParams = [],
                  irFuncRetType = Just IRI32,
                  irFuncBody = [IRRET (Just op_const_int)]
                }
         in IRFunctionDef func @?= IRFunctionDef func,
      testCase "IRStructDef" $ IRStructDef "Vec2" [("x", IRF32)] @?= IRStructDef "Vec2" [("x", IRF32)],
      testCase "Deriving Show/Eq" $ show (IRGlobalString "s1" "") @?= "IRGlobalString \"s1\" \"\""
    ]

testIRProgram :: TestTree
testIRProgram =
  testGroup
    "IRProgram (Record Accessors)"
    [ testCase "Accessors" $
        let defs =
              [ IRGlobalString "msg" "hello",
                IRFunctionDef
                  IRFunction
                    { irFuncName = "entry",
                      irFuncParams = [],
                      irFuncRetType = Nothing,
                      irFuncBody = []
                    }
              ]
            program =
              IRProgram
                { irProgramName = "my_program",
                  irProgramDefs = defs
                }
         in do
              irProgramName program @?= "my_program"
              irProgramDefs program @?= defs,
      testCase "Deriving Show/Eq" $
        let prog1 = IRProgram "p" []
            prog2 = IRProgram "p" []
         in do
              show prog1 @?= "IRProgram {irProgramName = \"p\", irProgramDefs = []}"
              prog1 @?= prog2
    ]
