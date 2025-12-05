module IR.IRPrinterSpec (irPrinterTests) where

import Data.List (intercalate, isInfixOf)
import Rune.IR.Nodes
import Rune.IR.Printer (prettyPrintIR)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- public
--

irPrinterTests :: TestTree
irPrinterTests =
  testGroup
    "Rune.IR.Printer Specs"
    [ testCase "IRProgram printout" testPrintProgram,
      testGroup
        "IRTopLevel printout"
        [ testCase "IRExtern" testPrintTopLevelExtern,
          testCase "IRGlobalString" testPrintTopLevelGlobalString,
          testCase "IRFunctionDef" testPrintTopLevelFunctionDef,
          testCase "IRStructDef" testPrintTopLevelStructDef
        ],
      testGroup
        "IRInstruction printout"
        [ testCase "IRASSIGN" testPrintInstructionAssign,
          testCase "Binary Operations" testPrintInstructionBinOps,
          testCase "Compare Operations" testPrintInstructionCmpOps,
          testCase "Logical Operations" testPrintInstructionLogOps,
          testCase "Control Flow" testPrintInstructionControlFlow,
          testCase "Function Calls" testPrintInstructionCall,
          testCase "Returns" testPrintInstructionRet,
          testCase "Memory/Pointers" testPrintInstructionMemPtr,
          testCase "Struct/Field Access" testPrintInstructionStruct
        ],
      testGroup
        "IRType printout"
        [ testCase "All primitive types" testPrintAllIRTypes
        ]
    ]

--
-- private
--

opTemp :: IROperand
opTemp = IRTemp "t1" IRI32

opParam :: IROperand
opParam = IRParam "p_a" IRI32

opGlobal :: IROperand
opGlobal = IRGlobal "g_val" IRI32

opConstInt :: IROperand
opConstInt = IRConstInt 123

opConstFloat :: IROperand
opConstFloat = IRConstFloat 3.14

opConstChar :: IROperand
opConstChar = IRConstChar 'x'

opConstNull :: IROperand
opConstNull = IRConstNull

irTypePtrChar :: IRType
irTypePtrChar = IRPtr IRChar

testPrintProgram :: IO ()
testPrintProgram =
  let program = IRProgram "my_module" [IRExtern "puts"]
      expected = "PROGRAM my_module:\nEXTERN puts\n"
   in prettyPrintIR program @?= expected

testPrintTopLevelExtern :: IO ()
testPrintTopLevelExtern =
  let extern = IRExtern "malloc"
      program = IRProgram "test" [extern]
      expected = "PROGRAM test:\nEXTERN malloc\n"
   in prettyPrintIR program @?= expected

testPrintTopLevelGlobalString :: IO ()
testPrintTopLevelGlobalString =
  let global = IRGlobalString "str0" "hello"
      program = IRProgram "test" [global]
      expected = "PROGRAM test:\nGLOBAL str0: string = \"hello\\0\"\n"
   in prettyPrintIR program @?= expected

testPrintTopLevelStructDef :: IO ()
testPrintTopLevelStructDef =
  let struct = IRStructDef "Vec2f" [("x", IRF32), ("y", IRF32)]
      program = IRProgram "test" [struct]
      expected = "PROGRAM test:\nSTRUCT Vec2f { x: f32, y: f32 }\n"
   in prettyPrintIR program @?= expected

testPrintTopLevelFunctionDef :: IO ()
testPrintTopLevelFunctionDef =
  let func =
        IRFunction
          { irFuncName = "main",
            irFuncParams = [("p_a", IRI32)],
            irFuncRetType = Just IRI32,
            irFuncBody =
              [ IRASSIGN "res" opConstInt IRI32,
                IRRET (Just opTemp)
              ]
          }
      program = IRProgram "test" [IRFunctionDef func]
      expected =
        intercalate
          "\n"
          [ "PROGRAM test:",
            "DEF main(p_a: i32):",
            "    res: i32 = 123",
            "    RET t1",
            ""
          ]
   in prettyPrintIR program @?= expected

testPrintInstructionAssign :: IO ()
testPrintInstructionAssign =
  let instr = IRASSIGN "a" opTemp IRI32
      instr2 = IRASSIGN "n" opConstNull IRNull
      func = IRFunction "f" [] Nothing [instr, instr2]
      expected = "PROGRAM p:\nDEF f():\n    a: i32 = t1\n    n: null = (null)\n"
   in prettyPrintIR (IRProgram "p" [IRFunctionDef func]) @?= expected

testPrintInstructionBinOps :: IO ()
testPrintInstructionBinOps =
  let instrs =
        [ IRADD_OP "r1" opConstInt opTemp IRI32,
          IRSUB_OP "r2" opParam opGlobal IRI32,
          IRMUL_OP "r3" opTemp opTemp IRI32,
          IRDIV_OP "r4" opGlobal opConstInt IRF32,
          IRMOD_OP "r5" opConstInt opConstInt IRI32
        ]
      func = IRFunction "f" [] Nothing instrs
      expected =
        intercalate
          "\n"
          [ "PROGRAM p:",
            "DEF f():",
            "    r1: i32 = ADD 123, t1",
            "    r2: i32 = SUB p_a, g_val",
            "    r3: i32 = MUL t1, t1",
            "    r4: f32 = DIV g_val, 123",
            "    r5: i32 = MOD 123, 123",
            ""
          ]
   in prettyPrintIR (IRProgram "p" [IRFunctionDef func]) @?= expected

testPrintInstructionCmpOps :: IO ()
testPrintInstructionCmpOps =
  let instrs =
        [ IRCMP_EQ "c1" opTemp opConstInt,
          IRCMP_NEQ "c2" opParam opGlobal,
          IRCMP_LT "c3" opConstFloat opConstFloat,
          IRCMP_LTE "c4" opConstInt opTemp,
          IRCMP_GT "c5" opTemp opParam,
          IRCMP_GTE "c6" opGlobal opGlobal
        ]
      func = IRFunction "f" [] Nothing instrs
      expected =
        intercalate
          "\n"
          [ "PROGRAM p:",
            "DEF f():",
            "    c1 = CMP_EQ t1, 123",
            "    c2 = CMP_NEQ p_a, g_val",
            "    c3 = CMP_LT 3.14, 3.14",
            "    c4 = CMP_LTE 123, t1",
            "    c5 = CMP_GT t1, p_a",
            "    c6 = CMP_GTE g_val, g_val",
            ""
          ]
   in prettyPrintIR (IRProgram "p" [IRFunctionDef func]) @?= expected

testPrintInstructionLogOps :: IO ()
testPrintInstructionLogOps =
  let instrs =
        [ IRAND_OP "r1" opConstInt opTemp IRI32,
          IROR_OP "r2" opParam opGlobal IRI32
        ]
      func = IRFunction "f" [] Nothing instrs
      expected =
        intercalate
          "\n"
          [ "PROGRAM p:",
            "DEF f():",
            "    r1: i32 = AND 123, t1",
            "    r2: i32 = OR p_a, g_val",
            ""
          ]
   in prettyPrintIR (IRProgram "p" [IRFunctionDef func]) @?= expected

testPrintInstructionControlFlow :: IO ()
testPrintInstructionControlFlow =
  let lbl = IRLabel ".L.target"
      instrs =
        [ IRLABEL lbl,
          IRJUMP lbl,
          IRJUMP_TRUE opTemp lbl,
          IRJUMP_FALSE opConstInt lbl,
          IRJUMP_EQ0 opParam lbl
        ]
      func = IRFunction "f" [] Nothing instrs
      expected =
        intercalate
          "\n"
          [ "PROGRAM p:",
            "DEF f():",
            ".L.target:",
            "    JUMP .L.target",
            "    JUMP_TRUE t1, .L.target",
            "    JUMP_FALSE 123, .L.target",
            "    JUMP_EQ0 p_a, .L.target",
            ""
          ]
   in prettyPrintIR (IRProgram "p" [IRFunctionDef func]) @?= expected

testPrintInstructionCall :: IO ()
testPrintInstructionCall =
  let call1 = IRCALL "r" "funcA" [opTemp, opConstInt] (Just IRI64)
      call2 = IRCALL "r2" "funcB" [opParam] Nothing
      call3 = IRCALL "" "funcC" [opGlobal, opConstChar] Nothing
      instrs = [call1, call2, call3]
      func = IRFunction "f" [] Nothing instrs
      expected =
        intercalate
          "\n"
          [ "PROGRAM p:",
            "DEF f():",
            "    r: i64 = CALL funcA(t1, 123)",
            "    r2: i32 = CALL funcB(p_a)",
            "    CALL funcC(g_val, 'x')",
            ""
          ]
   in prettyPrintIR (IRProgram "p" [IRFunctionDef func]) @?= expected

testPrintInstructionRet :: IO ()
testPrintInstructionRet =
  let instrs =
        [ IRRET (Just opTemp),
          IRRET Nothing
        ]
      func = IRFunction "f" [] Nothing instrs
      expected =
        intercalate
          "\n"
          [ "PROGRAM p:",
            "DEF f():",
            "    RET t1",
            "    RET",
            ""
          ]
   in prettyPrintIR (IRProgram "p" [IRFunctionDef func]) @?= expected

testPrintInstructionMemPtr :: IO ()
testPrintInstructionMemPtr =
  let instrs =
        [ IRADDR "p1" "var_name" irTypePtrChar,
          IRDEREF "val" opTemp IRI32,
          IRINC opParam,
          IRDEC opGlobal,
          IRSTORE opTemp opConstInt,
          IRLOAD "loaded" opTemp IRF32,
          IRALLOC "arr" (IRPtr IRI32)
        ]
      func = IRFunction "f" [] Nothing instrs
      expected =
        intercalate
          "\n"
          [ "PROGRAM p:",
            "DEF f():",
            "    p1: *char = ADDR var_name",
            "    val: i32 = DEREF t1",
            "    INC p_a",
            "    DEC g_val",
            "    STORE 123 -> t1",
            "    loaded: f32 = LOAD t1",
            "    ALLOC arr: *i32",
            ""
          ]
   in prettyPrintIR (IRProgram "p" [IRFunctionDef func]) @?= expected

testPrintInstructionStruct :: IO ()
testPrintInstructionStruct =
  let structPtr = IRPtr (IRStruct "Vec2f")
      opStruct = IRTemp "v_ptr" structPtr
      instrs =
        [ IRGET_FIELD "x_val" opStruct "Vec2f" "x" IRF32,
          IRSET_FIELD opStruct "Vec2f" "y" opConstFloat
        ]
      func = IRFunction "f" [] Nothing instrs
      expected =
        intercalate
          "\n"
          [ "PROGRAM p:",
            "DEF f():",
            "    x_val: f32 = GET_FIELD v_ptr, \"Vec2f\", \"x\"",
            "    SET_FIELD v_ptr, \"Vec2f\", \"y\", 3.14",
            ""
          ]
   in prettyPrintIR (IRProgram "p" [IRFunctionDef func]) @?= expected

testPrintAllIRTypes :: IO ()
testPrintAllIRTypes = do
  let types =
        [ IRI8,
          IRI16,
          IRI32,
          IRI64,
          IRF32,
          IRF64,
          IRU8,
          IRChar,
          IRU16,
          IRU32,
          IRU64,
          IRBool,
          IRNull,
          IRPtr IRI32,
          IRStruct "Vec2f"
        ]
      expected =
        [ "i8",
          "i16",
          "i32",
          "i64",
          "f32",
          "f64",
          "u8",
          "char",
          "u16",
          "u32",
          "u64",
          "bool",
          "null",
          "*i32",
          "Vec2f"
        ]
      operands = map (IRTemp "t") types
      instrs = zipWith (IRASSIGN "r") operands types
      func = IRFunction "f" [] Nothing instrs
      output = prettyPrintIR (IRProgram "p" [IRFunctionDef func])
      allTypesFound = all (\tStr -> any (tStr `isInfixOf`) (lines output)) expected
   in allTypesFound @?= True
