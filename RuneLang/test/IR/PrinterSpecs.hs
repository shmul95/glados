{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.PrinterSpecs (irPrinterTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Rune.IR.Printer
import Rune.IR.Nodes
import Data.List (intercalate)

--
-- public
--

irPrinterTests :: TestTree
irPrinterTests = testGroup "rune.ir.printer specs"
  [ testPrintType
  , testPrintOperand
  , testPrintInstruction
  , testPrintFunction
  , testPrintTopLevel
  , testPrintProgram
  ]

--
-- private
--

testPrintType :: TestTree
testPrintType = testGroup "printtype"
  [ testCase "i8" $ printType IRI8 @?= "i8"
  , testCase "i16" $ printType IRI16 @?= "i16"
  , testCase "i32" $ printType IRI32 @?= "i32"
  , testCase "i64" $ printType IRI64 @?= "i64"
  , testCase "f32" $ printType IRF32 @?= "f32"
  , testCase "f64" $ printType IRF64 @?= "f64"
  , testCase "u8" $ printType IRU8 @?= "u8"
  , testCase "char" $ printType IRChar @?= "char"
  , testCase "u16" $ printType IRU16 @?= "u16"
  , testCase "u32" $ printType IRU32 @?= "u32"
  , testCase "u64" $ printType IRU64 @?= "u64"
  , testCase "bool" $ printType IRBool @?= "bool"
  , testCase "ptr" $ printType (IRPtr IRI32) @?= "*i32"
  , testCase "struct" $ printType (IRStruct "MyStruct") @?= "MyStruct"
  , testCase "null" $ printType IRNull @?= "null"
  ]

testPrintOperand :: TestTree
testPrintOperand = testGroup "printoperand"
  [ testCase "constint" $ printOperand (IRConstInt 42) @?= "42"
  , testCase "constfloat" $ printOperand (IRConstFloat 1.2) @?= "1.2"
  , testCase "constchar" $ printOperand (IRConstChar 'a') @?= "'a'"
  , testCase "constbool true" $ printOperand (IRConstBool True) @?= "(true)"
  , testCase "constbool false" $ printOperand (IRConstBool False) @?= "(false)"
  , testCase "constnull" $ printOperand IRConstNull @?= "(null)"
  , testCase "temp" $ printOperand (IRTemp "tmp1" IRI32) @?= "tmp1"
  , testCase "param" $ printOperand (IRParam "p_x" IRF64) @?= "p_x"
  , testCase "global" $ printOperand (IRGlobal "g_str" (IRPtr IRChar)) @?= "g_str"
  ]

testPrintInstruction :: TestTree
testPrintInstruction = testGroup "printinstruction"
  [ 
    let op_i32_t1  = IRTemp "t1" IRI32
        op_i32_c10 = IRConstInt 10
        op_f64_f1  = IRTemp "f1" IRF64
        op_f64_c314= IRConstFloat 3.14
        lbl_true   = IRLabel "L_true"
    in testGroup "instructions"
    [ testCase "irassign" $ printInstruction (IRASSIGN "dest" op_i32_c10 IRI32) @?= "dest: i32 = 10"
    
    , testCase "iradd_op" $ printInstruction (IRADD_OP "d" op_i32_t1 op_i32_c10 IRI32) @?= "d: i32 = ADD t1, 10"
    , testCase "irsub_op" $ printInstruction (IRSUB_OP "d" op_i32_t1 op_i32_c10 IRI32) @?= "d: i32 = SUB t1, 10"
    , testCase "irmul_op" $ printInstruction (IRMUL_OP "d" op_i32_t1 op_i32_c10 IRI32) @?= "d: i32 = MUL t1, 10"
    , testCase "irdiv_op" $ printInstruction (IRDIV_OP "d" op_i32_t1 op_i32_c10 IRI32) @?= "d: i32 = DIV t1, 10"
    , testCase "irmod_op" $ printInstruction (IRMOD_OP "d" op_i32_t1 op_i32_c10 IRI32) @?= "d: i32 = MOD t1, 10"

    , testCase "ircmp_eq" $ printInstruction (IRCMP_EQ "d" op_f64_f1 op_f64_c314) @?= "d = CMP_EQ f1, 3.14"
    , testCase "ircmp_neq" $ printInstruction (IRCMP_NEQ "d" op_f64_f1 op_f64_c314) @?= "d = CMP_NEQ f1, 3.14"
    , testCase "ircmp_lt" $ printInstruction (IRCMP_LT "d" op_f64_f1 op_f64_c314) @?= "d = CMP_LT f1, 3.14"
    , testCase "ircmp_lte" $ printInstruction (IRCMP_LTE "d" op_f64_f1 op_f64_c314) @?= "d = CMP_LTE f1, 3.14"
    , testCase "ircmp_gt" $ printInstruction (IRCMP_GT "d" op_f64_f1 op_f64_c314) @?= "d = CMP_GT f1, 3.14"
    , testCase "ircmp_gte" $ printInstruction (IRCMP_GTE "d" op_f64_f1 op_f64_c314) @?= "d = CMP_GTE f1, 3.14"

    , testCase "irand_op" $ printInstruction (IRAND_OP "d" op_i32_t1 op_i32_c10 IRI32) @?= "d: i32 = AND t1, 10"
    , testCase "iror_op" $ printInstruction (IROR_OP "d" op_i32_t1 op_i32_c10 IRI32) @?= "d: i32 = OR t1, 10"

    , testCase "irlabel" $ printInstruction (IRLABEL lbl_true) @?= "L_true:"
    , testCase "irjump" $ printInstruction (IRJUMP lbl_true) @?= "JUMP L_true"
    , testCase "irjump_true" $ printInstruction (IRJUMP_TRUE op_i32_t1 lbl_true) @?= "JUMP_TRUE t1, L_true"
    , testCase "irjump_false" $ printInstruction (IRJUMP_FALSE op_i32_t1 lbl_true) @?= "JUMP_FALSE t1, L_true"
    , testCase "irjump_eq0" $ printInstruction (IRJUMP_EQ0 op_i32_t1 lbl_true) @?= "JUMP_EQ0 t1, L_true"
    
    , testCase "ircall with dest and type" $ printInstruction (IRCALL "res" "foo" [op_i32_t1, op_i32_c10] (Just IRI32)) @?= "res: i32 = CALL foo(t1, 10)"
    , testCase "ircall with dest and no type (fallback)" $ printInstruction (IRCALL "res" "foo" [op_i32_t1] Nothing) @?= "res: i32 = CALL foo(t1)"
    , testCase "ircall without dest" $ printInstruction (IRCALL "" "bar" [op_f64_f1] Nothing) @?= "CALL bar(f1)"
    , testCase "irret with operand" $ printInstruction (IRRET (Just op_i32_t1)) @?= "RET t1"
    , testCase "irret without operand" $ printInstruction (IRRET Nothing) @?= "RET"

    , testCase "iraddr" $ printInstruction (IRADDR "ptr" "x" (IRPtr IRChar)) @?= "ptr: *char = ADDR x"
    , testCase "irderef" $ printInstruction (IRDEREF "val" op_i32_t1 IRI32) @?= "val: i32 = DEREF t1"
    , testCase "irstore" $ printInstruction (IRSTORE op_i32_t1 op_i32_c10) @?= "STORE 10 -> t1"
    , testCase "irload" $ printInstruction (IRLOAD "val" op_i32_t1 IRI32) @?= "val: i32 = LOAD t1"
    , testCase "iralloc" $ printInstruction (IRALLOC "x" IRI32) @?= "ALLOC x: i32"
    
    , testCase "irget_field" $ printInstruction (IRGET_FIELD "ptr" op_i32_t1 "MyStruct" "fieldx" IRI32) @?= "ptr: i32 = GET_FIELD t1, \"MyStruct\", \"fieldx\""
    , testCase "irset_field" $ printInstruction (IRSET_FIELD op_i32_t1 "MyStruct" "fieldx" op_i32_c10) @?= "SET_FIELD t1, \"MyStruct\", \"fieldx\", 10"

    , testCase "irinc" $ printInstruction (IRINC op_i32_t1) @?= "INC t1"
    , testCase "irdec" $ printInstruction (IRDEC op_i32_t1) @?= "DEC t1"
    ]
  ]

testPrintFunction :: TestTree
testPrintFunction = testGroup "printfunction"
  [ testCase "standard function with params and body (testing indent)" $
      let params = [("p1", IRI32), ("p2", IRF32)]
          body = 
            [ IRLABEL (IRLabel "start")
            , IRALLOC "x" IRI32
            , IRADD_OP "t0" (IRTemp "x" IRI32) (IRParam "p1" IRI32) IRI32
            , IRJUMP (IRLabel "end")
            , IRLABEL (IRLabel "end")
            , IRRET (Just (IRTemp "t0" IRI32))
            ]
          func = IRFunction "main" params (Just IRI32) body
          expected = intercalate "\n"
            [ "DEF main(p1: i32, p2: f32):"
            , "start:"
            , "    ALLOC x: i32"
            , "    t0: i32 = ADD x, p1"
            , "    JUMP end"
            , "end:"
            , "    RET t0"
            ]
      in printFunction func @?= expected
  , testCase "function with no instructions" $
      let func = IRFunction "empty_func" [] Nothing []
          expected = "DEF empty_func():"
      in printFunction func @?= expected
  ]

testPrintTopLevel :: TestTree
testPrintTopLevel = testGroup "printtoplevel"
  [ testCase "irextern" $
      printTopLevel (IRExtern "puts") @?= "EXTERN puts"
  , testCase "irglobalstring (with escaped chars)" $
      printTopLevel (IRGlobalString "g_msg" "hello\nworld\"") @?= "GLOBAL g_msg: string = \"hello\\nworld\\\"\\0\""
  , testCase "irstructdef" $
      let fields = [("x", IRI32), ("y", IRF32)]
      in printTopLevel (IRStructDef "point" fields) @?= "STRUCT point { x: i32, y: f32 }"
  , testCase "irfunctiondef" $
      let func = IRFunction "main" [("p", IRI32)] (Just IRI32) [IRRET Nothing]
          expected = intercalate "\n"
            [ "DEF main(p: i32):"
            , "    RET"
            ]
      in printTopLevel (IRFunctionDef func) @?= expected
  ]

testPrintProgram :: TestTree
testPrintProgram = testGroup "printprogram/prettyprintir"
  [ testCase "simple program" $
      let extern = IRExtern "printf"
          global = IRGlobalString "g_str" "test"
          func = IRFunction "main" [] (Just IRI32) [IRRET Nothing]
          program = IRProgram "TestModule" [extern, global, IRFunctionDef func]
          expected = intercalate "\n"
            [ "PROGRAM TestModule:"
            , "EXTERN printf"
            , "GLOBAL g_str: string = \"test\\0\""
            , "DEF main():"
            , "    RET"
            , ""
            ]
      in do
        printProgram program @?= expected
        prettyPrintIR program @?= expected
  ]
