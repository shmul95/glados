module AST.PrinterSpec (astPrinterTests) where

import Rune.AST.Nodes
import Rune.AST.Printer (prettyPrint)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- public
--

astPrinterTests :: TestTree
astPrinterTests =
  testGroup
    "AST Printer Tests"
    [ testCase "Primitives and Types" testPrimitivesAndTypes,
      testCase "Binary and Unary Operators" testOperators,
      testCase "Struct Definitions and Initialization" testStructs,
      testCase "Functions and Overrides" testFunctionsAndOverrides,
      testCase "Control Flow (If, Return, Loops)" testControlFlow,
      testCase "Variable Declarations and Assignments" testVariables
    ]

--
-- test cases
--

testPrimitivesAndTypes :: IO ()
testPrimitivesAndTypes = do
  let ast =
        Program
          "TypesTest"
          [ DefFunction
              "main"
              []
              TypeNull
              [ StmtVarDecl "v1" (Just TypeI8) (ExprLitInt 1),
                StmtVarDecl "v2" (Just TypeI16) (ExprLitInt 2),
                StmtVarDecl "v3" (Just TypeI32) (ExprLitInt 3),
                StmtVarDecl "v4" (Just TypeI64) (ExprLitInt 4),
                StmtVarDecl "v5" (Just TypeF32) (ExprLitFloat 1.0),
                StmtVarDecl "v6" (Just TypeF64) (ExprLitFloat 2.0),
                StmtVarDecl "v7" (Just TypeBool) (ExprLitBool True),
                StmtVarDecl "v8" (Just TypeU8) (ExprLitInt 8),
                StmtVarDecl "v9" (Just TypeU16) (ExprLitInt 16),
                StmtVarDecl "v10" (Just TypeU32) (ExprLitInt 32),
                StmtVarDecl "v11" (Just TypeString) (ExprLitString "hello"),
                StmtVarDecl "v12" (Just TypeAny) (ExprLitChar 'c'),
                StmtVarDecl "v13" (Just TypeNull) ExprLitNull,
                StmtVarDecl "v14" (Just (TypeCustom "User")) (ExprVar "u")
              ]
          ]

  let expected =
        unlines
          [ "Program: TypesTest",
            "  DefFunction main",
            "    Parameters:",
            "    ReturnType: null",
            "    Body:",
            "      StmtVarDecl v1 : i8",
            "      Value:",
            "        ExprLitInt 1",
            "      StmtVarDecl v2 : i16",
            "      Value:",
            "        ExprLitInt 2",
            "      StmtVarDecl v3 : i32",
            "      Value:",
            "        ExprLitInt 3",
            "      StmtVarDecl v4 : i64",
            "      Value:",
            "        ExprLitInt 4",
            "      StmtVarDecl v5 : f32",
            "      Value:",
            "        ExprLitFloat 1.0",
            "      StmtVarDecl v6 : f64",
            "      Value:",
            "        ExprLitFloat 2.0",
            "      StmtVarDecl v7 : bool",
            "      Value:",
            "        ExprLitBool True",
            "      StmtVarDecl v8 : u8",
            "      Value:",
            "        ExprLitInt 8",
            "      StmtVarDecl v9 : u16",
            "      Value:",
            "        ExprLitInt 16",
            "      StmtVarDecl v10 : u32",
            "      Value:",
            "        ExprLitInt 32",
            "      StmtVarDecl v11 : string",
            "      Value:",
            "        ExprLitString \"hello\"",
            "      StmtVarDecl v12 : any",
            "      Value:",
            "        ExprLitChar 'c'",
            "      StmtVarDecl v13 : null",
            "      Value:",
            "        ExprLitNull",
            "      StmtVarDecl v14 : User",
            "      Value:",
            "        ExprVar u"
          ]

  prettyPrint ast @?= init expected

testOperators :: IO ()
testOperators = do
  let ast =
        Program
          "OpTest"
          [ DefFunction
              "ops"
              []
              TypeAny
              [ StmtExpr (ExprBinary Add (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Sub (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Mul (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Div (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Mod (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Eq (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Neq (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Lt (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Lte (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Gt (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Gte (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary And (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprBinary Or (ExprVar "a") (ExprVar "b")),
                StmtExpr (ExprUnary Negate (ExprVar "x")),
                StmtExpr (ExprUnary PropagateError (ExprVar "x")),
                StmtExpr (ExprUnary PrefixInc (ExprVar "x")),
                StmtExpr (ExprUnary PrefixDec (ExprVar "x")),
                StmtExpr (ExprUnary PostfixInc (ExprVar "x")),
                StmtExpr (ExprUnary PostfixDec (ExprVar "x"))
              ]
          ]

  let expected =
        unlines
          [ "Program: OpTest",
            "  DefFunction ops",
            "    Parameters:",
            "    ReturnType: any",
            "    Body:",
            "      StmtExpr",
            "        ExprBinary +",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary -",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary *",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary /",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary %",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary ==",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary !=",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary <",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary <=",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary >",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary >=",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary &&",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprBinary ||",
            "          ExprVar a",
            "          ExprVar b",
            "      StmtExpr",
            "        ExprUnary -",
            "          ExprVar x",
            "      StmtExpr",
            "        ExprUnary ?",
            "          ExprVar x",
            "      StmtExpr",
            "        ExprUnary ++(prefix)",
            "          ExprVar x",
            "      StmtExpr",
            "        ExprUnary --(prefix)",
            "          ExprVar x",
            "      StmtExpr",
            "        ExprUnary (postfix)++",
            "          ExprVar x",
            "      StmtExpr",
            "        ExprUnary (postfix)--",
            "          ExprVar x"
          ]

  prettyPrint ast @?= init expected

testStructs :: IO ()
testStructs = do
  let ast =
        Program
          "StructTest"
          [ DefStruct
              "Vector"
              [Field "x" TypeF32, Field "y" TypeF32]
              [ DefFunction "len" [Parameter "self" TypeAny] TypeF32 []
              ],
            DefFunction
              "make"
              []
              TypeAny
              [ StmtExpr
                  ( ExprStructInit
                      "Vector"
                      [("x", ExprLitFloat 0.0), ("y", ExprLitFloat 1.0)]
                  ),
                StmtExpr (ExprAccess (ExprVar "v") "x"),
                StmtExpr (ExprCall "print" [ExprVar "v"])
              ]
          ]

  let expected =
        unlines
          [ "Program: StructTest",
            "  DefStruct Vector",
            "    Fields:",
            "      x: f32",
            "      y: f32",
            "    Methods:",
            "      DefFunction len",
            "        Parameters:",
            "          self: any",
            "        ReturnType: f32",
            "        Body:",
            "  DefFunction make",
            "    Parameters:",
            "    ReturnType: any",
            "    Body:",
            "      StmtExpr",
            "        ExprStructInit Vector",
            "        Fields:",
            "          x:",
            "          ",
            "            ExprLitFloat 0.0",
            "          y:",
            "          ",
            "            ExprLitFloat 1.0",
            "      StmtExpr",
            "        ExprAccess .x",
            "          ExprVar v",
            "      StmtExpr",
            "        ExprCall print",
            "        Arguments:",
            "          ExprVar v"
          ]

  prettyPrint ast @?= init expected

testFunctionsAndOverrides :: IO ()
testFunctionsAndOverrides = do
  let ast =
        Program
          "OverrideTest"
          [ DefOverride
              "toString"
              [Parameter "self" TypeAny]
              TypeString
              [StmtReturn (Just (ExprLitString "str"))]
          ]

  let expected =
        unlines
          [ "Program: OverrideTest",
            "  DefOverride toString",
            "    Parameters:",
            "      self: any",
            "    ReturnType: string",
            "    Body:",
            "      StmtReturn",
            "        ExprLitString \"str\""
          ]
  prettyPrint ast @?= init expected

testControlFlow :: IO ()
testControlFlow = do
  let ast =
        Program
          "FlowTest"
          [ DefFunction
              "flow"
              []
              TypeNull
              [ -- If with Else
                StmtIf
                  (ExprLitBool True)
                  [StmtExpr (ExprLitInt 1)]
                  (Just [StmtExpr (ExprLitInt 2)]),
                StmtIf
                  (ExprLitBool False)
                  []
                  Nothing,
                StmtLoop
                  [ StmtStop,
                    StmtNext
                  ],
                StmtFor "i" (Just TypeI32) (Just (ExprLitInt 0)) (ExprLitInt 10) [],
                StmtFor "j" Nothing Nothing (ExprLitInt 5) [],
                StmtForEach "x" (Just TypeString) (ExprVar "list") [],
                StmtForEach "y" Nothing (ExprVar "list2") [],
                StmtReturn Nothing
              ]
          ]

  let expected =
        unlines
          [ "Program: FlowTest",
            "  DefFunction flow",
            "    Parameters:",
            "    ReturnType: null",
            "    Body:",
            "      StmtIf",
            "        Condition:",
            "          ExprLitBool True",
            "        Then:",
            "          StmtExpr",
            "            ExprLitInt 1",
            "        Else:",
            "          StmtExpr",
            "            ExprLitInt 2",
            "      StmtIf",
            "        Condition:",
            "          ExprLitBool False",
            "        Then:",
            "      StmtLoop",
            "        Body:",
            "          StmtStop",
            "          StmtNext",
            "      StmtFor i : i32",
            "        Start:",
            "          ExprLitInt 0",
            "        End:",
            "          ExprLitInt 10",
            "        Body:",
            "      StmtFor j",
            "        Start: <Implicit>",
            "        End:",
            "          ExprLitInt 5",
            "        Body:",
            "      StmtForEach x : string",
            "        Iterable:",
            "          ExprVar list",
            "        Body:",
            "      StmtForEach y",
            "        Iterable:",
            "          ExprVar list2",
            "        Body:",
            "      StmtReturn"
          ]

  prettyPrint ast @?= init expected

testVariables :: IO ()
testVariables = do
  let ast =
        Program
          "VarTest"
          [ DefFunction
              "vars"
              []
              TypeNull
              [ StmtVarDecl "x" Nothing (ExprLitInt 10),
                StmtAssignment (ExprVar "x") (ExprLitInt 20)
              ]
          ]

  let expected =
        unlines
          [ "Program: VarTest",
            "  DefFunction vars",
            "    Parameters:",
            "    ReturnType: null",
            "    Body:",
            "      StmtVarDecl x",
            "      Value:",
            "        ExprLitInt 10",
            "      StmtAssignment",
            "        LValue:",
            "          ExprVar x",
            "        RValue:",
            "          ExprLitInt 20"
          ]

  prettyPrint ast @?= init expected
