module Semantics.VarsSpecs (varsSemanticsTests) where

import Rune.AST.Nodes
import Rune.Semantics.Vars (verifVars, mangleFuncStack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool)
import Data.List (isInfixOf)
import qualified Data.HashMap.Strict as HM
import TestHelpers (dummyPos)

--
-- public
--

varsSemanticsTests :: TestTree
varsSemanticsTests =
  testGroup
    "Rune.Semantics.Vars"
    [ testGroup "Control Flow"
      [ expectOk "handles return without expression" returnNullProgram,
        expectOk "handles return with expression" returnWithExprProgram,
        expectOk "handles if-else branches" ifElseFullProgram,
        expectOk "handles if without else" ifOnlyProgram,
        expectOk "handles loop statement" loopProgram,
        expectOk "handles stop in loop" stopProgram,
        expectOk "handles next in loop" nextProgram
      ],

    testGroup "For Loops"
      [ expectOk "handles for loop with explicit start" forWithStartProgram,
        expectOk "handles for loop without start type annotation" forNoStartProgram,
        expectOk "handles for loop with deduced type from start" forDeducedTypeProgram
      ],

      testGroup "ForEach Loops"
      [ expectOk "handles foreach over string" forEachStringProgram,
        expectOk "handles foreach over array" forEachArrayProgram,
        expectOk "handles foreach over any type" forEachAnyProgram,
        expectOk "accepts invalid iterable in foreach (defaults to Any)" forEachInvalidProgram
      ],

      testGroup "Assignments"
      [ expectOk "handles assignment to simple variable" assignmentSimpleProgram,
        expectOk "handles assignment to struct fields" structAssignmentProgram,
        expectErr "detects incompatible struct field assignment" assignmentIncompatibleProgram "expression of type i32"
      ],

      testGroup "Variable Declarations"
      [ expectOk "handles var decl with explicit type" varDeclWithTypeProgram,
        expectOk "handles complex struct initialization" complexStructInitProgram
      ],

      testGroup "Expression Statements"
      [ expectOk "handles expression statement" exprStmtProgram,
        expectOk "handles function call as expression" callExprProgram
      ],

    testGroup "Expressions"
      [ testGroup "Binary Operations"
        [ expectOk "handles valid binary operation" binaryValidProgram,
          expectErr "detects invalid binary operation (type mismatch)" binaryInvalidProgram "binary operation type mismatch",
          expectErr "detects undefined variable on left" binaryUndefinedProgram "Undefined variable",
          expectErr "detects undefined variable on right" binaryUndefinedRightProgram "Undefined variable"
        ],

        testGroup "Unary Operations"
        [ expectOk "handles unary not operation" unaryNotProgram,
          expectOk "handles unary negate operation" unaryNegateProgram
        ],

        testGroup "Array Operations"
        [ expectOk "handles array indexing" arrayIndexProgram,
          expectOk "handles array indexing with variable" arrayIndexVarProgram,
          expectOk "handles array literal" arrayLiteralProgram
        ],

        testGroup "Struct Operations"
        [ expectOk "handles struct field access" structAccessProgram,
          expectOk "handles struct initialization" structInitProgram
        ],

        testGroup "Type Casts"
        [ expectOk "handles cast expressions" castProgram
        ],

        testGroup "Variable References"
        [ expectOk "handles simple variable reference" varRefProgram,
          expectErr "detects undefined variable in return" undefinedVarProgram "Undefined variable"
        ]
      ],

    testGroup "Method Verification"
      [ expectErr "rejects method with wrong first parameter" methodNoSelfProgram "First parameter of method",
        expectErr "rejects method with wrong first parameter name" methodWrongFirstParamProgram "First parameter of method"
      ],

    testGroup "Method Calls via ExprAccess"
      [ expectOk "handles method call on struct variable" methodCallProgram,
        expectOk "handles method call with arguments" methodCallWithArgsProgram,
        expectOk "handles method call with return value" methodCallReturnProgram,
        expectErr "detects undefined method on struct" methodCallUndefinedProgram "undefined"
      ],

    testGroup "Method Body Verification"
      [ expectOk "verifies method with self parameter and body" methodWithBodyProgram,
        expectErr "rejects method without any parameters" methodNoParamsProgram "Method must have at least one parameter",
        expectErr "rejects method with wrong first parameter" methodNoSelfProgram "First parameter of method"
      ],

    testGroup "Generic Functions"
      [ expectOk "instantiates generic from argument" genericInstArgProgram,
        expectOk "instantiates generic from return context" genericInstContextProgram,
        expectOk "uses instantiation cache" genericCacheProgram,
        expectErr "fails generic without context or args" genericFailProgram "cannot be instantiated",
        expectErr "fails generic with no arguments" genericNoArgsProgram "cannot be instantiated"
      ],

    testGroup "Utilities"
      [ testCase "mangleFuncStack handles overloads" testMangleFuncStack
      ]
    ]

--
-- Helper functions
--

expectOk :: String -> Program -> TestTree
expectOk label program = testCase label $
  case verifVars program of
    Right _ -> return ()
    Left err -> assertFailure $ "Expected success, but got error: " ++ err

expectErr :: String -> Program -> String -> TestTree
expectErr label program match =
  testCase label $ case verifVars program of
    Left msg | match `isInfixOf` msg -> return ()
    result -> assertFailure $ "Expected error matching '" ++ match ++ "', got: " ++ show result

--
-- Control Flow Programs
--

returnNullProgram :: Program
returnNullProgram = Program "ret" 
  [DefFunction "f" [] TypeNull [StmtReturn dummyPos Nothing] False]

returnWithExprProgram :: Program
returnWithExprProgram = Program "ret-expr"
  [DefFunction "f" [] TypeI32 [StmtReturn dummyPos (Just (ExprLitInt dummyPos 42))] False]

ifElseFullProgram :: Program
ifElseFullProgram = Program "if"
  [DefFunction "f" [] TypeNull
    [StmtIf dummyPos (ExprLitBool dummyPos True) [] (Just [StmtNext dummyPos])]
    False]

ifOnlyProgram :: Program
ifOnlyProgram = Program "if-only"
  [DefFunction "f" [] TypeNull
    [StmtIf dummyPos (ExprLitBool dummyPos True) [] Nothing]
    False]

loopProgram :: Program
loopProgram = Program "loop"
  [DefFunction "f" [] TypeNull
    [StmtLoop dummyPos [StmtStop dummyPos]]
    False]

stopProgram :: Program
stopProgram = Program "stop"
  [DefFunction "f" [] TypeNull
    [StmtLoop dummyPos [StmtStop dummyPos]]
    False]

nextProgram :: Program
nextProgram = Program "next"
  [DefFunction "f" [] TypeNull
    [StmtLoop dummyPos [StmtNext dummyPos]]
    False]

--
-- For Loop Programs
--

forWithStartProgram :: Program
forWithStartProgram = Program "for"
  [DefFunction "f" [] TypeNull
    [StmtFor dummyPos "i" (Just TypeI32) (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 10) []]
    False]

forNoStartProgram :: Program
forNoStartProgram = Program "for-no-start"
  [DefFunction "f" [] TypeNull
    [StmtFor dummyPos "i" (Just TypeI32) Nothing (ExprLitInt dummyPos 10) []]
    False]

forDeducedTypeProgram :: Program
forDeducedTypeProgram = Program "for-deduced"
  [DefFunction "f" [] TypeNull
    [StmtFor dummyPos "i" Nothing (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 10) []]
    False]

--
-- ForEach Loop Programs
--

forEachStringProgram :: Program
forEachStringProgram = Program "fe"
  [DefFunction "f" [] TypeNull
    [StmtForEach dummyPos "c" (Just TypeChar) (ExprLitString dummyPos "hi") []]
    False]

forEachArrayProgram :: Program
forEachArrayProgram = Program "fe-arr"
  [DefFunction "f" [Parameter "a" (TypeArray TypeI32)] TypeNull
    [StmtForEach dummyPos "x" Nothing (ExprVar dummyPos "a") []]
    False]

forEachAnyProgram :: Program
forEachAnyProgram = Program "feany"
  [DefFunction "f" [Parameter "a" TypeAny] TypeNull
    [StmtForEach dummyPos "x" Nothing (ExprVar dummyPos "a") []]
    False]

forEachInvalidProgram :: Program
forEachInvalidProgram = Program "feerr"
  [DefFunction "f" [] TypeNull
    [StmtForEach dummyPos "i" Nothing (ExprLitInt dummyPos 1) []]
    False]

--
-- Assignment Programs
--

assignmentSimpleProgram :: Program
assignmentSimpleProgram = Program "assign-simple"
  [DefFunction "f" [] TypeNull
    [StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 10),
     StmtAssignment dummyPos (ExprVar dummyPos "x") (ExprLitInt dummyPos 42)]
    False]

structAssignmentProgram :: Program
structAssignmentProgram = Program "assign"
  [DefStruct "S" [Field "x" TypeI32] [],
   DefFunction "f" [Parameter "s" (TypeCustom "S")] TypeNull
    [StmtAssignment dummyPos (ExprAccess dummyPos (ExprVar dummyPos "s") "x") (ExprLitInt dummyPos 42)]
    False]

assignmentIncompatibleProgram :: Program
assignmentIncompatibleProgram = Program "assignerr"
  [DefStruct "S" [Field "x" TypeI32] [],
   DefFunction "f" [Parameter "s" (TypeCustom "S")] TypeNull
    [StmtAssignment dummyPos (ExprAccess dummyPos (ExprVar dummyPos "s") "x") (ExprLitString dummyPos "bad")]
    False]

--
-- Variable Declaration Programs
--

varDeclWithTypeProgram :: Program
varDeclWithTypeProgram = Program "var-type"
  [DefFunction "f" [] TypeNull
    [StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 42)]
    False]

complexStructInitProgram :: Program
complexStructInitProgram = Program "sinit"
  [DefStruct "S" [Field "x" TypeI32, Field "y" TypeI32] [],
   DefFunction "f" [] TypeNull
    [StmtVarDecl dummyPos "s" Nothing (ExprStructInit dummyPos "S" [("x", ExprLitInt dummyPos 1), ("y", ExprLitInt dummyPos 2)])]
    False]

--
-- Expression Statement Programs
--

exprStmtProgram :: Program
exprStmtProgram = Program "expr-stmt"
  [DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprLitInt dummyPos 42)]
    False]

callExprProgram :: Program
callExprProgram = Program "call-expr"
  [DefFunction "g" [] TypeNull [] False,
   DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprCall dummyPos (ExprVar dummyPos "g") [])]
    False]

--
-- Binary Operation Programs
--

binaryValidProgram :: Program
binaryValidProgram = Program "bin-valid"
  [DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 2))]
    False]

binaryInvalidProgram :: Program
binaryInvalidProgram = Program "binerr"
  [DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitString dummyPos "a"))]
    False]

binaryUndefinedProgram :: Program
binaryUndefinedProgram = Program "binundef"
  [DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprBinary dummyPos Add (ExprVar dummyPos "ghost") (ExprLitInt dummyPos 1))]
    False]

binaryUndefinedRightProgram :: Program
binaryUndefinedRightProgram = Program "binundef-right"
  [DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprVar dummyPos "ghost"))]
    False]

--
-- Unary Operation Programs
--

unaryNotProgram :: Program
unaryNotProgram = Program "unary-not"
  [DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprUnary dummyPos Not (ExprLitBool dummyPos True))]
    False]

unaryNegateProgram :: Program
unaryNegateProgram = Program "unary-negate"
  [DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprUnary dummyPos Negate (ExprLitInt dummyPos 42))]
    False]

--
-- Array Operation Programs
--

arrayIndexProgram :: Program
arrayIndexProgram = Program "idx"
  [DefFunction "f" [Parameter "a" (TypeArray TypeI32)] TypeI32
    [StmtReturn dummyPos (Just (ExprIndex dummyPos (ExprVar dummyPos "a") (ExprLitInt dummyPos 0)))]
    False]

arrayIndexVarProgram :: Program
arrayIndexVarProgram = Program "aidx"
  [DefFunction "f" [Parameter "a" (TypeArray TypeI32), Parameter "i" TypeI32] TypeI32
    [StmtReturn dummyPos (Just (ExprIndex dummyPos (ExprVar dummyPos "a") (ExprVar dummyPos "i")))]
    False]

arrayLiteralProgram :: Program
arrayLiteralProgram = Program "arrlit"
  [DefFunction "f" [] (TypeArray TypeI32)
    [StmtReturn dummyPos (Just (ExprLitArray dummyPos [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2]))]
    False]

--
-- Struct Operation Programs
--

structAccessProgram :: Program
structAccessProgram = Program "struct-access"
  [DefStruct "S" [Field "x" TypeI32] [],
   DefFunction "f" [Parameter "s" (TypeCustom "S")] TypeI32
    [StmtReturn dummyPos (Just (ExprAccess dummyPos (ExprVar dummyPos "s") "x"))]
    False]

structInitProgram :: Program
structInitProgram = Program "struct-init"
  [DefStruct "S" [Field "x" TypeI32] [],
   DefFunction "f" [] TypeNull
    [StmtVarDecl dummyPos "s" Nothing (ExprStructInit dummyPos "S" [("x", ExprLitInt dummyPos 42)])]
    False]

--
-- Type Cast Programs
--

castProgram :: Program
castProgram = Program "cast"
  [DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprCast dummyPos (ExprLitInt dummyPos 42) TypeF32)]
    False]

--
-- Variable Reference Programs
--

varRefProgram :: Program
varRefProgram = Program "var-ref"
  [DefFunction "f" [] TypeI32
    [StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 42),
     StmtReturn dummyPos (Just (ExprVar dummyPos "x"))]
    False]

undefinedVarProgram :: Program
undefinedVarProgram = Program "undef-var"
  [DefFunction "f" [] TypeI32
    [StmtReturn dummyPos (Just (ExprVar dummyPos "notDefined"))]
    False]

--
-- Method Programs
--

methodNoSelfProgram :: Program
methodNoSelfProgram = Program "method-wrong-first"
  [DefStruct "S" []
    [DefFunction "f" [Parameter "x" TypeI32] TypeNull [] False]]

methodWrongFirstParamProgram :: Program
methodWrongFirstParamProgram = Program "method-wrong-first"
  [DefStruct "S" []
    [DefFunction "f" [Parameter "notself" TypeI32] TypeNull [] False]]

--
-- Method Call Programs (via ExprAccess)
--

methodCallProgram :: Program
methodCallProgram = Program "method-call"
  [DefStruct "S" []
    [DefFunction "f" [Parameter "self" TypeAny] TypeNull [] False],
   DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "s" Nothing (ExprStructInit dummyPos "S" []),
     StmtExpr dummyPos (ExprCall dummyPos (ExprAccess dummyPos (ExprVar dummyPos "s") "f") [])]
    False]

methodCallWithArgsProgram :: Program
methodCallWithArgsProgram = Program "method-call-args"
  [DefStruct "S" []
    [DefFunction "set" [Parameter "self" TypeAny, Parameter "val" TypeI32] TypeNull [] False],
   DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "s" Nothing (ExprStructInit dummyPos "S" []),
     StmtExpr dummyPos (ExprCall dummyPos (ExprAccess dummyPos (ExprVar dummyPos "s") "set") [ExprLitInt dummyPos 42])]
    False]

methodCallReturnProgram :: Program
methodCallReturnProgram = Program "method-call-ret"
  [DefStruct "S" [Field "x" TypeI32]
    [DefFunction "get" [Parameter "self" TypeAny] TypeI32 [StmtReturn dummyPos (Just (ExprAccess dummyPos (ExprVar dummyPos "self") "x"))] False],
   DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "s" Nothing (ExprStructInit dummyPos "S" [("x", ExprLitInt dummyPos 10)]),
     StmtVarDecl dummyPos "val" Nothing (ExprCall dummyPos (ExprAccess dummyPos (ExprVar dummyPos "s") "get") [])]
    False]

methodCallUndefinedProgram :: Program
methodCallUndefinedProgram = Program "method-undef"
  [DefStruct "S" [] [],
   DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "s" Nothing (ExprStructInit dummyPos "S" []),
     StmtExpr dummyPos (ExprCall dummyPos (ExprAccess dummyPos (ExprVar dummyPos "s") "nonexistent") [])]
    False]

--
-- Method Body Verification Programs
--

methodWithBodyProgram :: Program
methodWithBodyProgram = Program "method-body"
  [DefStruct "S" [Field "x" TypeI32]
    [DefFunction "inc" [Parameter "self" TypeAny] TypeNull
      [StmtVarDecl dummyPos "y" (Just TypeI32) (ExprLitInt dummyPos 1)]
      False]]

methodNoParamsProgram :: Program
methodNoParamsProgram = Program "method-no-params"
  [DefStruct "S" []
    [DefFunction "f" [] TypeNull [] False]]

--
-- Generic Function Programs
--

genericInstArgProgram :: Program
genericInstArgProgram = Program "gen"
  [DefFunction "id" [Parameter "x" TypeAny] TypeAny [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))] False,
   DefFunction "m" [] TypeNull [StmtExpr dummyPos (ExprCall dummyPos (ExprVar dummyPos "id") [ExprLitInt dummyPos 42])] False]

genericInstContextProgram :: Program
genericInstContextProgram = Program "genctx"
  [DefFunction "get" [] TypeAny [StmtReturn dummyPos (Just (ExprLitInt dummyPos 1))] False,
   DefFunction "m" [] TypeNull [StmtVarDecl dummyPos "x" (Just TypeI32) (ExprCall dummyPos (ExprVar dummyPos "get") [])] False]

genericCacheProgram :: Program
genericCacheProgram = Program "cache"
  [DefFunction "id" [Parameter "x" TypeAny] TypeAny [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))] False,
   DefFunction "m" [] TypeNull
    [StmtExpr dummyPos (ExprCall dummyPos (ExprVar dummyPos "id") [ExprLitInt dummyPos 1]),
     StmtExpr dummyPos (ExprCall dummyPos (ExprVar dummyPos "id") [ExprLitInt dummyPos 2])]
    False]

genericFailProgram :: Program
genericFailProgram = Program "genfail"
  [DefFunction "fail" [] TypeAny [StmtReturn dummyPos (Just (ExprLitNull dummyPos))] False,
   DefFunction "m" [] TypeNull [StmtExpr dummyPos (ExprCall dummyPos (ExprVar dummyPos "fail") [])] False]

genericNoArgsProgram :: Program
genericNoArgsProgram = Program "gen-no-args"
  [DefFunction "id" [Parameter "x" TypeAny] TypeAny [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))] False,
   DefFunction "m" [] TypeNull [StmtExpr dummyPos (ExprCall dummyPos (ExprVar dummyPos "id") [])] False]

--
-- Utility Test
--

testMangleFuncStack :: IO ()
testMangleFuncStack = do
  let fs = HM.fromList [("i32_f_i32", (TypeI32, [TypeI32])), ("f32_f_f32", (TypeF32, [TypeF32]))]
      mangled = mangleFuncStack fs
  assertBool "Should contain mangled names" (HM.member "i32_f_i32" mangled)
  assertBool "Should contain other mangled name" (HM.member "f32_f_f32" mangled)
