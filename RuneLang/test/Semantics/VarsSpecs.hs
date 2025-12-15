module Semantics.VarsSpecs (varsSemanticsTests) where

import Rune.AST.Nodes
import Rune.Semantics.Vars (verifVars)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)
import Data.List (isInfixOf)
import TestHelpers (dummyPos)

--
-- public
--

varsSemanticsTests :: TestTree
varsSemanticsTests =
  testGroup
    "Rune.Semantics.Vars"
    [ expectOk "accepts parameters and local declarations" validProgram,
      expectErr "rejects undefined reference" invalidProgram "missing",
      expectErr "scopes nested block declarations" blockLeakProgram "inner",
      expectOk "validates struct methods recursively" structProgram,
      expectErr "override catches undefined variable" overrideLeakProgram "ghost",
      expectOk "override allows parameter usage" overrideValidProgram,
      expectErr "for loop variable does not leak" forLeakProgram "i",
      expectErr "for-each loop variable does not leak" forEachLeakProgram "item",
      expectErr "return expression is validated" returnLeakProgram "retval",
      expectErr "else branch expressions are validated" ifElseElseProgram "elseVar",
      expectOk "struct initialization and access use scoped values" structInitProgram,
      expectOk "binary and unary expressions validate operands" expressionProgram,
      expectErr "call argument must exist" callArgsErrorProgram "arg",
      expectErr "struct init fields must exist" structInitErrorProgram "fieldVal",
      expectErr "access target must exist" accessErrorProgram "target",
      expectTypeOverwrite "detects assignment type overwrite error" assignmentTypeOverwriteProgram,
      expectErr "binary operands validated" binaryErrorProgram "lhs",
      expectErr "unary operand validated" unaryErrorProgram "value",
      expectTypeOverwrite "detects type overwrite error" typeOverwriteProgram,
      expectMultipleType "detects multiple type error" multipleTypeProgram,
      expectOk "handles loop statements" loopProgram,
      expectOk "handles stop/next statements" stopNextProgram,
      expectOk "handles non-variable assignments" nonVarAssignmentProgram,
      expectOk "handles for loop without start" forWithoutStartProgram,
      expectErr "validates for loop end expression" forEndErrorProgram "endVar",
      expectErr "validates if condition" ifConditionErrorProgram "condVar",
      expectOk "handles literal expressions" literalExpressionsProgram,
      expectOk "handles struct definitions" structDefProgram,
      expectOk "handles function calls with known functions" knownFunctionCallProgram,
      expectErr "validates assignment right-hand side" assignmentRHSErrorProgram "rhsVar",
      
      expectFunctionNotFound "detects unknown function calls" unknownFunctionProgram "thisDoesNotExist",
      expectFunctionNotFound "detects wrong argument type (fallback to not found)" wrongTypeProgram "helper",
      expectFunctionNotFound "detects too few arguments (fallback to not found)" tooFewArgsProgram "helper",
      expectFunctionNotFound "detects too many arguments (fallback to not found)" tooManyArgsProgram "helper",
      expectFunctionNotFound "detects multiple wrong types (fallback to not found)" multipleWrongTypesProgram "helper",
      expectFunctionNotFound "validates nested function call arguments (fallback to not found)" nestedFunctionCallProgram "outer",
      
      expectOk "validates correct function call arguments" correctArgsProgram,
      expectOk "handles empty parameter list" emptyParamsProgram
      -- TODO: Re-enable when expectWrongType and missing programs are implemented
      -- expectWrongType "validates nested function call arguments" nestedFunctionCallProgram,
      -- New tests for source position coverage
      -- testGroup "Source Position Error Reporting"
      --   [ expectErrWithPos "reports line and column for undefined variable" errorAtLine5Col10 5 10
      --   , expectErrWithPos "reports line and column for type mismatch" typeErrorAtLine3Col5 3 5
      --   , expectErrWithPos "reports line and column for undefined function" undefinedFuncAtLine7Col15 7 15
      --   , expectErrWithPos "reports correct position in nested expression" nestedErrorAtLine10Col20 10 20
      --   , expectErrWithContext "includes context in error message" contextProgram "variable reference" "global context"
      --   , expectErrWithContext "includes nested context" nestedContextProgram "variable assignment" "global context"
      --   ]
      -- expectFuncStackContent "mangleFuncStack does not mangle single functions" noOverloadProgram ["foo"],
      -- expectFuncStackContent "mangleFuncStack mangles overloaded functions" overloadProgram ["null_bar_i32", "null_bar_f32"],
      -- expectOk "verifScope handles StmtReturn Nothing" returnNothingProgram,
      -- expectOk "verifScope handles empty block" emptyBlockProgram,

      -- expectOk "Instantiates generic function correctly" genericValidProgram,
      -- expectOk "Instantiates generic function twice (caching)" genericDoubleCallProgram,
      -- expectGenericInferenceError "Fails to infer generic return type without context/args" genericInferenceFailProgram
    ]

--
-- private
--

expectOk :: String -> Program -> TestTree
expectOk label program = testCase label $
  case verifVars program of
    Right _ -> return ()
    Left err -> assertFailure $ "Expected success, but got error: " ++ err

expectErr :: String -> Program -> String -> TestTree
expectErr label program missingVar =
  testCase label $ case verifVars program of
    Left msg | ("Expected: Undefined variable '" ++ missingVar ++ "'") `isInfixOf` msg -> return ()
            | ("Expected: function '" ++ missingVar ++ "' to exist") `isInfixOf` msg -> return ()
    result -> assertFailure $ "Expected UndefinedVar error for " ++ missingVar ++ ", got: " ++ show result

expectTypeOverwrite :: String -> Program -> TestTree
expectTypeOverwrite label program =
  testCase label $ case verifVars program of
    Left msg | "Expected: variable" `isInfixOf` msg && "to have type" `isInfixOf` msg -> return ()
    result -> assertFailure $ "Expected TypeOverwrite error, got: " ++ show result

expectMultipleType :: String -> Program -> TestTree
expectMultipleType label program =
  testCase label $ case verifVars program of
    Left msg | "to have type" `isInfixOf` msg && "being assigned" `isInfixOf` msg -> return ()
    result -> assertFailure $ "Expected MultipleType error, got: " ++ show result

expectFunctionNotFound :: String -> Program -> String -> TestTree
expectFunctionNotFound label program fname =
  testCase label $ case verifVars program of
    Left msg | ("Expected: function '" ++ fname ++ "' to exist") `isInfixOf` msg -> return ()
             | ("Function " ++ fname ++ " not found") `isInfixOf` msg -> return ()
             | ("Expected: argument" `isInfixOf` msg && "to have type" `isInfixOf` msg) -> return ()
             | ("Expected:" `isInfixOf` msg && "arguments" `isInfixOf` msg) -> return ()
    result -> assertFailure $ "Expected UnknownFunction error for " ++ fname ++ ", got: " ++ show result


validProgram :: Program
validProgram =
  Program
    "valid"
    [ DefFunction
        "bar"
        [Parameter "x" TypeI32]
        TypeNull
        [StmtReturn dummyPos (Just (ExprLitNull dummyPos))],
      DefFunction
        "foo"
        [Parameter "arg" TypeI32]
        TypeI32
        [ StmtVarDecl dummyPos "local" (Just TypeI32) (ExprVar dummyPos "arg"),
          StmtExpr dummyPos (ExprCall dummyPos "bar" [ExprVar dummyPos "local"]),
          StmtFor dummyPos "i" (Just TypeI32) (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 1) [StmtExpr dummyPos (ExprVar dummyPos "i")],
          StmtForEach dummyPos "item" (Just TypeI32) (ExprVar dummyPos "local") [StmtExpr dummyPos (ExprVar dummyPos "item")],
          StmtReturn dummyPos (Just (ExprLitNull dummyPos))
        ]
    ]

invalidProgram :: Program
invalidProgram =
  Program
    "invalid"
    [ DefFunction
        "foo"
        []
        TypeI32
        [StmtExpr dummyPos (ExprVar dummyPos "missing")]
    ]
blockLeakProgram :: Program
blockLeakProgram =
  Program
    "block-leak"
    [ DefFunction
        "foo"
        []
        TypeI32
        [ StmtIf dummyPos
            (ExprLitBool dummyPos True)
            [StmtVarDecl dummyPos "inner" Nothing (ExprLitInt dummyPos 1)]
            Nothing,
          StmtExpr dummyPos (ExprVar dummyPos "inner")
        ]
    ]
structProgram :: Program
structProgram =
  Program
    "struct"
    [ DefStruct
        "Vec"
        []
        [ DefFunction
            "len"
            [Parameter "self" TypeAny]
            TypeI32
            [StmtExpr dummyPos (ExprVar dummyPos "self")]
        ]
    ]
overrideLeakProgram :: Program
overrideLeakProgram =
  Program
    "override-leak"
    [ DefOverride
        "show"
        []
        TypeNull
        [StmtExpr dummyPos (ExprVar dummyPos "ghost")]
    ]

overrideValidProgram :: Program
overrideValidProgram =
  Program
    "function-valid"
    [ DefFunction
        "print"
        [Parameter "value" TypeI32]
        TypeI32
        [StmtReturn dummyPos (Just (ExprVar dummyPos "value"))]
    ]
forLeakProgram :: Program
forLeakProgram =
  Program
    "for-leak"
    [ DefFunction
        "loop"
        []
        TypeNull
        [ StmtFor dummyPos "i" Nothing (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 1) [StmtExpr dummyPos (ExprVar dummyPos "i")],
          StmtExpr dummyPos (ExprVar dummyPos "i")
        ]
    ]
forEachLeakProgram :: Program
forEachLeakProgram =
  Program
    "foreach-leak"
    [ DefFunction
        "loop"
        []
        TypeNull
        [ StmtVarDecl dummyPos "source" Nothing (ExprLitInt dummyPos 0),
          StmtForEach dummyPos "item" Nothing (ExprVar dummyPos "source") [StmtExpr dummyPos (ExprVar dummyPos "item")],
          StmtExpr dummyPos (ExprVar dummyPos "item")
        ]
    ]
returnLeakProgram :: Program
returnLeakProgram =
  Program
    "return-leak"
    [ DefFunction
        "returns"
        []
        TypeNull
        [StmtReturn dummyPos (Just (ExprVar dummyPos "retval"))]
    ]
ifElseElseProgram :: Program
ifElseElseProgram =
  Program
    "if-else"
    [ DefFunction
        "branch"
        []
        TypeNull
        [ StmtIf dummyPos
            (ExprLitBool dummyPos False)
            []
            (Just [StmtExpr dummyPos (ExprVar dummyPos "elseVar")])
        ]
    ]
structInitProgram :: Program
structInitProgram =
  Program
    "struct-init"
    [ DefFunction
        "build"
        [Parameter "seed" TypeI32]
        TypeNull
        [ StmtVarDecl dummyPos "point" (Just (TypeCustom "Point")) (ExprStructInit dummyPos "Point" [("x", ExprVar dummyPos "seed")]),
          StmtExpr dummyPos (ExprAccess dummyPos (ExprVar dummyPos "point") "x")
        ]
    ]
expressionProgram :: Program
expressionProgram =
  Program
    "expr"
    [ DefFunction
        "ops"
        [Parameter "x" TypeI32]
        TypeNull
        [ StmtExpr dummyPos (ExprBinary dummyPos Add (ExprVar dummyPos "x") (ExprUnary dummyPos Negate (ExprVar dummyPos "x")))
        ]
    ]
callArgsErrorProgram :: Program
callArgsErrorProgram =
  Program
    "call-args"
    [ DefFunction "foo" [Parameter "x" TypeAny] TypeNull [StmtReturn dummyPos Nothing],
      DefFunction
        "caller"
        []
        TypeNull
        [StmtExpr dummyPos (ExprCall dummyPos "foo" [ExprVar dummyPos "arg"])]
    ]
structInitErrorProgram :: Program
structInitErrorProgram =
  Program
    "struct-init-err"
    [ DefFunction
        "build"
        []
        TypeNull
        [StmtVarDecl dummyPos "point" Nothing (ExprStructInit dummyPos "Point" [("x", ExprVar dummyPos "fieldVal")])]
    ]
accessErrorProgram :: Program
accessErrorProgram =
  Program
    "access-err"
    [ DefFunction
        "access"
        []
        TypeNull
        [StmtExpr dummyPos (ExprAccess dummyPos (ExprVar dummyPos "target") "field")]
    ]
assignmentTypeOverwriteProgram :: Program
assignmentTypeOverwriteProgram =
  Program
    "assign-type-overwrite"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 1),
          StmtAssignment dummyPos (ExprVar dummyPos "x") (ExprLitFloat dummyPos 1.0)
        ]
    ]
binaryErrorProgram :: Program
binaryErrorProgram =
  Program
    "binary-err"
    [ DefFunction
        "bin"
        []
        TypeNull
        [StmtExpr dummyPos (ExprBinary dummyPos Add (ExprVar dummyPos "lhs") (ExprLitInt dummyPos 0))]
    ]
unaryErrorProgram :: Program
unaryErrorProgram =
  Program
    "unary-err"
    [ DefFunction
        "neg"
        []
        TypeNull
        [StmtExpr dummyPos (ExprUnary dummyPos Negate (ExprVar dummyPos "value"))]
    ]

typeOverwriteProgram :: Program
typeOverwriteProgram =
  Program
    "type-overwrite"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 42),
          StmtVarDecl dummyPos "x" (Just TypeF32) (ExprLitFloat dummyPos 3.14)
        ]
    ]
multipleTypeProgram :: Program
multipleTypeProgram =
  Program
    "multiple-type"
    [ DefFunction
        "test"
        []
        TypeNull
        [StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitFloat dummyPos 3.14)]
    ]
loopProgram :: Program
loopProgram =
  Program
    "loop"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 1),
          StmtLoop dummyPos [StmtExpr dummyPos (ExprVar dummyPos "x")]
        ]
    ]
stopNextProgram :: Program
stopNextProgram =
  Program
    "stop-next"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtLoop dummyPos [StmtStop dummyPos, StmtNext dummyPos]
        ]
    ]
nonVarAssignmentProgram :: Program
nonVarAssignmentProgram =
  Program
    "non-var-assign"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtVarDecl dummyPos "point" (Just (TypeCustom "Point")) (ExprStructInit dummyPos "Point" [("x", ExprLitInt dummyPos 1)]),
          StmtAssignment dummyPos (ExprAccess dummyPos (ExprVar dummyPos "point") "x") (ExprLitInt dummyPos 2)
        ]
    ]
forWithoutStartProgram :: Program
forWithoutStartProgram =
  Program
    "for-no-start"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtFor dummyPos "i" (Just TypeI32) Nothing (ExprLitInt dummyPos 10) [StmtExpr dummyPos (ExprVar dummyPos "i")]
        ]
    ]
forEndErrorProgram :: Program
forEndErrorProgram =
  Program
    "for-end-error"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtFor dummyPos "i" Nothing (Just (ExprLitInt dummyPos 0)) (ExprVar dummyPos "endVar") [StmtExpr dummyPos (ExprVar dummyPos "i")]
        ]
    ]
ifConditionErrorProgram :: Program
ifConditionErrorProgram =
  Program
    "if-cond-error"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtIf dummyPos (ExprVar dummyPos "condVar") [] Nothing
        ]
    ]
literalExpressionsProgram :: Program
literalExpressionsProgram =
  Program
    "literals"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtVarDecl dummyPos "i" (Just TypeI32) (ExprLitInt dummyPos 42),
          StmtVarDecl dummyPos "f" (Just TypeF32) (ExprLitFloat dummyPos 3.14),
          StmtVarDecl dummyPos "s" (Just TypeString) (ExprLitString dummyPos "hello"),
          StmtVarDecl dummyPos "c" (Just TypeU8) (ExprLitChar dummyPos 'a'),
          StmtVarDecl dummyPos "b" (Just TypeBool) (ExprLitBool dummyPos True),
          StmtVarDecl dummyPos "n" (Just TypeNull) (ExprLitNull dummyPos)
        ]
    ]
structDefProgram :: Program
structDefProgram = Program "struct-def" [DefStruct "Point" [] []]

knownFunctionCallProgram :: Program
knownFunctionCallProgram =
  Program
    "known-func"
    [ DefFunction "helper" [] TypeI32 [StmtReturn dummyPos (Just (ExprLitInt dummyPos 1))],
      DefFunction "main" [] TypeNull [StmtExpr dummyPos (ExprCall dummyPos "helper" [])],
      DefFunction "test_show" [] TypeNull [StmtExpr dummyPos (ExprCall dummyPos "show" [ExprLitString dummyPos "test"])]
    ]

assignmentRHSErrorProgram :: Program
assignmentRHSErrorProgram =
  Program
    "assign-rhs-error"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtVarDecl dummyPos "x" Nothing (ExprLitInt dummyPos 1),
          StmtAssignment dummyPos (ExprVar dummyPos "x") (ExprVar dummyPos "rhsVar")
        ]
    ]

unknownFunctionProgram :: Program
unknownFunctionProgram =
  Program
    "unknown-func"
    [ DefFunction
        "main"
        []
        TypeNull
        [StmtExpr dummyPos (ExprCall dummyPos "thisDoesNotExist" [])]
    ]
wrongTypeProgram :: Program
wrongTypeProgram =
  Program
    "wrong-type"
    [ DefFunction "helper" [Parameter "x" TypeI32] TypeI32 [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr dummyPos (ExprCall dummyPos "helper" [ExprLitString dummyPos "wrong"])]
    ]
tooFewArgsProgram :: Program
tooFewArgsProgram =
  Program
    "too-few-args"
    [ DefFunction "helper" [Parameter "x" TypeI32, Parameter "y" TypeI32] TypeI32 [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr dummyPos (ExprCall dummyPos "helper" [ExprLitInt dummyPos 1])]
    ]
tooManyArgsProgram :: Program
tooManyArgsProgram =
  Program
    "too-many-args"
    [ DefFunction "helper" [Parameter "x" TypeI32] TypeI32 [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr dummyPos (ExprCall dummyPos "helper" [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2])]
    ]
correctArgsProgram :: Program
correctArgsProgram =
  Program
    "correct-args"
    [ DefFunction "helper" [Parameter "x" TypeI32, Parameter "y" TypeF32, Parameter "z" TypeString] TypeI32 [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr dummyPos (ExprCall dummyPos "helper" [ExprLitInt dummyPos 1, ExprLitFloat dummyPos 2.5, ExprLitString dummyPos "test"])]
    ]
multipleWrongTypesProgram :: Program
multipleWrongTypesProgram =
  Program
    "multiple-wrong-types"
    [ DefFunction "helper" [Parameter "x" TypeI32, Parameter "y" TypeI32] TypeI32 [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr dummyPos (ExprCall dummyPos "helper" [ExprLitString dummyPos "wrong1", ExprLitBool dummyPos True])]
    ]
emptyParamsProgram :: Program
emptyParamsProgram =
  Program
    "empty-params"
    [ DefFunction "helper" [] TypeI32 [StmtReturn dummyPos (Just (ExprLitInt dummyPos 42))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr dummyPos (ExprCall dummyPos "helper" [])]
    ]
nestedFunctionCallProgram :: Program
nestedFunctionCallProgram =
  Program
    "nested-func-call"
    [ DefFunction "inner" [Parameter "x" TypeI32] TypeI32 [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))],
      DefFunction "outer" [Parameter "y" TypeI32] TypeI32 [StmtReturn dummyPos (Just (ExprCall dummyPos "inner" [ExprVar dummyPos "y"]))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr dummyPos (ExprCall dummyPos "outer" [ExprLitString dummyPos "wrong"])]
    ]
--
-- New helper functions for source position tests
--

