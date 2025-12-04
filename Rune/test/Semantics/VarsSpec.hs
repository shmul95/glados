module Semantics.VarsSpec (varsSemanticsTests) where

import Rune.AST.Nodes
import Rune.Semantics.Vars (verifVars)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.List (isInfixOf)

varsSemanticsTests :: TestTree
varsSemanticsTests =
  testGroup
    "Rune.Semantics.Vars"
    [ expectOk "accepts parameters and local declarations" validProgram,
      expectErr "rejects undefined reference" invalidProgram "missing",
      expectErr "scopes nested block declarations" blockLeakProgram "inner",
      expectOk "validates struct methods recursively" structProgram,
      -- expectErr "struct method reports undefined variable" structMethodErrorProgram "ghost",
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
      expectErr "binary operands validated" binaryErrorProgram "lhs",
      expectErr "unary operand validated" unaryErrorProgram "value",
      -- New tests for better coverage
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
      -- Tests specifically for checkEachParam and checkParamType functions
      expectUnknownFunction "detects unknown function calls" unknownFunctionProgram "thisDoesNotExist",
      expectWrongType "detects wrong argument type" wrongTypeProgram,
      expectWrongNbArgsLess "detects too few arguments" tooFewArgsProgram,
      expectWrongNbArgsMore "detects too many arguments" tooManyArgsProgram,
      expectOk "validates correct function call arguments" correctArgsProgram,
      expectWrongType "detects multiple wrong types in same call" multipleWrongTypesProgram,
      expectOk "handles empty parameter list" emptyParamsProgram,
      expectWrongType "validates nested function call arguments" nestedFunctionCallProgram
    ]

expectOk :: String -> Program -> TestTree
expectOk label program = testCase label $ verifVars program @?= Nothing

expectErr :: String -> Program -> String -> TestTree
expectErr label program missingVar =
  testCase label $ verifVars program @?= Just (undefinedMsg missingVar)

expectTypeOverwrite :: String -> Program -> TestTree
expectTypeOverwrite label program =
  testCase label $ case verifVars program of
    Just msg | "TypeOverwrite:" `isInfixOf` msg -> return ()
    result -> fail $ "Expected TypeOverwrite error, got: " ++ show result

expectMultipleType :: String -> Program -> TestTree
expectMultipleType label program =
  testCase label $ case verifVars program of
    Just msg | "MultipleType:" `isInfixOf` msg -> return ()
    result -> fail $ "Expected MultipleType error, got: " ++ show result

expectUnknownFunction :: String -> Program -> String -> TestTree
expectUnknownFunction label program fname =
  testCase label $ case verifVars program of
    Just msg | ("UnknownFunction:" `isInfixOf` msg) && (fname `isInfixOf` msg) -> return ()
    result -> fail $ "Expected UnknownFunction error for " ++ fname ++ ", got: " ++ show result

expectWrongType :: String -> Program -> TestTree
expectWrongType label program =
  testCase label $ case verifVars program of
    Just msg | "WrongType:" `isInfixOf` msg -> return ()
    result -> fail $ "Expected WrongType error, got: " ++ show result

expectWrongNbArgsLess :: String -> Program -> TestTree
expectWrongNbArgsLess label program =
  testCase label $ case verifVars program of
    Just msg | ("WrongNbArgs:" `isInfixOf` msg) && ("too less" `isInfixOf` msg) -> return ()
    result -> fail $ "Expected WrongNbArgs (too less) error, got: " ++ show result

expectWrongNbArgsMore :: String -> Program -> TestTree
expectWrongNbArgsMore label program =
  testCase label $ case verifVars program of
    Just msg | ("WrongNbArgs:" `isInfixOf` msg) && ("too much" `isInfixOf` msg) -> return ()
    result -> fail $ "Expected WrongNbArgs (too much) error, got: " ++ show result

undefinedMsg :: String -> String
undefinedMsg name = "\n\tUndefinedVar: " ++ name ++ " doesn't exist in the scope"

validProgram :: Program
validProgram =
  Program
    "valid"
    [ DefFunction
        "bar"
        [Parameter "x" TypeI32]
        TypeNull
        [StmtReturn Nothing],
      DefFunction
        "foo"
        [Parameter "arg" TypeI32]
        TypeI32
        [ StmtVarDecl "local" (Just TypeI32) (ExprVar "arg"),
          StmtExpr (ExprCall "bar" [ExprVar "local"]),
          StmtFor "i" Nothing (Just (ExprLitInt 0)) (ExprLitInt 1) [StmtExpr (ExprVar "i")],
          StmtForEach "item" Nothing (ExprVar "local") [StmtExpr (ExprVar "item")],
          StmtReturn Nothing
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
        [StmtExpr (ExprVar "missing")]
    ]

blockLeakProgram :: Program
blockLeakProgram =
  Program
    "block-leak"
    [ DefFunction
        "foo"
        []
        TypeI32
        [ StmtIf
            (ExprLitBool True)
            [StmtVarDecl "inner" Nothing (ExprLitInt 1)]
            Nothing,
          StmtExpr (ExprVar "inner")
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
            [StmtExpr (ExprVar "self")]
        ]
    ]

-- structMethodErrorProgram :: Program
-- structMethodErrorProgram =
--   Program
--     "struct-error"
--     [ DefStruct
--         "Vec"
--         []
--         [ DefFunction
--             "len"
--             []
--             TypeI32
--             [StmtExpr (ExprVar "ghost")]
--         ]
--     ]

overrideLeakProgram :: Program
overrideLeakProgram =
  Program
    "override-leak"
    [ DefOverride
        "show"
        []
        TypeNull
        [StmtExpr (ExprVar "ghost")]
    ]

overrideValidProgram :: Program
overrideValidProgram =
  Program
    "override-valid"
    [ DefOverride
        "print"
        [Parameter "value" TypeI32]
        TypeI32
        [StmtReturn (Just (ExprVar "value"))]
    ]

forLeakProgram :: Program
forLeakProgram =
  Program
    "for-leak"
    [ DefFunction
        "loop"
        []
        TypeNull
        [ StmtFor "i" Nothing (Just (ExprLitInt 0)) (ExprLitInt 1) [StmtExpr (ExprVar "i")],
          StmtExpr (ExprVar "i")
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
        [ StmtVarDecl "source" Nothing (ExprLitInt 0),
          StmtForEach "item" Nothing (ExprVar "source") [StmtExpr (ExprVar "item")],
          StmtExpr (ExprVar "item")
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
        [StmtReturn (Just (ExprVar "retval"))]
    ]

ifElseElseProgram :: Program
ifElseElseProgram =
  Program
    "if-else"
    [ DefFunction
        "branch"
        []
        TypeNull
        [ StmtIf
            (ExprLitBool False)
            []
            (Just [StmtExpr (ExprVar "elseVar")])
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
        [ StmtVarDecl "point" Nothing (ExprStructInit "Point" [("x", ExprVar "seed")]),
          StmtExpr (ExprAccess (ExprVar "point") "x")
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
        [ StmtExpr (ExprBinary Add (ExprVar "x") (ExprUnary Negate (ExprVar "x")))
        ]
    ]

callArgsErrorProgram :: Program
callArgsErrorProgram =
  Program
    "call-args"
    [ DefFunction "foo" [Parameter "x" TypeAny] TypeNull [StmtReturn Nothing],
      DefFunction
        "caller"
        []
        TypeNull
        [StmtExpr (ExprCall "foo" [ExprVar "arg"])]
    ]

structInitErrorProgram :: Program
structInitErrorProgram =
  Program
    "struct-init-err"
    [ DefFunction
        "build"
        []
        TypeNull
        [StmtVarDecl "point" Nothing (ExprStructInit "Point" [("x", ExprVar "fieldVal")])]
    ]

accessErrorProgram :: Program
accessErrorProgram =
  Program
    "access-err"
    [ DefFunction
        "access"
        []
        TypeNull
        [StmtExpr (ExprAccess (ExprVar "target") "field")]
    ]

binaryErrorProgram :: Program
binaryErrorProgram =
  Program
    "binary-err"
    [ DefFunction
        "bin"
        []
        TypeNull
        [StmtExpr (ExprBinary Add (ExprVar "lhs") (ExprLitInt 0))]
    ]

unaryErrorProgram :: Program
unaryErrorProgram =
  Program
    "unary-err"
    [ DefFunction
        "neg"
        []
        TypeNull
        [StmtExpr (ExprUnary Negate (ExprVar "value"))]
    ]

-- New test programs for better coverage

typeOverwriteProgram :: Program
typeOverwriteProgram =
  Program
    "type-overwrite"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtVarDecl "x" (Just TypeI32) (ExprLitInt 42),
          StmtVarDecl "x" (Just TypeF32) (ExprLitFloat 3.14)
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
        [StmtVarDecl "x" (Just TypeI32) (ExprLitFloat 3.14)]
    ]

loopProgram :: Program
loopProgram =
  Program
    "loop"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtVarDecl "x" Nothing (ExprLitInt 1),
          StmtLoop [StmtExpr (ExprVar "x")]
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
        [ StmtLoop [StmtStop, StmtNext]
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
        [ StmtVarDecl "point" Nothing (ExprStructInit "Point" [("x", ExprLitInt 1)]),
          StmtAssignment (ExprAccess (ExprVar "point") "x") (ExprLitInt 2)
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
        [ StmtFor "i" (Just TypeI32) Nothing (ExprLitInt 10) [StmtExpr (ExprVar "i")]
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
        [ StmtFor "i" Nothing (Just (ExprLitInt 0)) (ExprVar "endVar") [StmtExpr (ExprVar "i")]
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
        [ StmtIf (ExprVar "condVar") [] Nothing
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
        [ StmtVarDecl "i" Nothing (ExprLitInt 42),
          StmtVarDecl "f" Nothing (ExprLitFloat 3.14),
          StmtVarDecl "s" Nothing (ExprLitString "hello"),
          StmtVarDecl "c" Nothing (ExprLitChar 'a'),
          StmtVarDecl "b" Nothing (ExprLitBool True),
          StmtVarDecl "n" Nothing ExprLitNull
        ]
    ]

structDefProgram :: Program
structDefProgram =
  Program
    "struct-def"
    [ DefStruct "Point" [] []
    ]

knownFunctionCallProgram :: Program
knownFunctionCallProgram =
  Program
    "known-func"
    [ DefFunction "helper" [] TypeI32 [StmtReturn (Just (ExprLitInt 1))],
      DefFunction "main" [] TypeNull [StmtExpr (ExprCall "helper" [])],
      DefFunction "test_show" [] TypeNull [StmtExpr (ExprCall "show" [ExprLitString "test"])]
    ]

assignmentRHSErrorProgram :: Program
assignmentRHSErrorProgram =
  Program
    "assign-rhs-error"
    [ DefFunction
        "test"
        []
        TypeNull
        [ StmtVarDecl "x" Nothing (ExprLitInt 1),
          StmtAssignment (ExprVar "x") (ExprVar "rhsVar")
        ]
    ]

-- Test programs for checkEachParam and checkParamType functions

unknownFunctionProgram :: Program
unknownFunctionProgram =
  Program
    "unknown-func"
    [ DefFunction
        "main"
        []
        TypeNull
        [StmtExpr (ExprCall "thisDoesNotExist" [])]
    ]

wrongTypeProgram :: Program
wrongTypeProgram =
  Program
    "wrong-type"
    [ DefFunction "helper" [Parameter "x" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr (ExprCall "helper" [ExprLitString "wrong"])]
    ]

tooFewArgsProgram :: Program
tooFewArgsProgram =
  Program
    "too-few-args"
    [ DefFunction "helper" [Parameter "x" TypeI32, Parameter "y" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr (ExprCall "helper" [ExprLitInt 1])]
    ]

tooManyArgsProgram :: Program
tooManyArgsProgram =
  Program
    "too-many-args"
    [ DefFunction "helper" [Parameter "x" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr (ExprCall "helper" [ExprLitInt 1, ExprLitInt 2])]
    ]

correctArgsProgram :: Program
correctArgsProgram =
  Program
    "correct-args"
    [ DefFunction "helper" [Parameter "x" TypeI32, Parameter "y" TypeF32, Parameter "z" TypeString] TypeI32 [StmtReturn (Just (ExprVar "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr (ExprCall "helper" [ExprLitInt 1, ExprLitFloat 2.5, ExprLitString "test"])]
    ]

multipleWrongTypesProgram :: Program
multipleWrongTypesProgram =
  Program
    "multiple-wrong-types"
    [ DefFunction "helper" [Parameter "x" TypeI32, Parameter "y" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr (ExprCall "helper" [ExprLitString "wrong1", ExprLitBool True])]
    ]

emptyParamsProgram :: Program
emptyParamsProgram =
  Program
    "empty-params"
    [ DefFunction "helper" [] TypeI32 [StmtReturn (Just (ExprLitInt 42))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr (ExprCall "helper" [])]
    ]

nestedFunctionCallProgram :: Program
nestedFunctionCallProgram =
  Program
    "nested-func-call"
    [ DefFunction "inner" [Parameter "x" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))],
      DefFunction "outer" [Parameter "y" TypeI32] TypeI32 [StmtReturn (Just (ExprCall "inner" [ExprVar "y"]))],
      DefFunction
        "main"
        []
        TypeNull
        [StmtExpr (ExprCall "outer" [ExprLitString "wrong"])]
    ]
