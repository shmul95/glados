module Semantics.VarsSpecs (varsSemanticsTests) where

import Rune.AST.Nodes
import Rune.Semantics.Vars (verifVars)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)
import Data.List (isInfixOf)
import qualified Data.HashMap.Strict as HM
import Control.Monad (unless)

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
      expectOk "handles empty parameter list" emptyParamsProgram,
      expectFuncStackContent "mangleFuncStack does not mangle single functions" noOverloadProgram ["foo"],
      expectFuncStackContent "mangleFuncStack mangles overloaded functions" overloadProgram ["null_bar_i32", "null_bar_f32"],
      expectOk "verifScope handles StmtReturn Nothing" returnNothingProgram,
      expectOk "verifScope handles empty block" emptyBlockProgram,

      expectOk "Instantiates generic function correctly" genericValidProgram,
      expectOk "Instantiates generic function twice (caching)" genericDoubleCallProgram,
      expectGenericInferenceError "Fails to infer generic return type without context/args" genericInferenceFailProgram
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
    Left msg | undefinedMsg missingVar `isInfixOf` msg -> return ()
    result -> assertFailure $ "Expected UndefinedVar error for " ++ missingVar ++ ", got: " ++ show result

expectTypeOverwrite :: String -> Program -> TestTree
expectTypeOverwrite label program =
  testCase label $ case verifVars program of
    Left msg | "TypeOverwrite:" `isInfixOf` msg -> return ()
    result -> assertFailure $ "Expected TypeOverwrite error, got: " ++ show result

expectMultipleType :: String -> Program -> TestTree
expectMultipleType label program =
  testCase label $ case verifVars program of
    Left msg | "MultipleType:" `isInfixOf` msg -> return ()
    result -> assertFailure $ "Expected MultipleType error, got: " ++ show result

expectFunctionNotFound :: String -> Program -> String -> TestTree
expectFunctionNotFound label program fname =
  testCase label $ case verifVars program of
    Left msg | ("Function " ++ fname ++ " not found") `isInfixOf` msg -> return ()
    result -> assertFailure $ "Expected 'Function not found' error for " ++ fname ++ ", got: " ++ show result

expectGenericInferenceError :: String -> Program -> TestTree
expectGenericInferenceError label program =
  testCase label $ case verifVars program of
    Left msg | "cannot be instantiated" `isInfixOf` msg -> return ()
    result -> assertFailure $ "Expected generic instantiation error, got: " ++ show result

expectFuncStackContent :: String -> Program -> [String] -> TestTree
expectFuncStackContent label program expectedNames = testCase label $
  case verifVars program of
    Right (_, fs) -> do
      let actualNames = HM.keys fs
      mapM_ (\name -> unless (name `elem` actualNames) $ assertFailure $ "Expected function " ++ name ++ " not found in FuncStack keys: " ++ show actualNames) expectedNames
    Left err -> assertFailure $ "Expected success, but got error: " ++ err

undefinedMsg :: String -> String
undefinedMsg name = "\n\tUndefinedVar: " ++ name ++ " doesn't exist in the scope"

validProgram :: Program
validProgram =
  Program
    "valid"
    [ DefFunction "bar" [Parameter "x" TypeI32] TypeNull [StmtReturn (Just ExprLitNull)],
      DefFunction "foo" [Parameter "arg" TypeI32] TypeI32
        [ StmtVarDecl "local" (Just TypeI32) (ExprVar "arg"),
          StmtExpr (ExprCall "bar" [ExprVar "local"]),
          StmtFor "i" (Just TypeI32) (Just (ExprLitInt 0)) (ExprLitInt 1) [StmtExpr (ExprVar "i")],
          StmtForEach "item" (Just TypeI32) (ExprVar "local") [StmtExpr (ExprVar "item")],
          StmtReturn (Just ExprLitNull)
        ]
    ]

invalidProgram :: Program
invalidProgram = Program "invalid" [DefFunction "foo" [] TypeI32 [StmtExpr (ExprVar "missing")]]

blockLeakProgram :: Program
blockLeakProgram = Program "block-leak" [DefFunction "foo" [] TypeI32 [StmtIf (ExprLitBool True) [StmtVarDecl "inner" Nothing (ExprLitInt 1)] Nothing, StmtExpr (ExprVar "inner")]]

structProgram :: Program
structProgram = Program "struct" [DefStruct "Vec" [] [DefFunction "len" [Parameter "self" TypeAny] TypeI32 [StmtExpr (ExprVar "self")]]]

overrideLeakProgram :: Program
overrideLeakProgram = Program "override-leak" [DefOverride "show" [] TypeNull [StmtExpr (ExprVar "ghost")]]

overrideValidProgram :: Program
overrideValidProgram = Program "override-valid" [DefFunction "myPrint" [Parameter "value" TypeF32] TypeNull [StmtReturn Nothing], DefOverride "myPrint" [Parameter "value" TypeI32] TypeNull [StmtVarDecl "y" (Just TypeI32) (ExprVar "value"), StmtReturn Nothing]]

forLeakProgram :: Program
forLeakProgram = Program "for-leak" [DefFunction "loop" [] TypeNull [StmtFor "i" Nothing (Just (ExprLitInt 0)) (ExprLitInt 1) [StmtExpr (ExprVar "i")], StmtExpr (ExprVar "i")]]

forEachLeakProgram :: Program
forEachLeakProgram = Program "foreach-leak" [DefFunction "loop" [] TypeNull [StmtVarDecl "source" Nothing (ExprLitInt 0), StmtForEach "item" Nothing (ExprVar "source") [StmtExpr (ExprVar "item")], StmtExpr (ExprVar "item")]]

returnLeakProgram :: Program
returnLeakProgram = Program "return-leak" [DefFunction "returns" [] TypeNull [StmtReturn (Just (ExprVar "retval"))]]

ifElseElseProgram :: Program
ifElseElseProgram = Program "if-else" [DefFunction "branch" [] TypeNull [StmtIf (ExprLitBool False) [] (Just [StmtExpr (ExprVar "elseVar")])]]

structInitProgram :: Program
structInitProgram = Program "struct-init" [DefFunction "build" [Parameter "seed" TypeI32] TypeNull [StmtVarDecl "point" (Just (TypeCustom "Point")) (ExprStructInit "Point" [("x", ExprVar "seed")]), StmtExpr (ExprAccess (ExprVar "point") "x")]]

expressionProgram :: Program
expressionProgram = Program "expr" [DefFunction "ops" [Parameter "x" TypeI32] TypeNull [StmtExpr (ExprBinary Add (ExprVar "x") (ExprUnary Negate (ExprVar "x")))]]

callArgsErrorProgram :: Program
callArgsErrorProgram = Program "call-args" [DefFunction "foo" [Parameter "x" TypeAny] TypeNull [StmtReturn Nothing], DefFunction "caller" [] TypeNull [StmtExpr (ExprCall "foo" [ExprVar "arg"])]]

structInitErrorProgram :: Program
structInitErrorProgram = Program "struct-init-err" [DefFunction "build" [] TypeNull [StmtVarDecl "point" Nothing (ExprStructInit "Point" [("x", ExprVar "fieldVal")])]]

accessErrorProgram :: Program
accessErrorProgram = Program "access-err" [DefFunction "access" [] TypeNull [StmtExpr (ExprAccess (ExprVar "target") "field")]]

assignmentTypeOverwriteProgram :: Program
assignmentTypeOverwriteProgram = Program "assign-type-overwrite" [DefFunction "test" [] TypeNull [StmtVarDecl "x" (Just TypeI32) (ExprLitInt 1), StmtAssignment (ExprVar "x") (ExprLitFloat 1.0)]]

binaryErrorProgram :: Program
binaryErrorProgram = Program "binary-err" [DefFunction "bin" [] TypeNull [StmtExpr (ExprBinary Add (ExprVar "lhs") (ExprLitInt 0))]]

unaryErrorProgram :: Program
unaryErrorProgram = Program "unary-err" [DefFunction "neg" [] TypeNull [StmtExpr (ExprUnary Negate (ExprVar "value"))]]

typeOverwriteProgram :: Program
typeOverwriteProgram = Program "type-overwrite" [DefFunction "test" [] TypeNull [StmtVarDecl "x" (Just TypeI32) (ExprLitInt 42), StmtVarDecl "x" (Just TypeF32) (ExprLitFloat 3.14)]]

multipleTypeProgram :: Program
multipleTypeProgram = Program "multiple-type" [DefFunction "test" [] TypeNull [StmtVarDecl "x" (Just TypeI32) (ExprLitFloat 3.14)]]

loopProgram :: Program
loopProgram = Program "loop" [DefFunction "test" [] TypeNull [StmtVarDecl "x" (Just TypeI32) (ExprLitInt 1), StmtLoop [StmtExpr (ExprVar "x")]]]

stopNextProgram :: Program
stopNextProgram = Program "stop-next" [DefFunction "test" [] TypeNull [StmtLoop [StmtStop, StmtNext]]]

nonVarAssignmentProgram :: Program
nonVarAssignmentProgram = Program "non-var-assign" [DefFunction "test" [] TypeNull [StmtVarDecl "point" (Just (TypeCustom "Point")) (ExprStructInit "Point" [("x", ExprLitInt 1)]), StmtAssignment (ExprAccess (ExprVar "point") "x") (ExprLitInt 2)]]

forWithoutStartProgram :: Program
forWithoutStartProgram = Program "for-no-start" [DefFunction "test" [] TypeNull [StmtFor "i" (Just TypeI32) Nothing (ExprLitInt 10) [StmtExpr (ExprVar "i")]]]

forEndErrorProgram :: Program
forEndErrorProgram = Program "for-end-error" [DefFunction "test" [] TypeNull [StmtFor "i" Nothing (Just (ExprLitInt 0)) (ExprVar "endVar") [StmtExpr (ExprVar "i")]]]

ifConditionErrorProgram :: Program
ifConditionErrorProgram = Program "if-cond-error" [DefFunction "test" [] TypeNull [StmtIf (ExprVar "condVar") [] Nothing]]

literalExpressionsProgram :: Program
literalExpressionsProgram = Program "literals" [DefFunction "test" [] TypeNull [StmtVarDecl "i" (Just TypeI32) (ExprLitInt 42), StmtVarDecl "f" (Just TypeF32) (ExprLitFloat 3.14), StmtVarDecl "s" (Just TypeString) (ExprLitString "hello"), StmtVarDecl "c" (Just TypeChar) (ExprLitChar 'a'), StmtVarDecl "b" (Just TypeBool) (ExprLitBool True), StmtVarDecl "n" (Just TypeNull) ExprLitNull]]

structDefProgram :: Program
structDefProgram = Program "struct-def" [DefStruct "Point" [] []]

knownFunctionCallProgram :: Program
knownFunctionCallProgram = Program "known-func" [DefFunction "helper" [] TypeI32 [StmtReturn (Just (ExprLitInt 1))], DefFunction "main" [] TypeNull [StmtExpr (ExprCall "helper" [])], DefFunction "test_show" [] TypeNull [StmtExpr (ExprCall "show" [ExprLitString "test"])]]

assignmentRHSErrorProgram :: Program
assignmentRHSErrorProgram = Program "assign-rhs-error" [DefFunction "test" [] TypeNull [StmtVarDecl "x" Nothing (ExprLitInt 1), StmtAssignment (ExprVar "x") (ExprVar "rhsVar")]]

unknownFunctionProgram :: Program
unknownFunctionProgram = Program "unknown-func" [DefFunction "main" [] TypeNull [StmtExpr (ExprCall "thisDoesNotExist" [])]]

wrongTypeProgram :: Program
wrongTypeProgram = Program "wrong-type" [DefFunction "helper" [Parameter "x" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))], DefFunction "main" [] TypeNull [StmtExpr (ExprCall "helper" [ExprLitString "wrong"])]]

tooFewArgsProgram :: Program
tooFewArgsProgram = Program "too-few-args" [DefFunction "helper" [Parameter "x" TypeI32, Parameter "y" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))], DefFunction "main" [] TypeNull [StmtExpr (ExprCall "helper" [ExprLitInt 1])]]

tooManyArgsProgram :: Program
tooManyArgsProgram = Program "too-many-args" [DefFunction "helper" [Parameter "x" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))], DefFunction "main" [] TypeNull [StmtExpr (ExprCall "helper" [ExprLitInt 1, ExprLitInt 2])]]

correctArgsProgram :: Program
correctArgsProgram = Program "correct-args" [DefFunction "helper" [Parameter "x" TypeI32, Parameter "y" TypeF32, Parameter "z" TypeString] TypeI32 [StmtReturn (Just (ExprVar "x"))], DefFunction "main" [] TypeNull [StmtExpr (ExprCall "helper" [ExprLitInt 1, ExprLitFloat 2.5, ExprLitString "test"])]]

multipleWrongTypesProgram :: Program
multipleWrongTypesProgram = Program "multiple-wrong-types" [DefFunction "helper" [Parameter "x" TypeI32, Parameter "y" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))], DefFunction "main" [] TypeNull [StmtExpr (ExprCall "helper" [ExprLitString "wrong1", ExprLitBool True])]]

emptyParamsProgram :: Program
emptyParamsProgram = Program "empty-params" [DefFunction "helper" [] TypeI32 [StmtReturn (Just (ExprLitInt 42))], DefFunction "main" [] TypeNull [StmtExpr (ExprCall "helper" [])]]

nestedFunctionCallProgram :: Program
nestedFunctionCallProgram = Program "nested-func-call" [DefFunction "inner" [Parameter "x" TypeI32] TypeI32 [StmtReturn (Just (ExprVar "x"))], DefFunction "outer" [Parameter "y" TypeI32] TypeI32 [StmtReturn (Just (ExprCall "inner" [ExprVar "y"]))], DefFunction "main" [] TypeNull [StmtExpr (ExprCall "outer" [ExprLitString "wrong"])]]

noOverloadProgram :: Program
noOverloadProgram = Program "no-overload" [DefFunction "foo" [Parameter "x" TypeI32] TypeNull [StmtReturn Nothing]]

overloadProgram :: Program
overloadProgram = Program "overload" [DefFunction "bar" [Parameter "x" TypeI32] TypeNull [StmtReturn Nothing], DefOverride "bar" [Parameter "y" TypeF32] TypeNull [StmtReturn Nothing]]

returnNothingProgram :: Program
returnNothingProgram = Program "return-nothing" [DefFunction "foo" [] TypeNull [StmtReturn Nothing]]

emptyBlockProgram :: Program
emptyBlockProgram = Program "empty-block" [DefFunction "foo" [] TypeNull []]

genericValidProgram :: Program
genericValidProgram = Program "generic-valid"
  [ DefFunction "identity" [Parameter "x" TypeAny] TypeAny [StmtReturn (Just (ExprVar "x"))]
  , DefFunction "main" [] TypeNull
      [ StmtVarDecl "i" (Just TypeI32) (ExprCall "identity" [ExprLitInt 1])
      ]
  ]

genericDoubleCallProgram :: Program
genericDoubleCallProgram = Program "generic-double"
  [ DefFunction "identity" [Parameter "x" TypeAny] TypeAny [StmtReturn (Just (ExprVar "x"))]
  , DefFunction "main" [] TypeNull
      [ StmtExpr (ExprCall "identity" [ExprLitInt 1])
      , StmtExpr (ExprCall "identity" [ExprLitInt 2])
      ]
  ]

genericInferenceFailProgram :: Program
genericInferenceFailProgram = Program "generic-fail"
  [ DefFunction "make" [] TypeAny [StmtReturn (Just (ExprLitInt 0))]
  , DefFunction "main" [] TypeNull [StmtExpr (ExprCall "make" [])]
  ]
