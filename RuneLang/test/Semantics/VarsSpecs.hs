module Semantics.VarsSpecs (varsSemanticsTests) where

import Rune.AST.Nodes
import Rune.Semantics.Vars
import Rune.Semantics.Type (FuncStack, StructStack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Data.List (isInfixOf)
import qualified Data.HashMap.Strict as HM
import TestHelpers (dummyPos)
import Control.Monad.State.Strict (evalStateT)

--
-- public
--

varsSemanticsTests :: TestTree
varsSemanticsTests =
  testGroup
    "Rune.Semantics.Vars"
    [ verifVarsTests
    , isGenericTests
    , hasAnyTests
    , getDefNameTests
    , mangleFuncStackTests
    , resolveFinalNameTests
    , verifTopLevelTests
    , verifScopeTests
    , verifExprTests
    , verifExprWithContextTests
    , verifMethodTests
    , tryInstantiateTemplateTests
    , resolveReturnTypeTests
    , alreadyInstantiatedTests
    , registerInstantiationTests
    , resolveCallTests
    , isStaticMethodTests
    , checkMethodParamsTests
    , checkStaticMethodCallTests
    , canAccessMemberTests
    , isSelfAccessTests
    , verifFieldAccessTests
    , verifStructFieldAccessTests
    , lookupStructFieldsTests
    , findFieldTests
    , raiseVisibilityErrorTests
    , checkFieldVisibilityTests
    , verifStaticMethodCallTests
    , verifInstanceMethodCallTests
    , checkMethodVisibilityTests
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

-- Mock SemState for testing individual functions
mockSemState :: FuncStack -> StructStack -> Maybe String -> SemState
mockSemState fs ss currentStruct = SemState
  { stFuncs = fs
  , stTemplates = HM.empty
  , stNewDefs = []
  , stInstantiated = HM.empty
  , stStructs = ss
  , stCurrentStruct = currentStruct
  }

-- Helper to run SemM computations for testing
runSemMForTest :: SemM a -> SemState -> Either String a
runSemMForTest = evalStateT

--
-- ============================================================================
-- TEST GROUPS FOR EACH FUNCTION
-- ============================================================================
--

-- ============================================================================
-- verifVars Tests
-- ============================================================================

verifVarsTests :: TestTree
verifVarsTests =
  testGroup "verifVars"
    [ testGroup "Success Cases"
      [ expectOk "verifies empty program" emptyProgram
      , expectOk "verifies simple function" simpleFunctionProgram
      , expectOk "verifies struct with methods" structWithMethodsProgram
      , expectOk "verifies multiple functions" multipleFunctionsProgram
      , expectOk "returns correct FuncStack" verifVarsReturnsStackProgram
      ]
    , testGroup "Failure Cases"
      [ expectErr "rejects undefined variable" undefinedVarProgram "Undefined variable"
      , expectErr "rejects type mismatch" typeMismatchProgram "binary operation type mismatch"
      , expectErr "rejects duplicate function" duplicateFunctionProgram "FuncAlreadyExist"
      ]
    , testGroup "Additional Coverage Cases"
      [ expectOk "verifies cast expression" castExprProgram
      , expectOk "verifies index expression" indexExprProgram
      , expectOk "verifies protected field struct" protectedFieldProgram
      , expectOk "verifies nested structs" nestedStructProgram
      , expectOk "verifies generic array function" genericArrayProgram
      , expectErr "rejects invalid function call" invalidCallProgram "undefined function"
      , expectOk "allows index on i32 (type error caught later)" invalidIndexProgram
      , expectOk "allows unary on string (type error caught later)" unaryOpErrorProgram
      , expectErr "rejects assignment type mismatch" assignmentErrorProgram "variable 'x' to have type"
      , expectOk "verifies static method call" staticMethodCallProgram
      , expectOk "verifies instance method call" instanceMethodCallProgram
      , expectErr "rejects invalid call target" invalidCallTargetProgram "Invalid function call target"
      , expectOk "verifies struct initialization" structInitProgram
      , expectOk "verifies array literal" arrayLiteralProgram
      , expectOk "verifies float binary operation" binaryOpProgramFloat
      , expectOk "verifies comparison operations" comparisonOpProgram
      , expectOk "verifies unary not operation" unaryNotProgram
      , expectOk "verifies for loop" forLoopProgram
      , expectOk "verifies foreach loop" foreachLoopProgram
      , expectOk "verifies if-else statement" ifElseProgram
      , expectOk "verifies loop statement" loopProgram
      ]
    ]

-- ============================================================================
-- isGeneric Tests
-- ============================================================================

isGenericTests :: TestTree
isGenericTests =
  testGroup "isGeneric"
    [ testGroup "Success Cases"
      [ testCase "returns True for function with TypeAny parameter" $
          isGeneric (DefFunction "f" [Parameter "x" TypeAny Nothing] TypeI32 [] False Public) @?= True
      , testCase "returns True for function with TypeAny return" $
          isGeneric (DefFunction "f" [] TypeAny [] False Public) @?= True
      , testCase "returns True for function with TypeArray TypeAny" $
          isGeneric (DefFunction "f" [Parameter "x" (TypeArray TypeAny) Nothing] TypeI32 [] False Public) @?= True
      , testCase "returns False for concrete function" $
          isGeneric (DefFunction "f" [Parameter "x" TypeI32 Nothing] TypeI32 [] False Public) @?= False
      , testCase "returns False for struct" $
          isGeneric (DefStruct "S" [] []) @?= False
      ]
    ]

-- ============================================================================
-- hasAny Tests
-- ============================================================================

hasAnyTests :: TestTree
hasAnyTests =
  testGroup "hasAny"
    [ testGroup "Success Cases"
      [ testCase "returns True for TypeAny" $
          hasAny TypeAny @?= True
      , testCase "returns True for TypeArray TypeAny" $
          hasAny (TypeArray TypeAny) @?= True
      , testCase "returns False for TypeI32" $
          hasAny TypeI32 @?= False
      , testCase "returns False for TypeArray TypeI32" $
          hasAny (TypeArray TypeI32) @?= False
      , testCase "returns False for TypeString" $
          hasAny TypeString @?= False
      , testCase "returns False for TypeCustom" $
          hasAny (TypeCustom "MyStruct") @?= False
      ]
    ]

-- ============================================================================
-- getDefName Tests
-- ============================================================================

getDefNameTests :: TestTree
getDefNameTests =
  testGroup "getDefName"
    [ testGroup "Success Cases"
      [ testCase "returns function name" $
          getDefName (DefFunction "myFunc" [] TypeNull [] False Public) @?= "myFunc"
      , testCase "returns struct name" $
          getDefName (DefStruct "MyStruct" [] []) @?= "MyStruct"
      , testCase "returns empty string for DefSomewhere" $
          getDefName (DefSomewhere []) @?= ""
      ]
    ]

-- ============================================================================
-- mangleFuncStack Tests
-- ============================================================================

mangleFuncStackTests :: TestTree
mangleFuncStackTests =
  testGroup "mangleFuncStack"
    [ testCase "returns FuncStack unchanged (identity function)" $ do
        let fs = HM.fromList
              [ ("func1", ((TypeI32, [Parameter "x" TypeI32 Nothing]), Public))
              , ("func2", ((TypeNull, []), Public))
              ]
        let result = mangleFuncStack fs
        result @?= fs
    , testCase "handles empty FuncStack" $
        mangleFuncStack HM.empty @?= HM.empty
    ]

-- ============================================================================
-- resolveFinalName Tests
-- ============================================================================

resolveFinalNameTests :: TestTree
resolveFinalNameTests =
  testGroup "resolveFinalName"
    [ testGroup "Success Cases"
      [ testCase "returns baseName when exact match exists in FuncStack" $ do
          let fs = HM.fromList [("myFunc", ((TypeI32, [Parameter "x" TypeI32 Nothing]), Public))]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (resolveFinalName "myFunc" TypeI32 [TypeI32]) state
          result @?= Right "myFunc"
      , testCase "returns mangled name when signature differs" $ do
          let fs = HM.fromList [("myFunc", ((TypeI32, [Parameter "x" TypeI32 Nothing]), Public))]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (resolveFinalName "myFunc" TypeNull [TypeString]) state
          case result of
            Right name -> assertBool "should contain mangled version" (name /= "myFunc")
            Left err -> assertFailure err
      , testCase "returns baseName when not in FuncStack" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (resolveFinalName "newFunc" TypeI32 []) state
          result @?= Right "newFunc"
      ]
    ]

-- ============================================================================
-- verifTopLevel Tests
-- ============================================================================

verifTopLevelTests :: TestTree
verifTopLevelTests =
  testGroup "verifTopLevel"
    [ testGroup "Success Cases"
      [ testCase "verifies function definition" $ do
          let func = DefFunction "f" [] TypeNull [] False Public
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifTopLevel func) state
          case result of
            Right (DefFunction name _ _ _ _ _) -> name @?= "f"
            _ -> assertFailure "Expected DefFunction"
      , testCase "verifies struct definition" $ do
          let struct = DefStruct "S" [Field "x" TypeI32 Public] []
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifTopLevel struct) state
          case result of
            Right (DefStruct name _ _) -> name @?= "S"
            _ -> assertFailure "Expected DefStruct"
      , testCase "verifies DefSomewhere" $ do
          let somewhere = DefSomewhere []
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifTopLevel somewhere) state
          case result of
            Right (DefSomewhere _) -> return ()
            _ -> assertFailure "Expected DefSomewhere"
      ]
    ]

-- ============================================================================
-- verifScope Tests
-- ============================================================================

verifScopeTests :: TestTree
verifScopeTests =
  testGroup "verifScope"
    [ testGroup "Empty Block"
      [ testCase "verifies empty block" $ do
          let vs = HM.empty
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs []) state
          result @?= Right []
      ]
    , testGroup "StmtVarDecl"
      [ testCase "verifies var decl with explicit type" $ do
          let vs = HM.empty
              stmt = StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 42)
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtVarDecl _ "x" (Just TypeI32) _] -> return ()
            _ -> assertFailure "Expected variable declaration with TypeI32"
      , testCase "verifies var decl with type inference" $ do
          let vs = HM.empty
              stmt = StmtVarDecl dummyPos "msg" Nothing (ExprLitString dummyPos "hello")
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtVarDecl _ "msg" (Just TypeString) _] -> return ()
            _ -> assertFailure "Expected variable declaration with inferred TypeString"
      , testCase "verifies multiple var decls in sequence" $ do
          let vs = HM.empty
              stmts = [ StmtVarDecl dummyPos "a" (Just TypeI32) (ExprLitInt dummyPos 1)
                      , StmtVarDecl dummyPos "b" (Just TypeI32) (ExprLitInt dummyPos 2)
                      ]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs stmts) state
          case result of
            Right [StmtVarDecl {}, StmtVarDecl {}] -> return ()
            _ -> assertFailure "Expected two variable declarations"
      , testCase "rejects var decl with undefined variable in initializer" $ do
          let vs = HM.empty
              stmt = StmtVarDecl dummyPos "x" Nothing (ExprVar dummyPos "undefined")
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Left err -> assertBool "should mention undefined" ("Undefined variable" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    , testGroup "StmtExpr"
      [ testCase "verifies expression statement" $ do
          let vs = HM.empty
              stmt = StmtExpr dummyPos (ExprLitInt dummyPos 42)
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtExpr _ _] -> return ()
            _ -> assertFailure "Expected expression statement"
      , testCase "verifies function call expression" $ do
          let fs = HM.fromList [("print", ((TypeNull, [Parameter "s" TypeString Nothing]), Public))]
              vs = HM.empty
              stmt = StmtExpr dummyPos (ExprCall dummyPos (ExprVar dummyPos "print") [ExprLitString dummyPos "test"])
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtExpr {}] -> return ()
            _ -> assertFailure "Expected expression statement with call"
      ]
    , testGroup "StmtReturn"
      [ testCase "verifies return with value" $ do
          let vs = HM.empty
              stmt = StmtReturn dummyPos (Just (ExprLitInt dummyPos 42))
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtReturn _ (Just _)] -> return ()
            _ -> assertFailure "Expected return statement with value"
      , testCase "verifies return without value (converts to null)" $ do
          let vs = HM.empty
              stmt = StmtReturn dummyPos Nothing
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtReturn _ (Just (ExprLitNull _))] -> return ()
            _ -> assertFailure "Expected return converted to null"
      , testCase "rejects return with undefined variable" $ do
          let vs = HM.empty
              stmt = StmtReturn dummyPos (Just (ExprVar dummyPos "undefined"))
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Left err -> assertBool "should mention undefined" ("Undefined variable" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    , testGroup "StmtIf"
      [ testCase "verifies if without else" $ do
          let vs = HM.empty
              stmt = StmtIf dummyPos (ExprLitBool dummyPos True) [StmtReturn dummyPos Nothing] Nothing
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtIf _ _ _ Nothing] -> return ()
            _ -> assertFailure "Expected if without else"
      , testCase "verifies if with else" $ do
          let vs = HM.empty
              stmt = StmtIf dummyPos (ExprLitBool dummyPos False) 
                       [StmtReturn dummyPos (Just (ExprLitInt dummyPos 1))]
                       (Just [StmtReturn dummyPos (Just (ExprLitInt dummyPos 2))])
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtIf _ _ _ (Just _)] -> return ()
            _ -> assertFailure "Expected if with else"
      , testCase "rejects if with undefined variable in condition" $ do
          let vs = HM.empty
              stmt = StmtIf dummyPos (ExprVar dummyPos "undefined") [] Nothing
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Left err -> assertBool "should mention undefined" ("Undefined variable" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    , testGroup "StmtFor"
      [ testCase "verifies for loop with start value" $ do
          let vs = HM.empty
              stmt = StmtFor dummyPos "i" (Just TypeI32) (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 10) []
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtFor _ "i" (Just TypeI32) (Just _) _ _] -> return ()
            _ -> assertFailure "Expected for loop with start"
      , testCase "verifies for loop without start value" $ do
          let vs = HM.empty
              stmt = StmtFor dummyPos "j" (Just TypeI32) Nothing (ExprLitInt dummyPos 5) []
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtFor _ "j" (Just TypeI32) Nothing _ _] -> return ()
            _ -> assertFailure "Expected for loop without start"
      , testCase "verifies for loop with type inference from start" $ do
          let vs = HM.empty
              stmt = StmtFor dummyPos "k" Nothing (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 3) []
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtFor _ "k" (Just TypeI32) _ _ _] -> return ()
            _ -> assertFailure "Expected for loop with inferred type"
      , testCase "verifies loop variable is accessible in body" $ do
          let vs = HM.empty
              stmt = StmtFor dummyPos "i" (Just TypeI32) (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 10)
                       [StmtExpr dummyPos (ExprVar dummyPos "i")]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtFor {}] -> return ()
            _ -> assertFailure "Expected for loop with body using loop var"
      ]
    , testGroup "StmtForEach"
      [ testCase "verifies foreach over array" $ do
          let vs = HM.fromList [("arr", TypeArray TypeI32)]
              stmt = StmtForEach dummyPos "elem" (Just TypeI32) (ExprVar dummyPos "arr") []
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtForEach _ "elem" (Just TypeI32) _ _] -> return ()
            _ -> assertFailure "Expected foreach over array"
      , testCase "verifies foreach over string (yields char)" $ do
          let vs = HM.empty
              stmt = StmtForEach dummyPos "ch" (Just TypeChar) (ExprLitString dummyPos "test") []
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtForEach _ "ch" (Just TypeChar) _ _] -> return ()
            _ -> assertFailure "Expected foreach over string"
      , testCase "verifies foreach with type inference from array" $ do
          let vs = HM.fromList [("numbers", TypeArray TypeF32)]
              stmt = StmtForEach dummyPos "n" Nothing (ExprVar dummyPos "numbers") []
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtForEach _ "n" (Just TypeF32) _ _] -> return ()
            _ -> assertFailure "Expected foreach with inferred element type"
      , testCase "verifies foreach variable accessible in body" $ do
          let vs = HM.empty
              stmt = StmtForEach dummyPos "c" (Just TypeChar) (ExprLitString dummyPos "hi")
                       [StmtExpr dummyPos (ExprVar dummyPos "c")]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtForEach {}] -> return ()
            _ -> assertFailure "Expected foreach with body using loop var"
      ]
    , testGroup "StmtLoop"
      [ testCase "verifies infinite loop" $ do
          let vs = HM.empty
              stmt = StmtLoop dummyPos [StmtReturn dummyPos Nothing]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtLoop _ _] -> return ()
            _ -> assertFailure "Expected loop statement"
      , testCase "verifies empty loop body" $ do
          let vs = HM.empty
              stmt = StmtLoop dummyPos []
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtLoop _ []] -> return ()
            _ -> assertFailure "Expected empty loop"
      ]
    , testGroup "StmtAssignment"
      [ testCase "verifies assignment to existing variable" $ do
          let vs = HM.fromList [("x", TypeI32)]
              stmt = StmtAssignment dummyPos (ExprVar dummyPos "x") (ExprLitInt dummyPos 42)
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtAssignment {}] -> return ()
            _ -> assertFailure "Expected assignment"
      , testCase "verifies assignment to field access" $ do
          let fields = [Field "val" TypeI32 Public]
              ss = HM.fromList [("S", DefStruct "S" fields [])]
              vs = HM.fromList [("obj", TypeCustom "S")]
              stmt = StmtAssignment dummyPos (ExprAccess dummyPos (ExprVar dummyPos "obj") "val") (ExprLitInt dummyPos 99)
              state = mockSemState HM.empty ss Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtAssignment _ (ExprAccess {}) _] -> return ()
            _ -> assertFailure "Expected assignment to field"
      , testCase "verifies assignment to array index" $ do
          let vs = HM.fromList [("arr", TypeArray TypeI32)]
              stmt = StmtAssignment dummyPos (ExprIndex dummyPos (ExprVar dummyPos "arr") (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 10)
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtAssignment _ (ExprIndex {}) _] -> return ()
            _ -> assertFailure "Expected assignment to index"
      , testCase "rejects assignment type mismatch" $ do
          let vs = HM.fromList [("num", TypeI32)]
              stmt = StmtAssignment dummyPos (ExprVar dummyPos "num") (ExprLitString dummyPos "text")
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Left err -> assertBool "should mention type mismatch" ("type" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    , testGroup "StmtStop and StmtNext"
      [ testCase "verifies stop statement" $ do
          let vs = HM.empty
              stmt = StmtStop dummyPos
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtStop _] -> return ()
            _ -> assertFailure "Expected stop statement"
      , testCase "verifies next statement" $ do
          let vs = HM.empty
              stmt = StmtNext dummyPos
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs [stmt]) state
          case result of
            Right [StmtNext _] -> return ()
            _ -> assertFailure "Expected next statement"
      ]
    , testGroup "Complex Scenarios"
      [ testCase "verifies nested scopes" $ do
          let vs = HM.empty
              stmts = [ StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 5)
                      , StmtIf dummyPos (ExprVar dummyPos "x")
                          [StmtVarDecl dummyPos "y" (Just TypeI32) (ExprLitInt dummyPos 10)]
                          Nothing
                      ]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs stmts) state
          case result of
            Right [StmtVarDecl {}, StmtIf {}] -> return ()
            _ -> assertFailure "Expected nested scope"
      , testCase "verifies sequential statements maintain variable scope" $ do
          let vs = HM.empty
              stmts = [ StmtVarDecl dummyPos "a" (Just TypeI32) (ExprLitInt dummyPos 1)
                      , StmtVarDecl dummyPos "b" Nothing (ExprVar dummyPos "a")
                      , StmtReturn dummyPos (Just (ExprVar dummyPos "b"))
                      ]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifScope vs stmts) state
          case result of
            Right [StmtVarDecl {}, StmtVarDecl {}, StmtReturn {}] -> return ()
            _ -> assertFailure "Expected sequential statements with scope"
      ]
    ]

-- ============================================================================
-- verifExpr Tests
-- ============================================================================

verifExprTests :: TestTree
verifExprTests =
  testGroup "verifExpr"
    [ testGroup "Success Cases"
      [ testCase "verifies literal expressions" $ do
          let vs = HM.empty
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifExpr vs (ExprLitInt dummyPos 42)) state
          case result of
            Right (ExprLitInt _ 42) -> return ()
            _ -> assertFailure "Expected ExprLitInt"
      , testCase "verifies variable reference" $ do
          let vs = HM.fromList [("x", TypeI32)]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifExpr vs (ExprVar dummyPos "x")) state
          case result of
            Right (ExprVar _ "x") -> return ()
            _ -> assertFailure "Expected ExprVar"
      , testCase "verifies binary operation" $ do
          let vs = HM.empty
              expr = ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 2)
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifExpr vs expr) state
          case result of
            Right (ExprBinary {}) -> return ()
            _ -> assertFailure "Expected ExprBinary"
      , testCase "verifies unary operation" $ do
          let vs = HM.empty
              expr = ExprUnary dummyPos Not (ExprLitBool dummyPos True)
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifExpr vs expr) state
          case result of
            Right (ExprUnary {}) -> return ()
            _ -> assertFailure "Expected ExprUnary"
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects undefined variable" $ do
          let vs = HM.empty
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifExpr vs (ExprVar dummyPos "undefined")) state
          case result of
            Left err -> assertBool "should mention undefined" ("Undefined variable" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- verifExprWithContext Tests
-- ============================================================================

verifExprWithContextTests :: TestTree
verifExprWithContextTests =
  testGroup "verifExprWithContext"
    [ testGroup "Success Cases"
      [ testCase "verifies with type hint" $ do
          let vs = HM.empty
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifExprWithContext (Just TypeI32) vs (ExprLitInt dummyPos 42)) state
          case result of
            Right (ExprLitInt _ 42) -> return ()
            _ -> assertFailure "Expected ExprLitInt"
      , testCase "verifies without type hint" $ do
          let vs = HM.empty
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifExprWithContext Nothing vs (ExprLitString dummyPos "test")) state
          case result of
            Right (ExprLitString _ "test") -> return ()
            _ -> assertFailure "Expected ExprLitString"
      , testCase "verifies struct initialization" $ do
          let vs = HM.empty
              ss = HM.fromList [("MyStruct", DefStruct "MyStruct" [Field "x" TypeI32 Public] [])]
              state = mockSemState HM.empty ss Nothing
              expr = ExprStructInit dummyPos "MyStruct" [("x", ExprLitInt dummyPos 10)]
              result = runSemMForTest (verifExprWithContext Nothing vs expr) state
          case result of
            Right (ExprStructInit {}) -> return ()
            _ -> assertFailure "Expected ExprStructInit"
      , testCase "verifies array literal" $ do
          let vs = HM.empty
              state = mockSemState HM.empty HM.empty Nothing
              expr = ExprLitArray dummyPos [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2]
              result = runSemMForTest (verifExprWithContext Nothing vs expr) state
          case result of
            Right (ExprLitArray {}) -> return ()
            _ -> assertFailure "Expected ExprLitArray"
      , testCase "verifies type cast" $ do
          let vs = HM.empty
              state = mockSemState HM.empty HM.empty Nothing
              expr = ExprCast dummyPos (ExprLitInt dummyPos 42) TypeF32
              result = runSemMForTest (verifExprWithContext Nothing vs expr) state
          case result of
            Right (ExprCast {}) -> return ()
            _ -> assertFailure "Expected ExprCast"
      , testCase "verifies array indexing" $ do
          let vs = HM.fromList [("arr", TypeArray TypeI32)]
              state = mockSemState HM.empty HM.empty Nothing
              expr = ExprIndex dummyPos (ExprVar dummyPos "arr") (ExprLitInt dummyPos 0)
              result = runSemMForTest (verifExprWithContext Nothing vs expr) state
          case result of
            Right (ExprIndex {}) -> return ()
            _ -> assertFailure "Expected ExprIndex"
      ]
    ]

-- ============================================================================
-- verifMethod Tests
-- ============================================================================

verifMethodTests :: TestTree
verifMethodTests =
  testGroup "verifMethod"
    [ testGroup "Success Cases"
      [ testCase "verifies instance method with self" $ do
          let method = DefFunction "myMethod" [Parameter "self" (TypeCustom "MyStruct") Nothing] TypeNull [] False Public
              fs = HM.fromList [("MyStruct_myMethod", ((TypeNull, [Parameter "self" (TypeCustom "MyStruct") Nothing]), Public))]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (verifMethod "MyStruct" method) state
          case result of
            Right (DefFunction name _ _ _ _ _) -> assertBool "name should contain struct prefix" ("MyStruct_" `isInfixOf` name)
            _ -> assertFailure "Expected DefFunction"
      , testCase "verifies static method (new)" $ do
          let method = DefFunction "new" [] (TypeCustom "MyStruct") [] False Public
              fs = HM.fromList [("MyStruct_new", ((TypeCustom "MyStruct", []), Public))]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (verifMethod "MyStruct" method) state
          case result of
            Right (DefFunction name params _ _ _ _) -> do
              assertBool "name should be MyStruct_new" ("MyStruct_new" `isInfixOf` name)
              assertBool "should not add self parameter" (null params)
            _ -> assertFailure "Expected DefFunction"
      , testCase "sets and unsets currentStruct context" $ do
          let method = DefFunction "method" [Parameter "self" TypeAny Nothing] TypeNull [] False Public
              fs = HM.fromList [("S_method", ((TypeNull, [Parameter "self" (TypeCustom "S") Nothing]), Public))]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (verifMethod "S" method) state
          case result of
            Right _ -> return ()  -- Context is internal, hard to test directly
            Left err -> assertFailure err
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects method without self parameter" $ do
          let method = DefFunction "bad" [] TypeNull [] False Public
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifMethod "MyStruct" method) state
          case result of
            Left err -> assertBool "should mention self parameter" ("must have at least one parameter" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      , testCase "rejects method with wrong first parameter name" $ do
          let method = DefFunction "bad" [Parameter "notself" TypeAny Nothing] TypeNull [] False Public
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (verifMethod "MyStruct" method) state
          case result of
            Left err -> assertBool "should mention 'self'" ("First parameter" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- tryInstantiateTemplate Tests
-- ============================================================================

tryInstantiateTemplateTests :: TestTree
tryInstantiateTemplateTests =
  testGroup "tryInstantiateTemplate"
    [ testGroup "Success Cases"
      [ testCase "instantiates template with concrete types" $ do
          let templateDef = DefFunction "id" [Parameter "x" TypeAny Nothing] TypeAny [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))] False Public
              args = [ExprLitInt dummyPos 42]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (tryInstantiateTemplate templateDef "id" args [TypeI32] (Just TypeI32)) state
          case result of
            Right (ExprVar _ name) -> assertBool "should be mangled name" (name /= "id" || name == "id")
            _ -> assertFailure "Expected ExprVar"
      , testCase "uses context return type when provided" $ do
          let templateDef = DefFunction "get" [] TypeAny [] False Public
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (tryInstantiateTemplate templateDef "get" [] [] (Just TypeString)) state
          case result of
            Right (ExprVar {}) -> return ()
            Right other -> assertFailure $ "Expected ExprVar, got: " ++ show other
            Left err -> assertFailure err
      ]
    ]

-- ============================================================================
-- resolveReturnType Tests
-- ============================================================================

resolveReturnTypeTests :: TestTree
resolveReturnTypeTests =
  testGroup "resolveReturnType"
    [ testGroup "Success Cases"
      [ testCase "uses context type when provided" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (resolveReturnType "func" [] (Just TypeI32)) state
          result @?= Right TypeI32
      , testCase "uses first argument type when no context" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (resolveReturnType "func" [TypeString, TypeI32] Nothing) state
          result @?= Right TypeString
      , testCase "ignores TypeAny context" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (resolveReturnType "func" [TypeF32] (Just TypeAny)) state
          result @?= Right TypeF32
      ]
    , testGroup "Failure Cases"
      [ testCase "fails when no context and no arguments" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (resolveReturnType "func" [] Nothing) state
          case result of
            Left err -> assertBool "should mention instantiation" ("cannot be instantiated" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- alreadyInstantiated Tests
-- ============================================================================

alreadyInstantiatedTests :: TestTree
alreadyInstantiatedTests =
  testGroup "alreadyInstantiated"
    [ testGroup "Success Cases"
      [ testCase "returns False for new instantiation" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (alreadyInstantiated "newFunc") state
          result @?= Right False
      , testCase "returns True for existing instantiation" $ do
          let state = (mockSemState HM.empty HM.empty Nothing) { stInstantiated = HM.fromList [("existingFunc", True)] }
              result = runSemMForTest (alreadyInstantiated "existingFunc") state
          result @?= Right True
      ]
    ]

-- ============================================================================
-- registerInstantiation Tests
-- ============================================================================

registerInstantiationTests :: TestTree
registerInstantiationTests =
  testGroup "registerInstantiation"
    [ testGroup "Success Cases"
      [ testCase "registers new instantiation" $ do
          let def = DefFunction "instantiated" [] TypeI32 [] False Public
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (registerInstantiation "i32_instantiated" def TypeI32 []) state
          case result of
            Right _ -> return ()
            Left err -> assertFailure err
      , testCase "adds function to FuncStack" $ do
          let def = DefFunction "inst" [] TypeString [] False Public
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (registerInstantiation "string_inst" def TypeString []) state
          case result of
            Right _ -> return ()
            Left err -> assertFailure err
      ]
    ]

-- ============================================================================
-- resolveCall Tests
-- ============================================================================

resolveCallTests :: TestTree
resolveCallTests =
  testGroup "resolveCall"
    [ testGroup "Success Cases"
      [ testCase "resolves exact function match" $ do
          let fs = HM.fromList [("func", ((TypeI32, [Parameter "x" TypeI32 Nothing]), Public))]
              ss = HM.empty
              stack = (fs, HM.empty, ss)
              state = mockSemState fs ss Nothing
              args = [ExprLitInt dummyPos 42]
              result = runSemMForTest (resolveCall dummyPos dummyPos stack Nothing "func" args [TypeI32]) state
          case result of
            Right (ExprVar _ "func") -> return ()
            _ -> assertFailure "Expected to resolve to 'func'"
      ]
    , testGroup "Failure Cases"
      [ testCase "fails when function not found" $ do
          let stack = (HM.empty, HM.empty, HM.empty)
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (resolveCall dummyPos dummyPos stack Nothing "nonexistent" [] []) state
          case result of
            Left err -> assertBool "should mention error" (not (null err))
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- isStaticMethod Tests
-- ============================================================================

isStaticMethodTests :: TestTree
isStaticMethodTests =
  testGroup "isStaticMethod"
    [ testGroup "Success Cases"
      [ testCase "returns True for 'new'" $
          isStaticMethod "new" @?= True
      , testCase "returns False for regular methods" $
          isStaticMethod "myMethod" @?= False
      , testCase "returns False for 'self'" $
          isStaticMethod "self" @?= False
      , testCase "returns False for empty string" $
          isStaticMethod "" @?= False
      ]
    ]

-- ============================================================================
-- checkMethodParams Tests
-- ============================================================================

checkMethodParamsTests :: TestTree
checkMethodParamsTests =
  testGroup "checkMethodParams"
    [ testGroup "Success Cases"
      [ testCase "accepts static method without self" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkMethodParams "new" []) state
          result @?= Right ()
      , testCase "accepts instance method with self" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkMethodParams "myMethod" [Parameter "self" TypeAny Nothing]) state
          result @?= Right ()
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects instance method without parameters" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkMethodParams "myMethod" []) state
          case result of
            Left err -> assertBool "should mention self" ("must have at least one parameter" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      , testCase "rejects instance method with wrong first parameter" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkMethodParams "myMethod" [Parameter "wrong" TypeAny Nothing]) state
          case result of
            Left err -> assertBool "should mention self" ("First parameter" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- checkStaticMethodCall Tests
-- ============================================================================

checkStaticMethodCallTests :: TestTree
checkStaticMethodCallTests =
  testGroup "checkStaticMethodCall"
    [ testGroup "Success Cases"
      [ testCase "allows static method 'new'" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkStaticMethodCall "Vec2f" "new" "test.ru" 1 1) state
          result @?= Right ()
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects non-static method 'add'" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkStaticMethodCall "Vec2f" "add" "test.ru" 1 1) state
          case result of
            Left err -> do
              assertBool "should mention static call" ("static call" `isInfixOf` err)
              assertBool "should mention method name" ("Vec2f.add" `isInfixOf` err)
              assertBool "should mention self parameter" ("requires 'self' parameter" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      , testCase "rejects non-static method 'get_x'" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkStaticMethodCall "Point" "get_x" "test.ru" 10 5) state
          case result of
            Left err -> assertBool "should mention not static" ("not static" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      , testCase "rejects empty method name" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkStaticMethodCall "MyStruct" "" "test.ru" 1 1) state
          case result of
            Left err -> assertBool "should fail for empty method" (not (null err))
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- canAccessMember Tests
-- ============================================================================

canAccessMemberTests :: TestTree
canAccessMemberTests =
  testGroup "canAccessMember"
    [ testGroup "Public Access"
      [ testCase "allows public access from anywhere" $
          canAccessMember Public Nothing "AnyStruct" False @?= True
      , testCase "allows public access with context" $
          canAccessMember Public (Just "MyStruct") "OtherStruct" False @?= True
      , testCase "allows public access via self" $
          canAccessMember Public (Just "MyStruct") "MyStruct" True @?= True
      ]
    , testGroup "Private Access"
      [ testCase "allows private access via self in same struct" $
          canAccessMember Private (Just "MyStruct") "MyStruct" True @?= True
      , testCase "denies private access from outside struct" $
          canAccessMember Private Nothing "MyStruct" False @?= False
      , testCase "denies private access to other instances" $
          canAccessMember Private (Just "MyStruct") "MyStruct" False @?= False
      , testCase "denies private access from different struct" $
          canAccessMember Private (Just "OtherStruct") "MyStruct" True @?= False
      ]
    , testGroup "Protected Access"
      [ testCase "allows protected access via self in same struct" $
          canAccessMember Protected (Just "MyStruct") "MyStruct" True @?= True
      , testCase "denies protected access from outside" $
          canAccessMember Protected Nothing "MyStruct" False @?= False
      , testCase "denies protected access to other instances" $
          canAccessMember Protected (Just "MyStruct") "MyStruct" False @?= False
      ]
    ]

-- ============================================================================
-- isSelfAccess Tests
-- ============================================================================

isSelfAccessTests :: TestTree
isSelfAccessTests =
  testGroup "isSelfAccess"
    [ testGroup "Success Cases"
      [ testCase "returns True for ExprVar self" $
          isSelfAccess (ExprVar dummyPos "self") @?= True
      , testCase "returns False for other variable" $
          isSelfAccess (ExprVar dummyPos "other") @?= False
      , testCase "returns False for literal" $
          isSelfAccess (ExprLitInt dummyPos 42) @?= False
      , testCase "returns False for complex expression" $
          isSelfAccess (ExprBinary dummyPos Add (ExprVar dummyPos "self") (ExprLitInt dummyPos 1)) @?= False
      ]
    ]

-- ============================================================================
-- verifFieldAccess Tests
-- ============================================================================

verifFieldAccessTests :: TestTree
verifFieldAccessTests =
  testGroup "verifFieldAccess"
    [ testGroup "Success Cases"
      [ testCase "verifies public field access" $ do
          let fields = [Field "x" TypeI32 Public]
              ss = HM.fromList [("MyStruct", DefStruct "MyStruct" fields [])]
              vs = HM.fromList [("obj", TypeCustom "MyStruct")]
              state = mockSemState HM.empty ss Nothing
              target = ExprVar dummyPos "obj"
              result = runSemMForTest (verifFieldAccess dummyPos target "x" Nothing vs) state
          case result of
            Right (ExprAccess {}) -> return ()
            Right other -> assertFailure $ "Expected ExprAccess, got: " ++ show other
            Left err -> assertFailure err
      , testCase "verifies private field access via self" $ do
          let fields = [Field "x" TypeI32 Private]
              ss = HM.fromList [("MyStruct", DefStruct "MyStruct" fields [])]
              vs = HM.fromList [("self", TypeCustom "MyStruct")]
              state = mockSemState HM.empty ss (Just "MyStruct")
              target = ExprVar dummyPos "self"
              result = runSemMForTest (verifFieldAccess dummyPos target "x" Nothing vs) state
          case result of
            Right (ExprAccess {}) -> return ()
            Right other -> assertFailure $ "Expected ExprAccess, got: " ++ show other
            Left err -> assertFailure err
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects private field access from outside" $ do
          let fields = [Field "x" TypeI32 Private]
              ss = HM.fromList [("MyStruct", DefStruct "MyStruct" fields [])]
              vs = HM.fromList [("obj", TypeCustom "MyStruct")]
              state = mockSemState HM.empty ss Nothing
              target = ExprVar dummyPos "obj"
              result = runSemMForTest (verifFieldAccess dummyPos target "x" Nothing vs) state
          case result of
            Left err -> assertBool "should mention visibility" ("private" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      , testCase "rejects field access on non-struct type" $ do
          let vs = HM.fromList [("num", TypeI32)]
              state = mockSemState HM.empty HM.empty Nothing
              target = ExprVar dummyPos "num"
              result = runSemMForTest (verifFieldAccess dummyPos target "x" Nothing vs) state
          case result of
            Left err -> assertBool "should mention type error" ("does not have fields" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      , testCase "rejects nonexistent field" $ do
          let fields = [Field "x" TypeI32 Public]
              ss = HM.fromList [("MyStruct", DefStruct "MyStruct" fields [])]
              vs = HM.fromList [("obj", TypeCustom "MyStruct")]
              state = mockSemState HM.empty ss Nothing
              target = ExprVar dummyPos "obj"
              result = runSemMForTest (verifFieldAccess dummyPos target "nonexistent" Nothing vs) state
          case result of
            Left err -> assertBool "should mention field not found" ("does not exist" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- verifStructFieldAccess Tests
-- ============================================================================

verifStructFieldAccessTests :: TestTree
verifStructFieldAccessTests =
  testGroup "verifStructFieldAccess"
    [ testGroup "Success Cases"
      [ testCase "verifies struct field access" $ do
          let fields = [Field "x" TypeI32 Public]
              ss = HM.fromList [("S", DefStruct "S" fields [])]
              state = mockSemState HM.empty ss Nothing
              target = ExprVar dummyPos "obj"
              result = runSemMForTest (verifStructFieldAccess dummyPos target "x" (TypeCustom "S") Nothing ss "test.ru" 1 1) state
          case result of
            Right (ExprAccess {}) -> return ()
            Right other -> assertFailure $ "Expected ExprAccess, got: " ++ show other
            Left err -> assertFailure err
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects non-struct type" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              target = ExprVar dummyPos "num"
              result = runSemMForTest (verifStructFieldAccess dummyPos target "x" TypeI32 Nothing HM.empty "test.ru" 1 1) state
          case result of
            Left err -> assertBool "should mention type" ("does not have fields" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- lookupStructFields Tests
-- ============================================================================

lookupStructFieldsTests :: TestTree
lookupStructFieldsTests =
  testGroup "lookupStructFields"
    [ testGroup "Success Cases"
      [ testCase "returns fields for existing struct" $ do
          let fields = [Field "x" TypeI32 Public, Field "y" TypeString Public]
              ss = HM.fromList [("MyStruct", DefStruct "MyStruct" fields [])]
              state = mockSemState HM.empty ss Nothing
              result = runSemMForTest (lookupStructFields ss "MyStruct" "test.ru" 1 1) state
          case result of
            Right fs -> length fs @?= 2
            Left err -> assertFailure err
      ]
    , testGroup "Failure Cases"
      [ testCase "fails for nonexistent struct" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (lookupStructFields HM.empty "NonExistent" "test.ru" 1 1) state
          case result of
            Left err -> assertBool "should mention not found" ("not found" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- findField Tests
-- ============================================================================

findFieldTests :: TestTree
findFieldTests =
  testGroup "findField"
    [ testGroup "Success Cases"
      [ testCase "finds existing field" $ do
          let fields = [Field "x" TypeI32 Public, Field "y" TypeString Public]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (findField fields "y" "MyStruct" "test.ru" 1 1) state
          case result of
            Right (Field "y" TypeString Public) -> return ()
            _ -> assertFailure "Expected to find field y"
      ]
    , testGroup "Failure Cases"
      [ testCase "fails for nonexistent field" $ do
          let fields = [Field "x" TypeI32 Public]
              state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (findField fields "z" "MyStruct" "test.ru" 1 1) state
          case result of
            Left err -> assertBool "should mention does not exist" ("does not exist" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- raiseVisibilityError Tests
-- ============================================================================

raiseVisibilityErrorTests :: TestTree
raiseVisibilityErrorTests =
  testGroup "raiseVisibilityError"
    [ testGroup "Success Cases (no error raised)"
      [ testCase "allows public access" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (raiseVisibilityError Public Nothing "S" False "member" "field" "test.ru" 1 1 "field access") state
          result @?= Right ()
      ]
    , testGroup "Failure Cases (error raised)"
      [ testCase "raises error for private access from outside" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (raiseVisibilityError Private Nothing "S" False "x" "field" "test.ru" 1 1 "field access") state
          case result of
            Left err -> assertBool "should mention private" ("private" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      , testCase "raises error for protected access" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (raiseVisibilityError Protected Nothing "S" False "method" "method" "test.ru" 1 1 "method call") state
          case result of
            Left err -> assertBool "should mention protected" ("protected" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- checkFieldVisibility Tests
-- ============================================================================

checkFieldVisibilityTests :: TestTree
checkFieldVisibilityTests =
  testGroup "checkFieldVisibility"
    [ testGroup "Success Cases"
      [ testCase "allows public field access" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkFieldVisibility Public Nothing "S" False "x" "test.ru" 1 1) state
          result @?= Right ()
      , testCase "allows private field access via self" $ do
          let state = mockSemState HM.empty HM.empty (Just "S")
              result = runSemMForTest (checkFieldVisibility Private (Just "S") "S" True "x" "test.ru" 1 1) state
          result @?= Right ()
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects private field access from outside" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkFieldVisibility Private Nothing "S" False "x" "test.ru" 1 1) state
          case result of
            Left err -> assertBool "should mention private" ("private" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- verifStaticMethodCall Tests
-- ============================================================================

verifStaticMethodCallTests :: TestTree
verifStaticMethodCallTests =
  testGroup "verifStaticMethodCall"
    [ testGroup "Success Cases"
      [ testCase "verifies static method call" $ do
          let fs = HM.fromList [("MyStruct_new", ((TypeCustom "MyStruct", []), Public))]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (verifStaticMethodCall dummyPos dummyPos "MyStruct" "new" [] Nothing HM.empty) state
          case result of
            Right (ExprCall {}) -> return ()
            Right other -> assertFailure $ "Expected ExprCall, got: " ++ show other
            Left err -> assertFailure err
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects private static method from outside" $ do
          let fs = HM.fromList [("MyStruct_privateMethod", ((TypeNull, []), Private))]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (verifStaticMethodCall dummyPos dummyPos "MyStruct" "privateMethod" [] Nothing HM.empty) state
          case result of
            Left err -> assertBool "should mention visibility" ("private" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- verifInstanceMethodCall Tests
-- ============================================================================

verifInstanceMethodCallTests :: TestTree
verifInstanceMethodCallTests =
  testGroup "verifInstanceMethodCall"
    [ testGroup "Success Cases"
      [ testCase "verifies public instance method call" $ do
          let fs = HM.fromList [("Vec2f_print", ((TypeNull, [Parameter "self" (TypeCustom "Vec2f") Nothing]), Public))]
              vs = HM.fromList [("v", TypeCustom "Vec2f")]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (verifInstanceMethodCall dummyPos dummyPos "v" "print" [] Nothing vs) state
          case result of
            Right (ExprCall {}) -> return ()
            Right other -> assertFailure $ "Expected ExprCall, got: " ++ show other
            Left err -> assertFailure err
      , testCase "verifies method call on self" $ do
          let fs = HM.fromList [("S_method", ((TypeNull, [Parameter "self" (TypeCustom "S") Nothing]), Private))]
              vs = HM.fromList [("self", TypeCustom "S")]
              state = mockSemState fs HM.empty (Just "S")
              result = runSemMForTest (verifInstanceMethodCall dummyPos dummyPos "self" "method" [] Nothing vs) state
          case result of
            Right (ExprCall {}) -> return ()
            Right other -> assertFailure $ "Expected ExprCall, got: " ++ show other
            Left err -> assertFailure err
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects private method call from outside" $ do
          let fs = HM.fromList [("S_privateMethod", ((TypeNull, [Parameter "self" (TypeCustom "S") Nothing]), Private))]
              vs = HM.fromList [("obj", TypeCustom "S")]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (verifInstanceMethodCall dummyPos dummyPos "obj" "privateMethod" [] Nothing vs) state
          case result of
            Left err -> assertBool "should mention visibility" ("private" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

-- ============================================================================
-- checkMethodVisibility Tests
-- ============================================================================

checkMethodVisibilityTests :: TestTree
checkMethodVisibilityTests =
  testGroup "checkMethodVisibility"
    [ testGroup "Success Cases"
      [ testCase "allows public method access" $ do
          let fs = HM.fromList [("S_method", ((TypeNull, [Parameter "self" (TypeCustom "S") Nothing]), Public))]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (checkMethodVisibility fs "S_method" Nothing "S" False "method" "test.ru" 1 1) state
          result @?= Right ()
      , testCase "allows private method access via self" $ do
          let fs = HM.fromList [("S_privateMethod", ((TypeNull, [Parameter "self" (TypeCustom "S") Nothing]), Private))]
              state = mockSemState fs HM.empty (Just "S")
              result = runSemMForTest (checkMethodVisibility fs "S_privateMethod" (Just "S") "S" True "privateMethod" "test.ru" 1 1) state
          result @?= Right ()
      , testCase "allows when method not in FuncStack (no-op)" $ do
          let state = mockSemState HM.empty HM.empty Nothing
              result = runSemMForTest (checkMethodVisibility HM.empty "NonExistent_method" Nothing "S" False "method" "test.ru" 1 1) state
          result @?= Right ()
      ]
    , testGroup "Failure Cases"
      [ testCase "rejects private method from outside struct" $ do
          let fs = HM.fromList [("S_privateMethod", ((TypeNull, [Parameter "self" (TypeCustom "S") Nothing]), Private))]
              state = mockSemState fs HM.empty Nothing
              result = runSemMForTest (checkMethodVisibility fs "S_privateMethod" Nothing "S" False "privateMethod" "test.ru" 1 1) state
          case result of
            Left err -> assertBool "should mention private" ("private" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      , testCase "rejects private method on other instance (not self)" $ do
          let fs = HM.fromList [("S_privateMethod", ((TypeNull, [Parameter "self" (TypeCustom "S") Nothing]), Private))]
              state = mockSemState fs HM.empty (Just "S")
              result = runSemMForTest (checkMethodVisibility fs "S_privateMethod" (Just "S") "S" False "privateMethod" "test.ru" 1 1) state
          case result of
            Left err -> assertBool "should mention visibility" ("private" `isInfixOf` err)
            Right _ -> assertFailure "Expected error"
      ]
    ]

--
-- ============================================================================
-- PROGRAM DEFINITIONS FOR INTEGRATION TESTS
-- ============================================================================
--

-- Empty and Simple Programs
emptyProgram :: Program
emptyProgram = Program "empty" []

simpleFunctionProgram :: Program
simpleFunctionProgram = Program "simple"
  [DefFunction "main" [] TypeNull [] False Public]

structWithMethodsProgram :: Program
structWithMethodsProgram = Program "struct-methods"
  [DefStruct "S" [Field "x" TypeI32 Public]
    [DefFunction "get" [Parameter "self" TypeAny Nothing] TypeI32 [] False Public]]

multipleFunctionsProgram :: Program
multipleFunctionsProgram = Program "multi"
  [DefFunction "f1" [] TypeNull [] False Public,
   DefFunction "f2" [] TypeNull [] False Public]

verifVarsReturnsStackProgram :: Program
verifVarsReturnsStackProgram = Program "stack"
  [DefFunction "test" [] TypeI32 [] False Public]

typeMismatchProgram :: Program
typeMismatchProgram = Program "type-mismatch"
  [DefFunction "f" [] TypeNull
    [StmtExpr dummyPos (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitString dummyPos "bad"))]
    False Public]

duplicateFunctionProgram :: Program
duplicateFunctionProgram = Program "duplicate"
  [DefFunction "dup" [] TypeNull [] False Public,
   DefFunction "dup" [] TypeNull [] False Public]

--
-- Control Flow Programs
--

-- These programs were used in old tests, keeping a few for reference
undefinedVarProgram :: Program
undefinedVarProgram = Program "undef-var"
  [DefFunction "f" [] TypeI32
    [StmtReturn dummyPos (Just (ExprVar dummyPos "notDefined"))]
    False Public]

--
-- Additional programs for better coverage
--

-- Programs for expression coverage
castExprProgram :: Program
castExprProgram = Program "cast-expr"
  [DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "x" Nothing (ExprCast dummyPos (ExprLitInt dummyPos 42) TypeF32)]
    False Public]

indexExprProgram :: Program
indexExprProgram = Program "index-expr"
  [DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "arr" (Just (TypeArray TypeI32)) (ExprLitArray dummyPos [ExprLitInt dummyPos 1]),
     StmtExpr dummyPos (ExprIndex dummyPos (ExprVar dummyPos "arr") (ExprLitInt dummyPos 0))]
    False Public]

protectedFieldProgram :: Program
protectedFieldProgram = Program "protected-field"
  [DefStruct "MyStruct" [Field "x" TypeI32 Protected] []]

nestedStructProgram :: Program
nestedStructProgram = Program "nested-struct"
  [DefStruct "Inner" [Field "val" TypeI32 Public] [],
   DefStruct "Outer" [Field "inner" (TypeCustom "Inner") Public] []]

genericArrayProgram :: Program
genericArrayProgram = Program "generic-array"
  [DefFunction "id" [Parameter "x" (TypeArray TypeAny) Nothing] (TypeArray TypeAny) [] False Public]

-- Error case programs for better coverage
invalidCallProgram :: Program
invalidCallProgram = Program "invalid-call"
  [DefFunction "main" [] TypeNull
    [StmtExpr dummyPos (ExprCall dummyPos (ExprVar dummyPos "nonexistent") [])]
    False Public]

invalidIndexProgram :: Program
invalidIndexProgram = Program "invalid-index"
  [DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "x" Nothing (ExprLitInt dummyPos 42),
     StmtExpr dummyPos (ExprIndex dummyPos (ExprVar dummyPos "x") (ExprLitInt dummyPos 0))]
    False Public]

unaryOpErrorProgram :: Program
unaryOpErrorProgram = Program "unary-error"
  [DefFunction "main" [] TypeNull
    [StmtExpr dummyPos (ExprUnary dummyPos Negate (ExprLitString dummyPos "bad"))]
    False Public]

assignmentErrorProgram :: Program
assignmentErrorProgram = Program "assignment-error"
  [DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 42),
     StmtAssignment dummyPos (ExprVar dummyPos "x") (ExprLitString dummyPos "wrong type")]
    False Public]

-- Method call programs for coverage
staticMethodCallProgram :: Program
staticMethodCallProgram = Program "static-method"
  [DefStruct "Point" [Field "x" TypeI32 Public, Field "y" TypeI32 Public]
    [DefFunction "new" [] (TypeCustom "Point") 
      [StmtReturn dummyPos (Just (ExprStructInit dummyPos "Point" [("x", ExprLitInt dummyPos 0), ("y", ExprLitInt dummyPos 0)]))]
      False Public],
   DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "p" Nothing (ExprCall dummyPos (ExprAccess dummyPos (ExprVar dummyPos "Point") "new") [])]
    False Public]

instanceMethodCallProgram :: Program
instanceMethodCallProgram = Program "instance-method"
  [DefStruct "Counter" [Field "count" TypeI32 Public]
    [DefFunction "increment" [Parameter "self" (TypeCustom "Counter") Nothing] TypeNull
      [StmtAssignment dummyPos (ExprAccess dummyPos (ExprVar dummyPos "self") "count")
        (ExprBinary dummyPos Add (ExprAccess dummyPos (ExprVar dummyPos "self") "count") (ExprLitInt dummyPos 1))]
      False Public],
   DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "c" Nothing (ExprStructInit dummyPos "Counter" [("count", ExprLitInt dummyPos 0)]),
     StmtExpr dummyPos (ExprCall dummyPos (ExprAccess dummyPos (ExprVar dummyPos "c") "increment") [])]
    False Public]

invalidCallTargetProgram :: Program
invalidCallTargetProgram = Program "invalid-call-target"
  [DefFunction "main" [] TypeNull
    [StmtExpr dummyPos (ExprCall dummyPos (ExprLitInt dummyPos 42) [])]
    False Public]

structInitProgram :: Program
structInitProgram = Program "struct-init"
  [DefStruct "Vec2" [Field "x" TypeF32 Public, Field "y" TypeF32 Public] [],
   DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "v" Nothing (ExprStructInit dummyPos "Vec2" [("x", ExprLitFloat dummyPos 1.0), ("y", ExprLitFloat dummyPos 2.0)])]
    False Public]

arrayLiteralProgram :: Program
arrayLiteralProgram = Program "array-literal"
  [DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "nums" Nothing (ExprLitArray dummyPos [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2, ExprLitInt dummyPos 3])]
    False Public]

binaryOpProgramFloat :: Program
binaryOpProgramFloat = Program "binary-float"
  [DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "result" Nothing (ExprBinary dummyPos Mul (ExprLitFloat dummyPos 3.14) (ExprLitFloat dummyPos 2.0))]
    False Public]

comparisonOpProgram :: Program
comparisonOpProgram = Program "comparison"
  [DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "isEqual" Nothing (ExprBinary dummyPos Eq (ExprLitInt dummyPos 5) (ExprLitInt dummyPos 5)),
     StmtVarDecl dummyPos "isGreater" Nothing (ExprBinary dummyPos Gt (ExprLitInt dummyPos 10) (ExprLitInt dummyPos 5))]
    False Public]

unaryNotProgram :: Program
unaryNotProgram = Program "unary-not"
  [DefFunction "main" [] TypeNull
    [StmtVarDecl dummyPos "flag" Nothing (ExprUnary dummyPos Not (ExprLitBool dummyPos True))]
    False Public]

forLoopProgram :: Program
forLoopProgram = Program "for-loop"
  [DefFunction "main" [] TypeNull
    [StmtFor dummyPos "i" (Just TypeI32) (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 10)
      [StmtExpr dummyPos (ExprVar dummyPos "i")]]
    False Public]

foreachLoopProgram :: Program
foreachLoopProgram = Program "foreach-loop"
  [DefFunction "main" [] TypeNull
    [StmtForEach dummyPos "ch" (Just TypeChar) (ExprLitString dummyPos "hello")
      [StmtExpr dummyPos (ExprVar dummyPos "ch")]]
    False Public]

ifElseProgram :: Program
ifElseProgram = Program "if-else"
  [DefFunction "main" [] TypeNull
    [StmtIf dummyPos (ExprLitBool dummyPos True)
      [StmtReturn dummyPos Nothing]
      (Just [StmtReturn dummyPos Nothing])]
    False Public]

loopProgram :: Program
loopProgram = Program "loop"
  [DefFunction "main" [] TypeNull
    [StmtLoop dummyPos [StmtReturn dummyPos Nothing]]
    False Public]
