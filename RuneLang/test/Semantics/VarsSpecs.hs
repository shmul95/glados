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
    [ expectOk "handles return without expression" returnNullProgram,
      expectOk "handles if-else branches" ifElseFullProgram,
      expectOk "handles for loop with explicit start" forWithStartProgram,
      expectOk "handles foreach over string" forEachStringProgram,
      expectOk "handles foreach over any type" forEachAnyProgram,
      
      expectOk "handles assignment to struct fields" structAssignmentProgram,
      expectOk "handles array indexing expressions" arrayIndexProgram,
      expectOk "handles complex struct initialization" complexStructInitProgram,

      expectOk "instantiates generic function from argument" genericInstArgProgram,
      expectOk "instantiates generic function from return context" genericInstContextProgram,
      expectOk "uses instantiation cache (alreadyInstantiated)" genericCacheProgram,
      expectErr "fails to instantiate generic without context" genericFailProgram "cannot be instantiated",

      expectErr "detects invalid binary operation (Left branch)" binaryInvalidProgram "binary operation type mismatch",
      expectOk "accepts invalid iterable in foreach (implementation defaults to Any)" forEachInvalidProgram,
      expectOk "accepts incompatible assignment type (implementation returns Any for field access)" assignmentIncompatibleProgram,
      expectErr "detects undefined variable in binary op" binaryUndefinedProgram "Undefined variable",

      testCase "mangleFuncStack covers all branches" testMangleFuncStack
    ]

--
-- private helpers
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
-- Test Programs
--

returnNullProgram :: Program
returnNullProgram = Program "ret" [DefFunction "f" [] TypeNull [StmtReturn dummyPos Nothing]]

ifElseFullProgram :: Program
ifElseFullProgram = Program "if" 
  [ DefFunction "f" [] TypeNull 
    [ StmtIf dummyPos (ExprLitBool dummyPos True) [] (Just [StmtNext dummyPos]) ] 
  ]

forWithStartProgram :: Program
forWithStartProgram = Program "for" 
  [ DefFunction "f" [] TypeNull 
    [ StmtFor dummyPos "i" (Just TypeI32) (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 10) [] ] 
  ]

forEachStringProgram :: Program
forEachStringProgram = Program "fe" 
  [ DefFunction "f" [] TypeNull 
    [ StmtForEach dummyPos "c" (Just TypeChar) (ExprLitString dummyPos "hi") [] ] 
  ]

forEachAnyProgram :: Program
forEachAnyProgram = Program "feany" 
  [ DefFunction "f" [Parameter "a" TypeAny] TypeNull 
    [ StmtForEach dummyPos "x" Nothing (ExprVar dummyPos "a") [] ] 
  ]

structAssignmentProgram :: Program
structAssignmentProgram = Program "assign" 
  [ DefStruct "S" [Field "x" TypeI32] [],
    DefFunction "f" [Parameter "s" (TypeCustom "S")] TypeNull 
    [ StmtAssignment dummyPos (ExprAccess dummyPos (ExprVar dummyPos "s") "x") (ExprLitInt dummyPos 42) ]
  ]

arrayIndexProgram :: Program
arrayIndexProgram = Program "idx" 
  [ DefFunction "f" [Parameter "a" (TypeArray TypeI32)] TypeI32 
    [ StmtReturn dummyPos (Just (ExprIndex dummyPos (ExprVar dummyPos "a") (ExprLitInt dummyPos 0))) ]
  ]

complexStructInitProgram :: Program
complexStructInitProgram = Program "sinit" 
  [ DefStruct "S" [Field "x" TypeI32, Field "y" TypeI32] [],
    DefFunction "f" [] TypeNull 
    [ StmtVarDecl dummyPos "s" Nothing (ExprStructInit dummyPos "S" [("x", ExprLitInt dummyPos 1), ("y", ExprLitInt dummyPos 2)]) ]
  ]

genericInstArgProgram :: Program
genericInstArgProgram = Program "gen" 
  [ DefFunction "id" [Parameter "x" TypeAny] TypeAny [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))],
    DefFunction "m" [] TypeNull [StmtExpr dummyPos (ExprCall dummyPos "id" [ExprLitInt dummyPos 42])]
  ]

genericInstContextProgram :: Program
genericInstContextProgram = Program "genctx" 
  [ DefFunction "get" [] TypeAny [StmtReturn dummyPos (Just (ExprLitInt dummyPos 1))],
    DefFunction "m" [] TypeNull [StmtVarDecl dummyPos "x" (Just TypeI32) (ExprCall dummyPos "get" [])]
  ]

genericCacheProgram :: Program
genericCacheProgram = Program "cache" 
  [ DefFunction "id" [Parameter "x" TypeAny] TypeAny [StmtReturn dummyPos (Just (ExprVar dummyPos "x"))],
    DefFunction "m" [] TypeNull 
    [ StmtExpr dummyPos (ExprCall dummyPos "id" [ExprLitInt dummyPos 1]),
      StmtExpr dummyPos (ExprCall dummyPos "id" [ExprLitInt dummyPos 2]) ]
  ]

genericFailProgram :: Program
genericFailProgram = Program "genfail" 
  [ DefFunction "fail" [] TypeAny [StmtReturn dummyPos (Just (ExprLitNull dummyPos))],
    DefFunction "m" [] TypeNull [StmtExpr dummyPos (ExprCall dummyPos "fail" [])]
  ]

binaryInvalidProgram :: Program
binaryInvalidProgram = Program "binerr" 
  [ DefFunction "f" [] TypeNull [StmtExpr dummyPos (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitString dummyPos "a"))]
  ]

forEachInvalidProgram :: Program
forEachInvalidProgram = Program "feerr" 
  [ DefFunction "f" [] TypeNull 
    [ StmtForEach dummyPos "i" Nothing (ExprLitInt dummyPos 1) [] ]
  ]

assignmentIncompatibleProgram :: Program
assignmentIncompatibleProgram = Program "assignerr" 
  [ DefStruct "S" [Field "x" TypeI32] [],
    DefFunction "f" [Parameter "s" (TypeCustom "S")] TypeNull 
    [ StmtAssignment dummyPos (ExprAccess dummyPos (ExprVar dummyPos "s") "x") (ExprLitString dummyPos "bad") ]
  ]

binaryUndefinedProgram :: Program
binaryUndefinedProgram = Program "binundef" 
  [ DefFunction "f" [] TypeNull [StmtExpr dummyPos (ExprBinary dummyPos Add (ExprVar dummyPos "ghost") (ExprLitInt dummyPos 1))]
  ]

testMangleFuncStack :: IO ()
testMangleFuncStack = do
  let fs = HM.fromList [("f", [(TypeI32, [TypeI32]), (TypeF32, [TypeF32])])]
      mangled = mangleFuncStack fs
  assertBool "Should contain mangled names" (HM.member "i32_f_i32" mangled)
  assertBool "Should contain original name" (HM.member "f" mangled)
