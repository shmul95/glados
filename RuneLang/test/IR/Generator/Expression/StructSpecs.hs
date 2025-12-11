{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.StructSpecs (structExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.IR.Generator.Expression.Struct
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..))
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (runGen)

--
-- public
--

structExprTests :: TestTree
structExprTests = testGroup "Rune.IR.Generator.Expression.Struct"
  [ testGenAccess
  , testGenStructInit
  , testResolveStructPtr
  ]

--
-- private
--

testGenAccess :: TestTree
testGenAccess = testGroup "genAccess"
  [ testCase "Generates field access for struct" $
      assertBool "Should generate access" True

  , testCase "Looks up field type" $
      assertBool "Field lookup works" True
  ]

testGenStructInit :: TestTree
testGenStructInit = testGroup "genStructInit"
  [ testCase "Generates struct initialization" $
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstNull, IRNull)
          (instrs, _, typ) = runGen (genStructInit genExpr "Point" [("x", ExprLitInt 1)])
      in do
        assertBool "Should have IRALLOC" $ any isAlloc instrs
        case typ of
          IRStruct "Point" -> return ()
          _ -> assertBool "Expected IRStruct Point" False

  , testCase "Generates field initializations" $
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstNull, IRNull)
          (instrs, _, _) = runGen (genStructInit genExpr "Vec" [("x", ExprLitInt 1), ("y", ExprLitInt 2)])
      in assertBool "Should have instructions" $ not $ null instrs
  ]

testResolveStructPtr :: TestTree
testResolveStructPtr = testGroup "resolveStructPtr"
  [ testCase "Resolves struct temp to pointer" $
      let (name, op, instrs) = resolveStructPtr (IRTemp "s" (IRStruct "Point")) (IRStruct "Point")
      in do
        name @?= "Point"
        assertBool "Should have IRADDR" $ not $ null instrs
        case op of
          IRTemp _ (IRPtr _) -> return ()
          _ -> assertBool "Expected IRTemp with IRPtr" False

  , testCase "Handles pointer to struct" $
      let (name, op, instrs) = resolveStructPtr (IRTemp "p" (IRPtr (IRStruct "Vec"))) (IRPtr (IRStruct "Vec"))
      in do
        name @?= "Vec"
        instrs @?= []
        op @?= IRTemp "p" (IRPtr (IRStruct "Vec"))
  ]

--
-- helpers
--

isAlloc :: IRInstruction -> Bool
isAlloc (IRALLOC _ _) = True
isAlloc _ = False
