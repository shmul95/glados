{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
#define TESTING_EXPORT

module IR.Generator.Expression.StructSpecs (structExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Control.Monad.State (runState)
import qualified Data.Map.Strict as Map
import Control.Exception (try, evaluate, SomeException)
import Rune.IR.Generator.Expression.Struct
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..), GenState(..))
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (runGen, emptyState)

--
-- public
--

structExprTests :: TestTree
structExprTests = testGroup "Rune.IR.Generator.Expression.Struct"
  [ testGenAccess
  , testGenStructInit
  , testResolveStructPtr
  , testLookupFieldType
  , testGenInitField
  ]

--
-- private
--

testGenAccess :: TestTree
testGenAccess = testGroup "genAccess"
  [ testCase "Generates field access for struct" $
      -- explanation
      -- genAccess should generate an IRGET_FIELD when given a struct value and a valid field, using a pointer temp if needed
      let genExpr (ExprVar "p") = return ([], IRTemp "p" (IRStruct "Point"), IRStruct "Point")
          genExpr _             = return ([], IRConstNull, IRNull)
          initialState =
            emptyState
              { gsStructs = Map.fromList [("Point", [("x", IRI32)])]
              }
          ((instrs, op, typ), _) =
            runState (genAccess genExpr (ExprVar "p") "x") initialState
      in do
        typ @?= IRI32
        assertBool "Should emit IRGET_FIELD" (any isGetField instrs)
        case op of
          IRTemp _ t -> t @?= IRI32
          _          -> assertFailure "Expected IRTemp result for field access"

  , testCase "Looks up field type" $
      -- explanation
      -- genAccess should also work when the target is already a pointer to a struct, without emitting an extra IRADDR
      let genExpr (ExprVar "p") = return ([], IRTemp "p" (IRPtr (IRStruct "Point")), IRPtr (IRStruct "Point"))
          genExpr _             = return ([], IRConstNull, IRNull)
          initialState =
            emptyState
              { gsStructs = Map.fromList [("Point", [("y", IRI64)])]
              }
          ((instrs, _, typ), _) =
            runState (genAccess genExpr (ExprVar "p") "y") initialState
      in do
        typ @?= IRI64
        assertBool "Should not emit IRADDR when operand is already a pointer"
          (not (any isAddr instrs))
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

  , testCase "Errors on non-struct type" $
      -- explanation
      -- resolveStructPtr should fail with an informative error when used on a non-struct type
      do
        result <- try @SomeException (evaluate (resolveStructPtr (IRTemp "n" IRI32) IRI32))
        case result of
          Left _  -> return ()
          Right _ -> assertFailure "Expected error for resolveStructPtr on non-struct type"
  ]

testLookupFieldType :: TestTree
testLookupFieldType = testGroup "lookupFieldType"
  [ testCase "Returns field type when struct and field exist" $
      -- explanation
      -- lookupFieldType should consult gsStructs and return the concrete IRType for a known field
      let initial =
            emptyState
              { gsStructs = Map.fromList [("Point", [("x", IRI32), ("y", IRI32)])]
              }
          (t, _) = runState (lookupFieldType "Point" "x") initial
      in t @?= IRI32

  , testCase "Errors when struct is missing" $
      -- explanation
      -- lookupFieldType must raise an error if the struct name is not in gsStructs
      do
        let initial = emptyState
        result <- try @SomeException (evaluate (fst (runState (lookupFieldType "Missing" "x") initial)))
        case result of
          Left _  -> return ()
          Right _ -> assertFailure "Expected error for missing struct in lookupFieldType"

  , testCase "Errors when field is missing" $
      -- explanation
      -- lookupFieldType must raise an error if the struct exists but the field name does not
      do
        let initial =
              emptyState
                { gsStructs = Map.fromList [("Point", [("x", IRI32)])]
                }
        result <- try @SomeException (evaluate (fst (runState (lookupFieldType "Point" "y") initial)))
        case result of
          Left _  -> return ()
          Right _ -> assertFailure "Expected error for missing field in lookupFieldType"
  ]

testGenInitField :: TestTree
testGenInitField = testGroup "genInitField"
  [ testCase "Generates address and set-field instructions for struct init" $
      -- explanation
      -- genInitField should compute the struct address into a temp pointer and emit an IRSET_FIELD with the value operand
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _              = return ([], IRConstNull, IRNull)
          (instrs, _) =
            runState (genInitField genExpr "Point" "s0" (IRStruct "Point") ("x", ExprLitInt 42)) emptyState
      in case instrs of
        [IRADDR ptrName base (IRPtr sType), IRSET_FIELD (IRTemp ptrOpName (IRPtr sType')) "Point" "x" (IRConstInt 42)] -> do
          base @?= "s0"
          ptrName @?= ptrOpName
          sType @?= IRStruct "Point"
          sType' @?= IRStruct "Point"
        _ -> assertFailure "Expected IRADDR then IRSET_FIELD for genInitField"
  ]

--
-- helpers
--

isAlloc :: IRInstruction -> Bool
isAlloc (IRALLOC _ _) = True
isAlloc _ = False

isGetField :: IRInstruction -> Bool
isGetField (IRGET_FIELD _ _ _ _ _) = True
isGetField _ = False

isAddr :: IRInstruction -> Bool
isAddr (IRADDR _ _ _) = True
isAddr _ = False
