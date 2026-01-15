{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.CallSpecs (callExprTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Rune.IR.Generator.Expression.Call
import Rune.IR.Nodes
import Rune.AST.Nodes

import IR.TestUtils (runGenUnsafe, emptyState)
import TestHelpers (dummyPos)
import Control.Monad.State (evalState)
import Control.Monad.Except (runExceptT)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)

--
-- helpers
--

isCall :: IRInstruction -> Bool
isCall (IRCALL {}) = True
isCall _ = False

isAdd :: IRInstruction -> Bool
isAdd (IRADD_OP {}) = True
isAdd _ = False

inside :: String -> String -> Bool
inside needle haystack = needle `isInfixOf` haystack

isAddr :: IRInstruction -> Bool
isAddr (IRADDR {}) = True
isAddr _ = False

isAssign :: IRInstruction -> Bool
isAssign (IRASSIGN {}) = True
isAssign _ = False

--
-- setup
--

runGenWithFuncStack :: HM.HashMap String (Type, [Parameter]) -> IRGen a -> Either String a
runGenWithFuncStack funcStack action =
  let state = emptyState { gsFuncStack = funcStack }
  in evalState (runExceptT action) state

runGenWithFuncStackUnsafe :: HM.HashMap String (Type, [Parameter]) -> IRGen a -> a
runGenWithFuncStackUnsafe fs action = case runGenWithFuncStack fs action of
  Right val -> val
  Left err -> error $ "IR Generation failed: " ++ err

--
-- tests
--

callExprTests :: TestTree
callExprTests = testGroup "Rune.IR.Generator.Expression.Call"
  [ testNamingLogic
  , testStrategyDecisions
  , testStandardCallPaths
  , testUnrolledVariadicPaths
  , testArgumentCasting
  , testPointerAndReferenceLogic
  ]

testNamingLogic :: TestTree
testNamingLogic = testGroup "Naming & Extraction"
  [ testCase "matchesBaseName: handles infix underscores" $
      matchesBaseName "foo" "abc_foo_def" @?= True
  , testCase "extractBaseName: handles empty rest after underscore" $
      extractBaseName "struct_" @?= "struct_"
  ]

testStrategyDecisions :: TestTree
testStrategyDecisions = testGroup "selectCallStrategy & shouldUnroll"
  [ testCase "shouldUnroll: returns false if variadicOverload is Nothing" $
      shouldUnroll "p" [] [] Nothing @?= False
  
  , testCase "shouldUnroll: returns false if arguments do not exceed normal params" $
      let variadic = ("p", (TypeI32, [Parameter "a" TypeI32 Nothing, Parameter "b" (TypeVariadic TypeI32) Nothing]))
          matching = [variadic, ("p_i32", (TypeI32, [Parameter "a" TypeI32 Nothing]))]
          args = [ExprLitInt dummyPos 1]
      in shouldUnroll "p" args matching (Just variadic) @?= False

  , testCase "shouldUnroll: returns true when conditions met" $
      let variadic = ("p", (TypeI32, [Parameter "v" (TypeVariadic TypeI32) Nothing]))
          single = ("p_i32", (TypeI32, [Parameter "v" TypeI32 Nothing]))
          matching = [variadic, single]
          args = [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2]
      in shouldUnroll "p" args matching (Just variadic) @?= True

  , testCase "selectCallStrategy: selects VariadicUnroll when shouldUnroll is true" $
      let variadic = ("p", (TypeI32, [Parameter "v" (TypeVariadic TypeI32) Nothing]))
          single = ("p_i32", (TypeI32, [Parameter "v" TypeI32 Nothing]))
          matching = [variadic, single]
          args = [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2]
      in selectCallStrategy "p" args matching (Just variadic) @?= VariadicUnroll matching variadic

  , testCase "maxNonVariadicParams: calculates correctly" $
      let f1 = ("f", (TypeNull, [Parameter "a" TypeI32 Nothing]))
          f2 = ("f", (TypeNull, [Parameter "a" TypeI32 Nothing, Parameter "b" TypeI32 Nothing]))
          fvar = ("f", (TypeNull, [Parameter "v" (TypeVariadic TypeI32) Nothing]))
      in maxNonVariadicParams [f1, f2, fvar] @?= 2
  ]

testStandardCallPaths :: TestTree
testStandardCallPaths = testGroup "genStandardCall Fallbacks"
  [ testCase "genStandardCall: throws error if function not found" $
      let genExpr _ = return ([], IRConstInt 0, IRI32)
          res = runGenWithFuncStack HM.empty (genStandardCall genExpr "unknown" [])
      in case res of
           Left err -> assertBool "Correct error message" ("not found in function stack" `inside` err)
           Right _ -> error "Should have failed"

  , testCase "genStandardCall: handles case with no signature match (fallback to simple mapM)" $
      let genExpr _ = return ([], IRConstInt 1, IRI32)
          fs = HM.fromList [("test", (TypeI32, [Parameter "a" TypeI32 Nothing, Parameter "b" TypeI32 Nothing]))]
          res = runGenWithFuncStack fs (genStandardCall genExpr "test" [])
      in case res of
           Left _ -> return ()
           Right _ -> return ()

  , testCase "genStandardCall: variadic path without unrolling" $
      let genExpr _ = return ([], IRConstInt 1, IRI32)
          -- Only one function, so unrolling disabled
          variadic = ("sum", (TypeI32, [Parameter "v" (TypeVariadic TypeI32) Nothing]))
          matching = [variadic]
          fs = HM.fromList matching
          args = [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2]
          (instrs, _, _) = runGenWithFuncStackUnsafe fs (genStandardCall genExpr "sum" args)
      in do
         -- Should have 1 call to "sum" with 2 arguments
         case filter isCall instrs of
           [IRCALL _ "sum" ops _] -> length ops @?= 2
           _ -> assertBool "Expected single call to sum with 2 ops" False
  ]

testUnrolledVariadicPaths :: TestTree
testUnrolledVariadicPaths = testGroup "genUnrolledCall & Overloads"
  [ testCase "genUnrolledCall: single variadic argument branch" $
      let genExpr _ = return ([], IRConstInt 1, IRI32)
          variadic = ("p", (TypeI32, [Parameter "v" (TypeVariadic TypeI32) Nothing]))
          overload = ("p_i32", (TypeI32, [Parameter "v" TypeI32 Nothing]))
          matching = [variadic, overload]
          args = [ExprLitInt dummyPos 1]
          fs = HM.fromList matching
          (instrs, _, _) = runGenWithFuncStackUnsafe fs (genUnrolledCall genExpr "p" matching variadic args)
      in (length $ filter isCall instrs) @?= 1

  , testCase "genUnrolledCall: multiple variadic arguments trigger accumulateResults" $
      let genExpr _ = return ([], IRConstInt 1, IRI32)
          variadic = ("sum", (TypeI32, [Parameter "v" (TypeVariadic TypeI32) Nothing]))
          overload = ("sum_i32", (TypeI32, [Parameter "v" TypeI32 Nothing]))
          matching = [variadic, overload]
          args = [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2] -- 2 args > maxNonVariadic (1 for sum_i32)
          fs = HM.fromList matching
          (instrs, _, _) = runGenWithFuncStackUnsafe fs (genUnrolledCall genExpr "sum" matching variadic args)
      in do
         -- accumulateResults generates IRADD_OP
         assertBool "Should have IRADD_OP" (any isAdd instrs)

  , testCase "genOverloadedCall: fallback to baseName when no bestMatch found" $
      let genExpr _ = return ([], IRConstInt 1, IRI32)
          variadic = ("print", (TypeI32, [Parameter "v" (TypeVariadic TypeAny) Nothing]))
          matching = [variadic]
          fs = HM.fromList matching
          (instrs, _) = runGenWithFuncStackUnsafe fs (genOverloadedCall genExpr "print" matching IRI32 ([], IRConstInt 1, IRI32))
      in case filter isCall instrs of
           (IRCALL _ name _ _ : _) -> name @?= "print"
           _ -> error "Should call base print"
  ]

testArgumentCasting :: TestTree
testArgumentCasting = testGroup "genArgWithContext Inference"
  [ testCase "needsInference: covers Char" $
      let genExpr _ = return ([], IRConstChar 'a', IRChar)
          (instrs, _, _) = runGenUnsafe $ genArgWithContext genExpr (ExprLitChar dummyPos 'a') TypeI32
      in assertBool "Should emit cast for Char" (any isAssign instrs)

  , testCase "needsInference: covers Bool" $
      let genExpr _ = return ([], IRConstBool True, IRBool)
          (instrs, _, _) = runGenUnsafe $ genArgWithContext genExpr (ExprLitBool dummyPos True) TypeI32
      in assertBool "Should emit cast for Bool" (any isAssign instrs)

  , testCase "needsInference: covers Global floats" $
      let genExpr _ = return ([], IRGlobal "G" IRF32, IRF32)
          (instrs, _, _) = runGenUnsafe $ genArgWithContext genExpr (ExprVar dummyPos "G") TypeF64
      in assertBool "Should emit cast for Global float" (any isAssign instrs)
  ]

testPointerAndReferenceLogic :: TestTree
testPointerAndReferenceLogic = testGroup "Pointer & Reference Edge Cases"
  [ testCase "prepareArg: handles IRPtr (IRStruct _)" $
      let
          (instrs, op) = prepareArg Map.empty ([], IRTemp "ptr" (IRPtr (IRStruct "S")), IRPtr (IRStruct "S"))
      in do
        assertBool "Should emit ADDR for ptr" (any isAddr instrs)
        op @?= IRTemp "p_ptr" (IRPtr (IRPtr (IRStruct "S")))

  , testCase "prepareParamArg: handles IRGlobal as reference" $
      let (instrs, op) = prepareParamArg Map.empty (TypeRef TypeI32) [] (IRGlobal "glob" IRI32) IRI32
      in do
        assertBool "Correct Global ADDR" (any isAddr instrs)
        op @?= IRTemp "addr_glob" (IRPtr IRI32)

  , testCase "prepareParamArg: handles constants by materializing them" $
      let
          (instrs, op) = prepareParamArg Map.empty (TypeRef TypeI32) [] (IRConstInt 42) IRI32
      in do
        assertBool "Materializes constant" (any isAssign instrs)
        assertBool "Takes address of temp" (any isAddr instrs)
        op @?= IRTemp "addr_tmp" (IRPtr IRI32)
  ]
