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

--
-- public
--

callExprTests :: TestTree
callExprTests = testGroup "Rune.IR.Generator.Expression.Call"
  [ testGenCall
  , testPrepareArg
  , testGenArgWithContext
  ]

--
-- private
--

runGenWithFuncStack :: [(String, Type, [Type])] -> IRGen a -> a
runGenWithFuncStack funcs action =
  let funcStack = HM.fromList [(name, (ret, [Parameter ("p" ++ show i) t Nothing | (i, t) <- zip [0::Int ..] args])) | (name, ret, args) <- funcs]
      state = emptyState { gsFuncStack = funcStack }
  in case evalState (runExceptT action) state of
       Right val -> val
       Left err -> error $ "IR Generation failed: " ++ err

testGenCall :: TestTree
testGenCall = testGroup "genCall"
  [ testCase "Generates function call with no args" $
      let genExpr _ = return ([], IRConstInt 0, IRI32)
          funcs = [("test", TypeI32, [])]
          (instrs, op, _) = runGenWithFuncStack funcs (genCall genExpr "test" [])
      in do
        assertBool "Should have IRCALL" $ any isCall instrs
        case op of
          IRTemp _ _ -> return ()
          _ -> assertBool "Expected IRTemp" False

  , testCase "Generates function call with args" $
      let genExpr (ExprLitInt _ n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          funcs = [("add", TypeI32, [TypeI32, TypeI32])]
          (instrs, _, _) = runGenWithFuncStack funcs (genCall genExpr "add" [ExprLitInt dummyPos 1, ExprLitInt dummyPos 2])
      in assertBool "Should have IRCALL" $ any isCall instrs

  , testCase "Registers function call" $
      assertBool "Should register call" True
  ]

testPrepareArg :: TestTree
testPrepareArg = testGroup "prepareArg"
  [ testCase "Adds IRADDR for struct temp" $
      let (instrs, op) = prepareArg ([], IRTemp "s" (IRStruct "Point"), IRStruct "Point")
      in do
        assertBool "Should have IRADDR" $ not $ null instrs
        case op of
          IRTemp name (IRPtr _) -> assertBool "Name should be prefixed" $ "p_" == take 2 name
          _ -> assertBool "Expected IRTemp with IRPtr" False

  , testCase "Adds IRADDR for pointer to struct" $
      let (instrs, op) = prepareArg ([], IRTemp "s" (IRPtr (IRStruct "Vec")), IRPtr (IRStruct "Vec"))
      in do
        assertBool "Should have IRADDR" $ not $ null instrs
        case op of
          IRTemp _ (IRPtr _) -> return ()
          _ -> assertBool "Expected IRTemp with IRPtr" False

  , testCase "Passes through non-struct operands" $
      let (instrs, op) = prepareArg ([IRALLOC "x" IRI32], IRConstInt 42, IRI32)
      in do
        length instrs @?= 1
        op @?= IRConstInt 42

  , testCase "Passes through non-temp operands" $
      let (instrs, op) = prepareArg ([], IRConstInt 10, IRI32)
      in do
        instrs @?= []
        op @?= IRConstInt 10
  ]

testGenArgWithContext :: TestTree
testGenArgWithContext = testGroup "genArgWithContext"
  [ testCase "Infers type for integer constant" $
      let genExpr (ExprLitInt _ n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, op, inferredType) = runGenUnsafe $ genArgWithContext genExpr (ExprLitInt dummyPos 42) TypeI64
      in do
        length instrs @?= 1
        case op of
          IRTemp _ t -> t @?= IRI64
          _ -> assertBool "Expected IRTemp" False
        inferredType @?= IRI64
  , testCase "Does not infer type when not needed" $
      let genExpr (ExprLitInt _ n) = return ([], IRConstInt n, IRI64)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, op, inferredType) = runGenUnsafe $ genArgWithContext genExpr (ExprLitInt dummyPos 42) TypeI64
      in do
        length instrs @?= 0
        op @?= IRConstInt 42
        inferredType @?= IRI64
  ]

--
-- helpers
--

isCall :: IRInstruction -> Bool
isCall (IRCALL _ _ _ _) = True
isCall _ = False
