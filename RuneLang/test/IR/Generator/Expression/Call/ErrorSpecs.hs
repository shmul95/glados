{-# LANGUAGE OverloadedStrings #-}

module IR.Generator.Expression.Call.ErrorSpecs (errorCallTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Control.Monad.State (runState)
import Control.Monad.Except (runExceptT)
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import TestHelpers (dummyPos)
import Rune.IR.Generator.Expression.Call.Error
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..), IRTopLevel(..), IRFunction(..), IRGlobalValue(..), GenState(..), IRGen)
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (emptyState)

--
-- public
--

errorCallTests :: TestTree
errorCallTests = testGroup "Rune.IR.Generator.Expression.Call.Error"
  [ test_error_bool_dispatch
  , test_error_char_dispatch
  , test_error_i64_dispatch
  , test_error_struct_dispatch
  , test_get_error_func_logic
  , test_ensure_error_bool_func_idempotency
  , test_mk_error_bool_func_content
  ]

--
-- helpers
--

isCallTo :: String -> IRInstruction -> Bool
isCallTo name (IRCALL _ n _ _) = n == name
isCallTo _ _ = False

isErrorBoolDef :: IRTopLevel -> Bool
isErrorBoolDef (IRFunctionDef f) = irFuncName f == "error_bool"
isErrorBoolDef _ = False

genExprSimple :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExprSimple (ExprLitInt _ n) = return ([], IRConstInt n, IRI64)
genExprSimple (ExprLitBool _ b) = return ([], IRConstBool b, IRBool)
genExprSimple (ExprLitChar _ c) = return ([], IRConstChar c, IRChar)
genExprSimple (ExprVar _ "s") = return ([], IRTemp "s" (IRStruct "Vec"), IRStruct "Vec")
genExprSimple _ = error "Unexpected expression in genExprSimple"

--
-- tests
--

test_error_bool_dispatch :: TestTree
test_error_bool_dispatch = testCase "genErrorCall with Bool dispatches to error_bool" $ do
    case runState (runExceptT $ genErrorCall genExprSimple (ExprLitBool dummyPos True)) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), state) -> do
        typ @?= IRNull
        assertBool "Should contain call to error_bool" $ any (isCallTo "error_bool") instrs
        assertBool "error_bool function should be defined" $ any isErrorBoolDef (gsGlobals state)

test_error_char_dispatch :: TestTree
test_error_char_dispatch = testCase "genErrorCall with Char dispatches to dprintf" $ do
    case runState (runExceptT $ genErrorCall genExprSimple (ExprLitChar dummyPos 'x')) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), _) -> do
        typ @?= IRNull
        assertBool "Should contain call to dprintf" $ any (isCallTo "dprintf") instrs
        -- stderr registration removed

test_error_i64_dispatch :: TestTree
test_error_i64_dispatch = testCase "genErrorCall with IRI64 uses dprintf with fd 2 and %ld" $ do
    case runState (runExceptT $ genErrorCall genExprSimple (ExprLitInt dummyPos 42)) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), state) -> do
        typ @?= IRNull
        assertBool "Should call dprintf" $ any (isCallTo "dprintf") instrs
        
        let formatStrs = [v | IRGlobalDef _ (IRGlobalStringVal v) <- gsGlobals state]
        assertBool "Should register %ld" $ any ("%ld" `isPrefixOf`) formatStrs
        
        case filter (isCallTo "dprintf") instrs of
          [IRCALL _ _ (fdArg:_) _] -> do
            fdArg @?= IRConstInt 2
          _ -> assertFailure "Expected exactly one dprintf call with fd 2 as first argument"

test_error_struct_dispatch :: TestTree
test_error_struct_dispatch = testCase "genErrorCall with IRStruct uses error_StructName" $ do
    case runState (runExceptT $ genErrorCall genExprSimple (ExprVar dummyPos "s")) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right (instrs, _, typ), _) -> do
        typ @?= IRNull
        assertBool "Should call error_Vec" $ any (isCallTo "error_Vec") instrs
        assertBool "Should have prepared address (IRADDR)" $ 
          any (\i -> case i of IRADDR {} -> True; _ -> False) instrs

test_get_error_func_logic :: TestTree
test_get_error_func_logic = testGroup "getErrorFunc logic"
  [ testCase "Struct" $ 
      getErrorFunc (IRTemp "s" (IRStruct "Pos")) (IRStruct "Pos") @?= "error_Pos"
  , testCase "Struct Ptr" $ 
      getErrorFunc (IRTemp "p" (IRPtr (IRStruct "Pos"))) (IRPtr (IRStruct "Pos")) @?= "error_Pos"
  , testCase "Primitive" $ 
      getErrorFunc (IRConstInt 0) IRI32 @?= "dprintf"
  ]

test_ensure_error_bool_func_idempotency :: TestTree
test_ensure_error_bool_func_idempotency = testCase "ensureErrorBoolFunc adds definition only once" $ do
    let action = ensureErrorBoolFunc >> ensureErrorBoolFunc
    case runState (runExceptT action) emptyState of
      (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
      (Right _, state) -> do
        let defs = filter isErrorBoolDef (gsGlobals state)
        length defs @?= 1

test_mk_error_bool_func_content :: TestTree
test_mk_error_bool_func_content = testCase "mkErrorBoolFunc generates correct body with dprintf" $ do
  case runState (runExceptT mkErrorBoolFunc) emptyState of
    (Left err, _) -> assertFailure $ "Unexpected error: " ++ err
    (Right _, state) -> do
      assertBool "dprintf should be registered" $ Set.member "dprintf" (gsCalledFuncs state)
      
      case gsGlobals state of
        (IRFunctionDef f : _) -> do
          irFuncName f @?= "error_bool"
          let body = irFuncBody f
          assertBool "Should contain IRJUMP_TRUE" $ any (\i -> case i of IRJUMP_TRUE {} -> True; _ -> False) body
          assertBool "Should contain dprintf calls" $ any (isCallTo "dprintf") body
          assertBool "Calls should include fd 2 as first arg" $ 
            any (\i -> case i of IRCALL _ "dprintf" (IRConstInt 2 : _) _ -> True; _ -> False) body
        _ -> assertFailure "Function definition not found in globals"
