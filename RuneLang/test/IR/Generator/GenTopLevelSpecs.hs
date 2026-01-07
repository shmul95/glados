{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.GenTopLevelSpecs (genTopLevelTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import TestHelpers (dummyPos)

import Control.Monad.State (evalState, gets)
import Control.Monad.Except (runExceptT)

import Rune.IR.Generator.GenTopLevel
import Rune.IR.Nodes (GenState(..), IRTopLevel(..), IRFunction(..), IRType(..), IRInstruction(..))
import Rune.AST.Nodes (TopLevelDef(..), Parameter(..), Field(..), Type(..), Statement(..))
import IR.TestUtils (emptyState, runGenUnsafe)

--
-- public
--

genTopLevelTests :: TestTree
genTopLevelTests = testGroup "Rune.IR.Generator.GenTopLevel"
  [ testGenTopLevel
  , testGenFunction
  , testGenOverride
  , testGenStruct
  , testGenStructMethod
  , testGenParam
  , testFixSelfParam
  , testResetFunctionState
  , testClearFunctionState
  , testEnsureReturn
  , testLastOrNothing
  ]

--
-- private
--

testGenTopLevel :: TestTree
testGenTopLevel = testGroup "genTopLevel"
  [ testCase "Routes DefFunction to genFunction" $
      let def = DefFunction "test" [] TypeNull [] False
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRFunctionDef func] -> irFuncName func @?= "test"
        _ -> assertBool "Expected IRFunctionDef" False

  , testCase "Routes DefOverride to genOverride" $
      let def = DefOverride "show" [Parameter "self" (TypeCustom "Point")] TypeNull [] False
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRFunctionDef func] -> irFuncName func @?= "show_Point"
        _ -> assertBool "Expected IRFunctionDef" False

  , testCase "Routes DefStruct to genStruct" $
      let def = DefStruct "Point" [Field "x" TypeI32] []
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRStructDef name _] -> name @?= "Point"
        _ -> assertBool "Expected IRStructDef" False
  ]

testGenFunction :: TestTree
testGenFunction = testGroup "genFunction"
  [ testCase "Generates function with no params or body" $
      let def = DefFunction "empty" [] TypeNull [] False
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRFunctionDef func] -> do
          irFuncName func @?= "empty"
          irFuncParams func @?= []
          irFuncRetType func @?= Just IRNull
          irFuncBody func @?= [IRRET Nothing]
        _ -> assertBool "Expected IRFunctionDef" False

  , testCase "Generates function with params" $
      let def = DefFunction "add" [Parameter "a" TypeI32, Parameter "b" TypeI32] TypeI32 [] False
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRFunctionDef func] -> do
          irFuncName func @?= "add"
          length (irFuncParams func) @?= 2
        _ -> assertBool "Expected IRFunctionDef" False

  , testCase "Generates function with body" $
      let def = DefFunction "test" [] TypeNull [StmtReturn dummyPos Nothing] False
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRFunctionDef func] -> do
          assertBool "Body should have IRRET" $ not $ null (irFuncBody func)
        _ -> assertBool "Expected IRFunctionDef" False
  ]

testGenOverride :: TestTree
testGenOverride = testGroup "genOverride"
  [ testCase "Mangles name with TypeCustom first param" $
      let def = DefOverride "show" [Parameter "self" (TypeCustom "Vec2")] TypeNull [] False
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRFunctionDef func] -> irFuncName func @?= "show_Vec2"
        _ -> assertBool "Expected IRFunctionDef" False

  , testCase "Does not mangle without TypeCustom" $
      let def = DefOverride "print" [Parameter "x" TypeI32] TypeNull [] False
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRFunctionDef func] -> irFuncName func @?= "print"
        _ -> assertBool "Expected IRFunctionDef" False

  , testCase "Handles empty params" $
      let def = DefOverride "test" [] TypeNull [] False
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRFunctionDef func] -> irFuncName func @?= "test"
        _ -> assertBool "Expected IRFunctionDef" False
  ]

testGenStruct :: TestTree
testGenStruct = testGroup "genStruct"
  [ testCase "Generates struct with fields" $
      let def = DefStruct "Point" [Field "x" TypeI32, Field "y" TypeI32] []
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRStructDef name fields] -> do
          name @?= "Point"
          fields @?= [("x", IRI32), ("y", IRI32)]
        _ -> assertBool "Expected IRStructDef" False

  , testCase "Generates struct with methods" $
      let def = DefStruct "Vec2" [Field "x" TypeF32] 
                [DefFunction "magnitude" [Parameter "self" (TypeCustom "Vec2")] TypeF32 [] False]
          result = runGenUnsafe (genTopLevel def)
      in do
        length result @?= 2
        case result of
          [IRStructDef _ _, IRFunctionDef func] -> irFuncName func @?= "Vec2_magnitude"
          _ -> assertBool "Expected struct and method" False

  , testCase "Handles struct with no fields" $
      let def = DefStruct "Empty" [] []
          result = runGenUnsafe (genTopLevel def)
      in case result of
        [IRStructDef name fields] -> do
          name @?= "Empty"
          fields @?= []
        _ -> assertBool "Expected IRStructDef" False
  ]

testGenStructMethod :: TestTree
testGenStructMethod = testGroup "genStructMethod"
  [ testCase "Mangles method name" $
      let method = DefFunction "calc" [Parameter "self" (TypeCustom "Point")] TypeI32 [] False
          result = runGenUnsafe (genStructMethod "Point" method)
      in case result of
        [IRFunctionDef func] -> irFuncName func @?= "Point_calc"
        _ -> assertBool "Expected IRFunctionDef" False

  , testCase "Fixes self parameter type" $
      let method = DefFunction "test" [Parameter "self" TypeAny] TypeNull [] False
          result = runGenUnsafe (genStructMethod "Vec" method)
      in case result of
        [IRFunctionDef _] -> return ()
        _ -> assertBool "Expected IRFunctionDef" False

  , testCase "Returns empty for non-function" $
      let result = runGenUnsafe (genStructMethod "S" (DefStruct "X" [] []))
      in result @?= []
  ]

testGenParam :: TestTree
testGenParam = testGroup "genParam"
  [ testCase "Generates param with i32 type" $
      let param = Parameter "x" TypeI32
      in case evalState (runExceptT (genParam param)) emptyState of
        Left err -> assertFailure $ "Unexpected error: " ++ err
        Right result -> result @?= ("p_x", IRI32)

  , testCase "Converts struct type to pointer" $
      let param = Parameter "point" (TypeCustom "Point")
      in case evalState (runExceptT (genParam param)) emptyState of
        Left err -> assertFailure $ "Unexpected error: " ++ err
        Right result -> result @?= ("p_point", IRPtr (IRStruct "Point"))

  , testCase "Handles primitive types" $
      let param = Parameter "flag" TypeBool
      in case evalState (runExceptT (genParam param)) emptyState of
        Left err -> assertFailure $ "Unexpected error: " ++ err
        Right result -> result @?= ("p_flag", IRBool)
  ]

testFixSelfParam :: TestTree
testFixSelfParam = testGroup "fixSelfParam"
  [ testCase "Fixes self parameter" $
      let param = Parameter "self" TypeAny
          result = fixSelfParam "Vec2" param
      in result @?= Parameter "self" (TypeCustom "Vec2")

  , testCase "Leaves other params unchanged" $
      let param = Parameter "x" TypeI32
          result = fixSelfParam "Vec2" param
      in result @?= Parameter "x" TypeI32

  , testCase "Fixes self even with different type" $
      let param = Parameter "self" TypeI32
          result = fixSelfParam "Point" param
      in result @?= Parameter "self" (TypeCustom "Point")
  ]

testResetFunctionState :: TestTree
testResetFunctionState = testGroup "resetFunctionState"
  [ testCase "Sets current function name" $
      let initialState = emptyState { gsTempCounter = 10 }
      in case evalState (runExceptT (do
           resetFunctionState "test"
           gets gsCurrentFunc)) initialState of
        Left err -> assertFailure $ "Unexpected error: " ++ err
        Right result -> result @?= Just "test"

  , testCase "Resets temp counter" $
      let initialState = emptyState { gsTempCounter = 10 }
      in case evalState (runExceptT (do
           resetFunctionState "f"
           gets gsTempCounter)) initialState of
        Left err -> assertFailure $ "Unexpected error: " ++ err
        Right result -> result @?= 0
  ]

testClearFunctionState :: TestTree
testClearFunctionState = testGroup "clearFunctionState"
  [ testCase "Clears current function" $
      let initialState = emptyState { gsCurrentFunc = Just "test" }
      in case evalState (runExceptT (do
           clearFunctionState
           gets gsCurrentFunc)) initialState of
        Left err -> assertFailure $ "Unexpected error: " ++ err
        Right result -> result @?= Nothing
  ]

testEnsureReturn :: TestTree
testEnsureReturn = testGroup "ensureReturn"
  [ testCase "Adds return for IRNull with no existing return" $
      let result = ensureReturn IRNull [IRALLOC "x" IRI32]
      in last result @?= IRRET Nothing

  , testCase "Does not add return for IRNull with existing return" $
      let instrs = [IRALLOC "x" IRI32, IRRET Nothing]
          result = ensureReturn IRNull instrs
      in length result @?= 2

  , testCase "Does not add return for non-IRNull types" $
      let instrs = [IRALLOC "x" IRI32]
          result = ensureReturn IRI32 instrs
      in result @?= instrs

  , testCase "Handles empty instruction list" $
      let result = ensureReturn IRNull []
      in result @?= [IRRET Nothing]
  ]

testLastOrNothing :: TestTree
testLastOrNothing = testGroup "lastOrNothing"
  [ testCase "Returns Nothing for empty list" $
      lastOrNothing ([] :: [Int]) @?= (Nothing :: Maybe Int)

  , testCase "Returns Just last element" $
      lastOrNothing ([1, 2, 3] :: [Int]) @?= Just (3 :: Int)

  , testCase "Returns Just for single element" $
      lastOrNothing ([42] :: [Int]) @?= Just (42 :: Int)
  ]
