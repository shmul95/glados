{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.GeneratorSpecs (generatorTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import qualified Data.HashMap.Strict as HM
import TestHelpers (dummyPos)
import qualified Data.Set as Set
import Rune.IR.Generator (generateIR, initialState, getDefinedFuncName)
import Rune.IR.Nodes (IRProgram(..), IRTopLevel(..), IRFunction(..), IRType(..), IRInstruction(..), GenState(..), IRGlobalValue(..))
import Rune.AST.Nodes (Program(..), TopLevelDef(..), Type(..), Statement(..), Expression(..), Field(..), Parameter(..))

--
-- public
--

generatorTests :: TestTree
generatorTests = testGroup "Rune.IR.Generator"
  [ testGenerateIR
  , testInitialState
  , testGetDefinedFuncName
  ]

--
-- private
--

testGenerateIR :: TestTree
testGenerateIR = testGroup "generateIR"
  [ testCase "Generates empty program" $
      let prog = Program "test" []
          fs = HM.empty
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg -> do
          irProgramName irProg @?= "test"
          irProgramDefs irProg @?= []
  
  , testCase "Generates program with function" $
      let prog = Program "test" 
            [ DefFunction "main" [] TypeNull [] False ]
          fs = HM.empty
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg -> do
          irProgramName irProg @?= "test"
          length (irProgramDefs irProg) @?= 1
          case irProgramDefs irProg of
            [IRFunctionDef func] -> do
              irFuncName func @?= "main"
              irFuncParams func @?= []
              irFuncRetType func @?= Just IRNull
              irFuncBody func @?= [IRRET Nothing]
            _ -> fail "Expected IRFunctionDef"
  
  , testCase "Generates program with external function call" $
      let prog = Program "test"
            [ DefFunction "caller" [] TypeNull 
                [ StmtReturn dummyPos (Just (ExprCall dummyPos "external_func" [])) ]
                False
            ]
          fs = HM.singleton "external_func" [(TypeNull, [])]
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg -> do
          irProgramName irProg @?= "test"
          let defs = irProgramDefs irProg
          assertBool "Should have extern" $ any isExtern defs
          case filter isExtern defs of
            [IRExtern name] -> name @?= "external_func"
            _ -> return ()

  , testCase "Generates program with struct" $
      let prog = Program "test"
            [ DefStruct "Point" 
                [ Field "x" TypeI32
                , Field "y" TypeI32
                ] 
                []
            ]
          fs = HM.empty
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg -> do
          irProgramName irProg @?= "test"
          case filter isStructDef (irProgramDefs irProg) of
            [IRStructDef name fields] -> do
              name @?= "Point"
              fields @?= [("x", IRI32), ("y", IRI32)]
            _ -> fail "Expected IRStructDef"
  
  , testCase "Externs appear before other definitions" $
      let prog = Program "test"
            [ DefFunction "caller" [] TypeNull 
                [ StmtReturn dummyPos (Just (ExprCall dummyPos "ext1" [])) ]
                False
            ]
          fs = HM.singleton "ext1" [(TypeNull, [])]
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg ->
          case irProgramDefs irProg of
            (IRExtern _ : _) -> return ()
            _ -> fail "Expected extern to appear first"
  
  , testCase "Global strings reversed properly" $
      let prog = Program "test"
            [ DefFunction "main" [] TypeNull 
                [ StmtReturn dummyPos (Just (ExprLitString dummyPos "hello"))
                , StmtReturn dummyPos (Just (ExprLitString dummyPos "world"))
                ]
                False
            ]
          fs = HM.empty
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg -> do
          irProgramName irProg @?= "test"
          assertBool "Should have function and globals" $ length (irProgramDefs irProg) >= 1

  , testCase "Multiple functions" $
      let prog = Program "test"
            [ DefFunction "func1" [] TypeI32 [StmtReturn dummyPos (Just (ExprLitInt dummyPos 1))] False
            , DefFunction "func2" [] TypeI32 [StmtReturn dummyPos (Just (ExprLitInt dummyPos 2))] False
            ]
          fs = HM.empty
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg -> do
          let funcDefs = filter isFunctionDef (irProgramDefs irProg)
          length funcDefs @?= 2

  , testCase "Calls to defined functions are not extern" $
      let prog = Program "test"
            [ DefFunction "callee" [] TypeNull [] False
            , DefFunction "caller" [] TypeNull 
                [ StmtExpr dummyPos (ExprCall dummyPos "callee" []) ]
                False
            ]
          fs = HM.singleton "callee" [(TypeNull, [])]
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg -> do
          let externs = filter isExtern (irProgramDefs irProg)
          length externs @?= 0

  , testCase "Difference between called and defined functions" $
      let prog = Program "test"
            [ DefFunction "caller" [] TypeNull 
                [ StmtExpr dummyPos (ExprCall dummyPos "ext1" [])
                , StmtExpr dummyPos (ExprCall dummyPos "ext2" [])
                ]
                False
            ]
          fs = HM.fromList [("ext1", [(TypeNull, [])]), ("ext2", [(TypeNull, [])])]
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg -> do
          let externs = filter isExtern (irProgramDefs irProg)
          length externs @?= 2

  , testCase "Override function generates mangled name" $
      let prog = Program "test"
            [ DefOverride "show" [Parameter "self" (TypeCustom "Point")] TypeNull [] False
            ]
          fs = HM.empty
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg ->
          case filter isFunctionDef (irProgramDefs irProg) of
            [IRFunctionDef func] -> irFuncName func @?= "show_Point"
            _ -> fail "Expected mangled function"

  , testCase "Struct with methods" $
      let prog = Program "test"
            [ DefStruct "Vec2" 
                [Field "x" TypeF32, Field "y" TypeF32]
                [DefFunction "magnitude" [Parameter "self" (TypeCustom "Vec2")] TypeF32 [] False]
            ]
          fs = HM.empty
          result = generateIR prog fs
      in case result of
        Left err -> fail $ "Unexpected error: " ++ err
        Right irProg -> do
          let structDefs = filter isStructDef (irProgramDefs irProg)
          let funcDefs = filter isFunctionDef (irProgramDefs irProg)
          length structDefs @?= 1
          length funcDefs @?= 1
          case funcDefs of
            [IRFunctionDef func] -> irFuncName func @?= "Vec2_magnitude"
            _ -> fail "Expected mangled method"
  ]

testInitialState :: TestTree
testInitialState = testGroup "initialState"
  [ testCase "Creates valid initial state with empty funcStack" $
      let fs = HM.empty
          state = initialState fs
      in do
        gsTempCounter state @?= 0
        gsLabelCounter state @?= 0
        gsStringCounter state @?= 0
        gsGlobals state @?= []
        gsCurrentFunc state @?= Nothing
        Set.null (gsCalledFuncs state) @?= True
        gsFuncStack state @?= fs

  , testCase "Preserves funcStack in initial state" $
      let fs = HM.singleton "test" [(TypeI32, [])]
          state = initialState fs
      in gsFuncStack state @?= fs
  ]

testGetDefinedFuncName :: TestTree
testGetDefinedFuncName = testGroup "getDefinedFuncName"
  [ testCase "Extracts function name from IRFunctionDef" $
      let func = IRFunction "myFunc" [] Nothing [] False
          topLevel = IRFunctionDef func
      in getDefinedFuncName topLevel @?= ["myFunc"]
  
  , testCase "Returns empty list for IRExtern" $
      getDefinedFuncName (IRExtern "ext") @?= []

  , testCase "Returns empty list for IRGlobal" $
      getDefinedFuncName (IRGlobalDef "str" (IRGlobalStringVal "val")) @?= []

  , testCase "Returns empty list for IRStructDef" $
      getDefinedFuncName (IRStructDef "S" []) @?= []
  ]

--
-- helpers
--

isExtern :: IRTopLevel -> Bool
isExtern (IRExtern _) = True
isExtern _ = False

isStructDef :: IRTopLevel -> Bool
isStructDef (IRStructDef _ _) = True
isStructDef _ = False

isFunctionDef :: IRTopLevel -> Bool
isFunctionDef (IRFunctionDef _) = True
isFunctionDef _ = False
