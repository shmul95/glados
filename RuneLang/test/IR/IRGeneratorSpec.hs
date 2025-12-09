module IR.IRGeneratorSpec (irGeneratorTests) where

import qualified Rune.AST.Nodes as AST
import Rune.AST.Nodes hiding (Field)
import Rune.IR.Generator (generateIR)
import Rune.IR.Nodes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

--
-- public
--

irGeneratorTests :: TestTree
irGeneratorTests =
  testGroup
    "Rune.IR.Generator Specs"
    [ testEmptyProgram,
      testSimpleFunction,
      testFunctionWithParams,
      testFunctionWithReturn,
      testGlobalStringGeneration,
      testExternFunctionDetection,
      testMultipleFunctions,
      testFunctionCallDetection,
      testShowNull,
      testShowStruct
    ]

--
-- private
--

testEmptyProgram :: TestTree
testEmptyProgram =
  testCase "Generate IR for empty program" $
    let astProg = Program "empty" []
        irProg = generateIR astProg
     in do
          irProgramName irProg @?= "empty"
          irProgramDefs irProg @?= []

testSimpleFunction :: TestTree
testSimpleFunction =
  testCase "Generate IR for simple function" $
    let func =
          DefFunction
            { funcName = "main",
              funcParams = [],
              funcReturnType = TypeNull,
              funcBody = [StmtReturn Nothing]
            }
        astProg = Program "test" [func]
        irProg = generateIR astProg
        defs = irProgramDefs irProg
     in do
          assertBool "Has at least one definition" (length defs >= 1)
          let funcDefs = [f | IRFunctionDef f <- defs]
          assertBool "Has main function" (any (\f -> irFuncName f == "main") funcDefs)

testFunctionWithParams :: TestTree
testFunctionWithParams =
  testCase "Generate IR for function with parameters" $
    let func =
          DefFunction
            { funcName = "add",
              funcParams = [Parameter "a" TypeI32, Parameter "b" TypeI32],
              funcReturnType = TypeI32,
              funcBody = [StmtReturn (Just (ExprVar "a"))]
            }
        astProg = Program "test" [func]
        irProg = generateIR astProg
        defs = irProgramDefs irProg
     in do
          let funcDefs = [f | IRFunctionDef f <- defs]
          assertBool "Has function" (not (null funcDefs))
          case funcDefs of
            (addFunc : _) -> do
              irFuncName addFunc @?= "add"
              irFuncParams addFunc @?= [("p_a", IRI32), ("p_b", IRI32)]
              irFuncRetType addFunc @?= Just IRI32
            [] -> error "Expected at least one function"

testFunctionWithReturn :: TestTree
testFunctionWithReturn =
  testCase "Generate IR for function with return value" $
    let func =
          DefFunction
            { funcName = "getValue",
              funcParams = [],
              funcReturnType = TypeI32,
              funcBody = [StmtReturn (Just (ExprLitInt 42))]
            }
        astProg = Program "test" [func]
        irProg = generateIR astProg
        defs = irProgramDefs irProg
     in do
          let funcDefs = [f | IRFunctionDef f <- defs]
          assertBool "Has function" (not (null funcDefs))
          case funcDefs of
            (f : _) -> assertBool "Body has return" (any isReturnInstr (irFuncBody f))
            [] -> error "Expected at least one function"

testGlobalStringGeneration :: TestTree
testGlobalStringGeneration =
  testCase "Generate global strings for string literals" $
    let callExpr = ExprCall {callName = "printf", callArgs = [ExprLitString "Hello"]}
        func =
          DefFunction
            { funcName = "main",
              funcParams = [],
              funcReturnType = TypeNull,
              funcBody = [StmtExpr callExpr, StmtReturn Nothing]
            }
        astProg = Program "test" [func]
        irProg = generateIR astProg
        defs = irProgramDefs irProg
        hasGlobalString = any isGlobalString defs
     in assertBool "Has global string" hasGlobalString

testExternFunctionDetection :: TestTree
testExternFunctionDetection =
  testCase "Detect external function calls" $
    let callExpr = ExprCall {callName = "puts", callArgs = [ExprLitString "test"]}
        func =
          DefFunction
            { funcName = "main",
              funcParams = [],
              funcReturnType = TypeNull,
              funcBody = [StmtExpr callExpr, StmtReturn Nothing]
            }
        astProg = Program "test" [func]
        irProg = generateIR astProg
        defs = irProgramDefs irProg
        hasExtern = any (\d -> case d of IRExtern _ -> True; _ -> False) defs
     in assertBool "Has extern" hasExtern

testMultipleFunctions :: TestTree
testMultipleFunctions =
  testCase "Generate IR for multiple functions" $
    let func1 =
          DefFunction
            { funcName = "foo",
              funcParams = [],
              funcReturnType = TypeNull,
              funcBody = [StmtReturn Nothing]
            }
        func2 =
          DefFunction
            { funcName = "bar",
              funcParams = [],
              funcReturnType = TypeNull,
              funcBody = [StmtReturn Nothing]
            }
        astProg = Program "test" [func1, func2]
        irProg = generateIR astProg
        defs = irProgramDefs irProg
        funcDefs = filter isFunctionDef defs
     in length funcDefs @?= 2

testFunctionCallDetection :: TestTree
testFunctionCallDetection =
  testCase "Track called functions" $
    let callExpr = ExprCall {callName = "helper", callArgs = []}
        func1 =
          DefFunction
            { funcName = "main",
              funcParams = [],
              funcReturnType = TypeNull,
              funcBody = [StmtExpr callExpr, StmtReturn Nothing]
            }
        func2 =
          DefFunction
            { funcName = "helper",
              funcParams = [],
              funcReturnType = TypeNull,
              funcBody = [StmtReturn Nothing]
            }
        astProg = Program "test" [func1, func2]
        irProg = generateIR astProg
        defs = irProgramDefs irProg
        hasHelperDef = any (\d -> case d of IRFunctionDef f -> irFuncName f == "helper"; _ -> False) defs
     in assertBool "Has helper function" hasHelperDef

testShowNull :: TestTree
testShowNull =
  testCase "Generate IR for show(null) using (null) format" $
    let callExpr = ExprCall {callName = "show", callArgs = [ExprLitNull]}
        func =
          DefFunction
            { funcName = "main",
              funcParams = [],
              funcReturnType = TypeNull,
              funcBody = [StmtExpr callExpr, StmtReturn Nothing]
            }
        astProg = Program "test" [func]
        irProg = generateIR astProg
        defs = irProgramDefs irProg
        hasNullFormat = any (\d -> case d of IRGlobalString _ s -> s == "(null)"; _ -> False) defs
     in assertBool "Has (null) format string" hasNullFormat

testShowStruct :: TestTree
testShowStruct =
  testCase "Generate IR for show(struct)" $
    let structDef = DefStruct "Vec" [AST.Field "x" TypeI32] []
        initExpr = ExprStructInit "Vec" [("x", ExprLitInt 1)]
        callExpr = ExprCall "show" [initExpr]
        func = DefFunction "main" [] TypeNull [StmtExpr callExpr, StmtReturn Nothing]
        prog = Program "test" [structDef, func]
        irProg = generateIR prog
        defs = irProgramDefs irProg
        hasShowVec = any (\d -> case d of IRFunctionDef f -> any isCallShowVec (irFuncBody f); _ -> False) defs
        isCallShowVec (IRCALL _ "show_Vec" _ _) = True
        isCallShowVec _ = False
     in assertBool "Has call to show_Vec" hasShowVec

isReturnInstr :: IRInstruction -> Bool
isReturnInstr (IRRET _) = True
isReturnInstr _ = False

isGlobalString :: IRTopLevel -> Bool
isGlobalString (IRGlobalString _ _) = True
isGlobalString _ = False

isFunctionDef :: IRTopLevel -> Bool
isFunctionDef (IRFunctionDef _) = True
isFunctionDef _ = False
