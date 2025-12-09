module IR.IRShowCallSpec (irShowCallTests) where

import Control.Monad.State (evalState)
import Rune.AST.Nodes (Expression (..))
import Rune.IR.Generator.Expression.Call.Show (genShowCall)
import Rune.IR.Nodes (GenState (..), IRInstruction (..), IROperand (..), IRType (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- public
--

irShowCallTests :: TestTree
irShowCallTests =
  testGroup
    "IR.Generator.Expression.Call.Show Specs"
    [ testShowBoolCalls,
      testShowPrimitiveCalls,
      testShowStructCalls,
      testShowFormatSpecifiers
    ]

--
-- private
--

initialState :: GenState
initialState =
  GenState
    { gsTempCounter = 0,
      gsLabelCounter = 0,
      gsGlobals = [],
      gsStringCounter = 0,
      gsCurrentFunc = Nothing,
      gsSymTable = mempty,
      gsStructs = mempty,
      gsLoopStack = [],
      gsCalledFuncs = mempty,
      gsStringMap = mempty
    }

mockGenExpr :: Expression -> ([IRInstruction], IROperand, IRType)
mockGenExpr (ExprLitBool b) = ([], IRConstBool b, IRBool)
mockGenExpr (ExprLitInt i) = ([], IRConstInt i, IRI32)
mockGenExpr (ExprLitChar c) = ([], IRConstChar c, IRChar)
mockGenExpr (ExprLitFloat _) = ([], IRTemp "t_float" IRF32, IRF32)
mockGenExpr (ExprLitString _) = ([], IRTemp "t_str" (IRPtr IRChar), IRPtr IRChar)
mockGenExpr ExprLitNull = ([], IRTemp "t_null" IRNull, IRNull)
mockGenExpr (ExprVar name) = ([], IRTemp name IRI32, IRI32)
mockGenExpr _ = ([], IRTemp "t_unknown" IRI32, IRI32)

runShowCall :: Expression -> IRType -> (([IRInstruction], IROperand, IRType), GenState)
runShowCall expr _expectedType =
  let genExpr _e = return $ mockGenExpr expr
      computation = genShowCall genExpr expr
   in case evalState computation initialState of
        result -> (result, initialState)

testShowBoolCalls :: TestTree
testShowBoolCalls =
  testGroup
    "show(bool) calls"
    [ testCase "show(true) generates show_bool call" $ do
        let expr = ExprLitBool True
            ((instrs, _op, typ), _) = runShowCall expr IRBool
        typ @?= IRNull
        length (filter isCallInstr instrs) > 0 @?= True,
      testCase "show(false) generates show_bool call" $ do
        let expr = ExprLitBool False
            ((instrs, _op, typ), _) = runShowCall expr IRBool
        typ @?= IRNull
        length (filter isCallInstr instrs) > 0 @?= True
    ]

testShowPrimitiveCalls :: TestTree
testShowPrimitiveCalls =
  testGroup
    "show(primitive) calls"
    [ testCase "show(i32) generates printf call" $ do
        let mockExpr = ExprLitInt 42
            genExpr _e = return ([], IRConstInt 42, IRI32)
            result = evalState (genShowCall genExpr mockExpr) initialState
        case result of
          (instrs, _, typ) -> do
            typ @?= IRNull
            length instrs > 0 @?= True,
      testCase "show(char) generates printf call" $ do
        let mockExpr = ExprLitChar 'x'
            genExpr _e = return ([], IRConstChar 'x', IRChar)
            result = evalState (genShowCall genExpr mockExpr) initialState
        case result of
          (instrs, _, typ) -> do
            typ @?= IRNull
            length instrs > 0 @?= True,
      testCase "show(f32) generates printf call" $ do
        let mockExpr = ExprLitFloat 3.14
            genExpr _e = return ([], IRTemp "t_f" IRF32, IRF32)
            result = evalState (genShowCall genExpr mockExpr) initialState
        case result of
          (instrs, _, typ) -> do
            typ @?= IRNull
            length instrs > 0 @?= True,
      testCase "show(f64) generates printf call" $ do
        let mockExpr = ExprVar "d"
            genExpr _e = return ([], IRTemp "d" IRF64, IRF64)
            result = evalState (genShowCall genExpr mockExpr) initialState
        case result of
          (instrs, _, typ) -> do
            typ @?= IRNull
            length instrs > 0 @?= True,
      testCase "show(null) generates printf call" $ do
        let mockExpr = ExprLitNull
            genExpr _e = return ([], IRTemp "t_null" IRNull, IRNull)
            result = evalState (genShowCall genExpr mockExpr) initialState
        case result of
          (instrs, _, typ) -> do
            typ @?= IRNull
            length instrs > 0 @?= True
    ]

testShowStructCalls :: TestTree
testShowStructCalls =
  testGroup
    "show(struct) calls"
    [ testCase "show(struct) generates struct-specific show call" $ do
        let mockExpr = ExprVar "point"
            genExpr _e = return ([], IRTemp "point" (IRStruct "Point"), IRStruct "Point")
            result = evalState (genShowCall genExpr mockExpr) initialState
        case result of
          (instrs, _, typ) -> do
            typ @?= IRNull
            length instrs > 0 @?= True,
      testCase "show(ptr-to-struct) generates struct-specific show call" $ do
        let mockExpr = ExprVar "p_point"
            genExpr _e = return ([], IRTemp "p_point" (IRPtr (IRStruct "Point")), IRPtr (IRStruct "Point"))
            result = evalState (genShowCall genExpr mockExpr) initialState
        case result of
          (instrs, _, typ) -> do
            typ @?= IRNull
            length instrs > 0 @?= True
    ]

testShowFormatSpecifiers :: TestTree
testShowFormatSpecifiers =
  testGroup
    "format specifier coverage"
    [ testCase "i8 format specifier" $ do
        let genExpr _e = return ([], IRTemp "i" IRI8, IRI8)
            result = evalState (genShowCall genExpr (ExprVar "i")) initialState
        case result of
          (instrs, _, _) -> length instrs > 0 @?= True,
      testCase "i16 format specifier" $ do
        let genExpr _e = return ([], IRTemp "i" IRI16, IRI16)
            result = evalState (genShowCall genExpr (ExprVar "i")) initialState
        case result of
          (instrs, _, _) -> length instrs > 0 @?= True,
      testCase "i64 format specifier" $ do
        let genExpr _e = return ([], IRTemp "i" IRI64, IRI64)
            result = evalState (genShowCall genExpr (ExprVar "i")) initialState
        case result of
          (instrs, _, _) -> length instrs > 0 @?= True,
      testCase "u8 format specifier" $ do
        let genExpr _e = return ([], IRTemp "u" IRU8, IRU8)
            result = evalState (genShowCall genExpr (ExprVar "u")) initialState
        case result of
          (instrs, _, _) -> length instrs > 0 @?= True,
      testCase "u16 format specifier" $ do
        let genExpr _e = return ([], IRTemp "u" IRU16, IRU16)
            result = evalState (genShowCall genExpr (ExprVar "u")) initialState
        case result of
          (instrs, _, _) -> length instrs > 0 @?= True,
      testCase "u32 format specifier" $ do
        let genExpr _e = return ([], IRTemp "u" IRU32, IRU32)
            result = evalState (genShowCall genExpr (ExprVar "u")) initialState
        case result of
          (instrs, _, _) -> length instrs > 0 @?= True,
      testCase "u64 format specifier" $ do
        let genExpr _e = return ([], IRTemp "u" IRU64, IRU64)
            result = evalState (genShowCall genExpr (ExprVar "u")) initialState
        case result of
          (instrs, _, _) -> length instrs > 0 @?= True,
      testCase "string format specifier" $ do
        let genExpr _e = return ([], IRTemp "s" (IRPtr IRChar), IRPtr IRChar)
            result = evalState (genShowCall genExpr (ExprVar "s")) initialState
        case result of
          (instrs, _, _) -> length instrs > 0 @?= True
    ]

isCallInstr :: IRInstruction -> Bool
isCallInstr (IRCALL _ _ _ _) = True
isCallInstr _ = False
