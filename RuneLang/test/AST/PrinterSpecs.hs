module AST.PrinterSpecs (astPrinterTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Rune.AST.Nodes
import Rune.AST.Printer
import Control.Monad.State.Strict (execState)
import TestHelpers (dummyPos)

--
-- public
--

astPrinterTests :: TestTree
astPrinterTests =
  testGroup
    "AST Printer Tests"
    [ showTypeTests
    , showBinaryOpTests
    , showUnaryOpTests
    , helperTests
    , expressionPrinterTests
    , statementPrinterTests
    , topLevelPrinterTests
    , programPrinterTests
    ]

--
-- helpers
--

runPrinter :: Printer () -> String
runPrinter action =
  let initialState = PrinterState 0 id
      finalState = execState action initialState
   in psOutput finalState ""

--
-- tests
--

showTypeTests :: TestTree
showTypeTests = testGroup "showType Tests"
  [ testCase "i8" $ assertEqual "i8" "i8" (showType TypeI8)
  , testCase "i16" $ assertEqual "i16" "i16" (showType TypeI16)
  , testCase "i32" $ assertEqual "i32" "i32" (showType TypeI32)
  , testCase "i64" $ assertEqual "i64" "i64" (showType TypeI64)
  , testCase "u8" $ assertEqual "u8" "u8" (showType TypeU8)
  , testCase "u16" $ assertEqual "u16" "u16" (showType TypeU16)
  , testCase "u32" $ assertEqual "u32" "u32" (showType TypeU32)
  , testCase "u64" $ assertEqual "u64" "u64" (showType TypeU64)
  , testCase "f32" $ assertEqual "f32" "f32" (showType TypeF32)
  , testCase "f64" $ assertEqual "f64" "f64" (showType TypeF64)
  , testCase "bool" $ assertEqual "bool" "bool" (showType TypeBool)
  , testCase "char" $ assertEqual "char" "char" (showType TypeChar)
  , testCase "string" $ assertEqual "string" "string" (showType TypeString)
  , testCase "any" $ assertEqual "any" "any" (showType TypeAny)
  , testCase "null" $ assertEqual "null" "null" (showType TypeNull)
  , testCase "Custom" $ assertEqual "Custom" "MyType" (showType (TypeCustom "MyType"))
  ]

showBinaryOpTests :: TestTree
showBinaryOpTests = testGroup "showBinaryOp Tests"
  [ testCase "Add" $ assertEqual "+" "+" (showBinaryOp Add)
  , testCase "Sub" $ assertEqual "-" "-" (showBinaryOp Sub)
  , testCase "Mul" $ assertEqual "*" "*" (showBinaryOp Mul)
  , testCase "Div" $ assertEqual "/" "/" (showBinaryOp Div)
  , testCase "Mod" $ assertEqual "%" "%" (showBinaryOp Mod)
  , testCase "Eq" $ assertEqual "==" "==" (showBinaryOp Eq)
  , testCase "Neq" $ assertEqual "!=" "!=" (showBinaryOp Neq)
  , testCase "Lt" $ assertEqual "<" "<" (showBinaryOp Lt)
  , testCase "Lte" $ assertEqual "<=" "<=" (showBinaryOp Lte)
  , testCase "Gt" $ assertEqual ">" ">" (showBinaryOp Gt)
  , testCase "Gte" $ assertEqual ">=" ">=" (showBinaryOp Gte)
  , testCase "And" $ assertEqual "&&" "&&" (showBinaryOp And)
  , testCase "Or" $ assertEqual "||" "||" (showBinaryOp Or)
  ]

showUnaryOpTests :: TestTree
showUnaryOpTests = testGroup "showUnaryOp Tests"
  [ testCase "Negate" $ assertEqual "-" "-" (showUnaryOp Negate)
  , testCase "Not" $ assertEqual "!" "!" (showUnaryOp Not)
  , testCase "PropagateError" $ assertEqual "?" "?" (showUnaryOp PropagateError)
  , testCase "PrefixInc" $ assertEqual "++(prefix)" "++(prefix)" (showUnaryOp PrefixInc)
  , testCase "PrefixDec" $ assertEqual "--(prefix)" "--(prefix)" (showUnaryOp PrefixDec)
  , testCase "PostfixInc" $ assertEqual "(postfix)++" "(postfix)++" (showUnaryOp PostfixInc)
  , testCase "PostfixDec" $ assertEqual "(postfix)--" "(postfix)--" (showUnaryOp PostfixDec)
  ]

helperTests :: TestTree
helperTests = testGroup "Helper Function Tests"
  [ testCase "newLine" $
      assertEqual "NewLine" "\n" (runPrinter newLine)
  , testCase "indent & dedent effect" $
      let action = do
            emitParam (Parameter "p1" TypeI32 Nothing)
            indent
            newLine
            emitParam (Parameter "p2" TypeI32 Nothing)
            dedent
      in assertEqual "Indent/Dedent" "\np1: i32\n  \n  p2: i32" (runPrinter action)
  , testCase "emitParam" $
      assertEqual "emitParam" "\np: i32" (runPrinter (emitParam (Parameter "p" TypeI32 Nothing)))
  ]

expressionPrinterTests :: TestTree
expressionPrinterTests = testGroup "Expression Printer Tests"
  [ testCase "ExprBinary" $
      assertEqual "Binary" "ExprBinary +\n  ExprLitInt 1\n  ExprLitInt 2" 
        (runPrinter $ visitExpression (ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 2)))
  , testCase "ExprUnary" $
      assertEqual "Unary" "ExprUnary -\n  ExprLitInt 1"
        (runPrinter $ visitExpression (ExprUnary dummyPos Negate (ExprLitInt dummyPos 1)))
  , testCase "ExprCall" $
      assertEqual "Call" "ExprCall\n  Target:\n    ExprVar f\n  Arguments:\n    ExprLitInt 1"
        (runPrinter $ visitExpression (ExprCall dummyPos (ExprVar dummyPos "f") [ExprLitInt dummyPos 1]))
  , testCase "ExprStructInit" $
      assertEqual "StructInit" "ExprStructInit P\nFields:\n  x:\n  \n    ExprLitInt 1"
        (runPrinter $ visitExpression (ExprStructInit dummyPos "P" [("x", ExprLitInt dummyPos 1)]))
  , testCase "ExprAccess" $
      assertEqual "Access" "ExprAccess .x\n  ExprVar p" 
        (runPrinter $ visitExpression (ExprAccess dummyPos (ExprVar dummyPos "p") "x"))
  , testCase "ExprLitInt" $      assertEqual "Int" "ExprLitInt 1" (runPrinter $ visitExpression (ExprLitInt dummyPos 1))
  , testCase "ExprLitFloat" $
      assertEqual "Float" "ExprLitFloat 1.5" (runPrinter $ visitExpression (ExprLitFloat dummyPos 1.5))
  , testCase "ExprLitString" $
      assertEqual "String" "ExprLitString \"s\"" (runPrinter $ visitExpression (ExprLitString dummyPos "s"))
  , testCase "ExprLitChar" $
      assertEqual "Char" "ExprLitChar 'c'" (runPrinter $ visitExpression (ExprLitChar dummyPos 'c'))
  , testCase "ExprLitBool" $
      assertEqual "Bool" "ExprLitBool True" (runPrinter $ visitExpression (ExprLitBool dummyPos True))
  , testCase "ExprLitNull" $
      assertEqual "Null" "ExprLitNull" (runPrinter $ visitExpression (ExprLitNull dummyPos))
  , testCase "ExprVar" $
      assertEqual "Var" "ExprVar x" (runPrinter $ visitExpression (ExprVar dummyPos "x"))
  ]

statementPrinterTests :: TestTree
statementPrinterTests =
  testGroup
    "Statement Printer Tests"
    [ testCase "StmtVarDecl (Full)" $
        let stmt = StmtVarDecl dummyPos "x" (Just TypeI32) (ExprLitInt dummyPos 1)
         in assertEqual "VarDecl Full" "StmtVarDecl x : i32\nValue:\n  ExprLitInt 1" (runPrinter $ visitStatement stmt)
    , testCase "StmtVarDecl (Inferred)" $
        let stmt = StmtVarDecl dummyPos "x" Nothing (ExprLitInt dummyPos 1)
         in assertEqual "VarDecl Inferred" "StmtVarDecl x\nValue:\n  ExprLitInt 1" (runPrinter $ visitStatement stmt)
  
  , testCase "StmtAssignment" $
      let stmt = StmtAssignment dummyPos (ExprVar dummyPos "x") (ExprLitInt dummyPos 1)
      in assertEqual "Assignment" "StmtAssignment\n  LValue:\n    ExprVar x\n  RValue:\n    ExprLitInt 1" (runPrinter $ visitStatement stmt)

  , testCase "StmtReturn (Just)" $
      let stmt = StmtReturn dummyPos (Just (ExprLitInt dummyPos 1))
      in assertEqual "Return Just" "StmtReturn\n  ExprLitInt 1" (runPrinter $ visitStatement stmt)
  , testCase "StmtReturn (Nothing)" $
      let stmt = StmtReturn dummyPos Nothing
      in assertEqual "Return Nothing" "StmtReturn" (runPrinter $ visitStatement stmt)

  , testCase "StmtIf (With Else)" $
      let stmt = StmtIf dummyPos (ExprLitBool dummyPos True) [StmtStop dummyPos] (Just [StmtNext dummyPos])
      in assertEqual "If Else" "StmtIf\n  Condition:\n    ExprLitBool True\n  Then:\n    StmtStop\n  Else:\n    StmtNext" (runPrinter $ visitStatement stmt)
  , testCase "StmtIf (No Else)" $
      let stmt = StmtIf dummyPos (ExprLitBool dummyPos True) [StmtStop dummyPos] Nothing
      in assertEqual "If No Else" "StmtIf\n  Condition:\n    ExprLitBool True\n  Then:\n    StmtStop" (runPrinter $ visitStatement stmt)

  , testCase "StmtFor (Full)" $
      let stmt = StmtFor dummyPos "i" (Just TypeI32) (Just (ExprLitInt dummyPos 0)) (ExprLitInt dummyPos 10) [StmtNext dummyPos]
      in assertEqual "For Full" "StmtFor i : i32\n  Start:\n    ExprLitInt 0\n  End:\n    ExprLitInt 10\n  Body:\n    StmtNext" (runPrinter $ visitStatement stmt)
  , testCase "StmtFor (Implicit Start/Type)" $
      let stmt = StmtFor dummyPos "i" Nothing Nothing (ExprLitInt dummyPos 10) [StmtNext dummyPos]
      in assertEqual "For Implicit" "StmtFor i\n  Start: <Implicit>\n  End:\n    ExprLitInt 10\n  Body:\n    StmtNext" (runPrinter $ visitStatement stmt)

  , testCase "StmtForEach (Full)" $
      let stmt = StmtForEach dummyPos "x" (Just TypeI32) (ExprVar dummyPos "xs") [StmtNext dummyPos]
      in assertEqual "ForEach Full" "StmtForEach x : i32\n  Iterable:\n    ExprVar xs\n  Body:\n    StmtNext" (runPrinter $ visitStatement stmt)
  , testCase "StmtForEach (Inferred)" $
      let stmt = StmtForEach dummyPos "x" Nothing (ExprVar dummyPos "xs") [StmtNext dummyPos]
      in assertEqual "ForEach Inferred" "StmtForEach x\n  Iterable:\n    ExprVar xs\n  Body:\n    StmtNext" (runPrinter $ visitStatement stmt)

  , testCase "StmtLoop" $
      let stmt = StmtLoop dummyPos [StmtStop dummyPos]
      in assertEqual "Loop" "StmtLoop\n  Body:\n    StmtStop" (runPrinter $ visitStatement stmt)

  , testCase "StmtStop" $
      assertEqual "Stop" "StmtStop" (runPrinter $ visitStatement (StmtStop dummyPos))

  , testCase "StmtNext" $
      assertEqual "Next" "StmtNext" (runPrinter $ visitStatement (StmtNext dummyPos))

  , testCase "StmtExpr" $
      let stmt = StmtExpr dummyPos (ExprLitInt dummyPos 1)
      in assertEqual "Expr" "StmtExpr\n  ExprLitInt 1" (runPrinter $ visitStatement stmt)
  ]

topLevelPrinterTests :: TestTree
topLevelPrinterTests = testGroup "TopLevel Printer Tests"
  [ testCase "Function" $
      let def = DefFunction "f" [] TypeNull [] False
      in assertEqual "Function" "DefFunction f\n  Parameters:\n  ReturnType: null\n  Body:" (runPrinter $ visitTopLevel def)
  , testCase "Struct" $
      let def = DefStruct "S" [Field "x" TypeI32] []
      in assertEqual "Struct" "DefStruct S\n  Fields:\n    x: i32\n  Methods:" (runPrinter $ visitTopLevel def)
  
  , testCase "visitFunction (Ignore other types)" $
      assertEqual "Ignore Struct" "" (runPrinter $ visitFunction (DefStruct "S" [] []))
  
  , testCase "visitStruct (Ignore other types)" $
      assertEqual "Ignore Function" "" (runPrinter $ visitStruct (DefFunction "f" [] TypeNull [] False))
  ]

programPrinterTests :: TestTree
programPrinterTests = testGroup "Program Printer Tests"
  [ testCase "Empty" $
      assertEqual "Empty" "Program: p" (prettyPrint (Program "p" []))
  , testCase "With Content" $
      let def = DefFunction "f" [] TypeNull [] False
      in assertEqual "Content" "Program: p\n  DefFunction f\n    Parameters:\n    ReturnType: null\n    Body:" (prettyPrint (Program "p" [def]))
  ]