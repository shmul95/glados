{-# LANGUAGE OverloadedStrings #-}

module AST.NodesSpec (astNodesTests) where

import Control.Exception (evaluate)
import Rune.AST.Nodes
import Test.Tasty
import Test.Tasty.HUnit

--
-- public
--

astNodesTests :: TestTree
astNodesTests =
  testGroup
    "Rune.AST.Nodes"
    [ typeTests,
      binaryOpTests,
      unaryOpTests,
      programTests,
      topLevelDefTests,
      statementTests,
      expressionTests,
      paramFieldTests,
      parameterTests,
      fieldTests
    ]

--
-- private
--

typeTests :: TestTree
typeTests =
  testGroup
    "Type"
    [ testCase "Eq/Show" $ do
        let t = TypeI32
        show t @?= "TypeI32"
        t @?= TypeI32,
      testCase "TypeCustom" $ do
        let t = TypeCustom "MyType"
        show t @?= "TypeCustom \"MyType\""
        t @?= TypeCustom "MyType"
    ]

binaryOpTests :: TestTree
binaryOpTests =
  testGroup
    "BinaryOp"
    [ testCase "Add" $ Add @?= Add,
      testCase "All constructors show" $
        mapM_ (\op -> evaluate (show op)) [Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Lte, Gt, Gte, And, Or]
    ]

unaryOpTests :: TestTree
unaryOpTests =
  testGroup
    "UnaryOp"
    [ testCase "Negate" $ Negate @?= Negate,
      testCase "PropagateError" $ PropagateError @?= PropagateError
    ]

programTests :: TestTree
programTests = testCase "Program constructor" $ do
  let p = Program "myProg" []
  programName p @?= "myProg"
  programDefs p @?= []

topLevelDefTests :: TestTree
topLevelDefTests =
  testGroup
    "TopLevelDef"
    [ testCase "DefFunction" $ do
        let f = DefFunction "foo" [Parameter "x" TypeI32] TypeI32 []
        funcName f @?= "foo"
        funcParams f @?= [Parameter "x" TypeI32]
        funcReturnType f @?= TypeI32
        funcBody f @?= [],
      testCase "DefStruct" $ do
        let s = DefStruct "Vec2f" [Field "x" TypeF32] []
        structName s @?= "Vec2f"
        structFields s @?= [Field "x" TypeF32]
        structMethods s @?= [],
      testCase "DefOverride" $ do
        let o = DefOverride "show" [] TypeNull []
        overrideName o @?= "show"
        overrideParams o @?= []
        overrideReturnType o @?= TypeNull
        overrideBody o @?= []
    ]

parameterTests :: TestTree
parameterTests =
  testGroup
    "Parameter"
    [ testCase "Parameter constructor" $ do
        let p = Parameter "param1" TypeI32
        paramName p @?= "param1"
        paramType p @?= TypeI32
    ]

fieldTests :: TestTree
fieldTests =
  testGroup
    "Field"
    [ testCase "Field constructor" $ do
        let f = Field "field1" TypeF64
        fieldName f @?= "field1"
        fieldType f @?= TypeF64
    ]

statementTests :: TestTree
statementTests =
  testGroup
    "Statement"
    [ testCase "StmtVarDecl" $ do
        let s = StmtVarDecl "x" (Just TypeI32) (ExprLitInt 42)
        varName s @?= "x"
        varType s @?= Just TypeI32
        varValue s @?= ExprLitInt 42,
      testCase "StmtReturn" $ StmtReturn (Just (ExprLitBool True)) @?= StmtReturn (Just (ExprLitBool True)),
      testCase "StmtIf" $ do
        let s = StmtIf (ExprLitBool True) [StmtExpr (ExprLitInt 1)] (Just [StmtExpr (ExprLitInt 0)])
        ifCond s @?= ExprLitBool True
        ifThen s @?= [StmtExpr (ExprLitInt 1)]
        ifElse s @?= Just [StmtExpr (ExprLitInt 0)],
      testCase "StmtFor" $ do
        let s = StmtFor "i" (ExprLitInt 0) (ExprLitInt 10) []
        forVar s @?= "i"
        forStart s @?= ExprLitInt 0
        forEnd s @?= ExprLitInt 10
        forBody s @?= [],
      testCase "StmtForEach" $ do
        let s = StmtForEach "item" (ExprVar "list") []
        forEachVar s @?= "item"
        forEachIterable s @?= ExprVar "list"
        forEachBody s @?= [],
      testCase "StmtExpr" $ StmtExpr (ExprLitInt 123) @?= StmtExpr (ExprLitInt 123)
    ]

expressionTests :: TestTree
expressionTests =
  testGroup
    "Expression"
    [ testCase "ExprBinary" $ ExprBinary Add (ExprLitInt 1) (ExprLitInt 2) @?= ExprBinary Add (ExprLitInt 1) (ExprLitInt 2),
      testCase "ExprUnary" $ ExprUnary Negate (ExprLitInt 5) @?= ExprUnary Negate (ExprLitInt 5),
      testCase "ExprCall" $ do
        let expr = ExprCall "foo" [ExprLitInt 1, ExprVar "x"]
        callName expr @?= "foo"
        callArgs expr @?= [ExprLitInt 1, ExprVar "x"],
      testCase "ExprStructInit" $ do
        let expr = ExprStructInit "Point" [("x", ExprLitFloat 1.0), ("y", ExprLitFloat 2.0)]
        initStructName expr @?= "Point"
        initFields expr @?= [("x", ExprLitFloat 1.0), ("y", ExprLitFloat 2.0)],
      testCase "ExprAccess" $ do
        let expr = ExprAccess (ExprVar "p") "x"
        accessTarget expr @?= ExprVar "p"
        accessField expr @?= "x",
      testCase "Literals" $ do
        evaluate (ExprLitInt 42) >>= (@?= ExprLitInt 42)
        evaluate (ExprLitFloat 3.14) >>= (@?= ExprLitFloat 3.14)
        evaluate (ExprLitString "hi") >>= (@?= ExprLitString "hi")
        evaluate (ExprLitBool True) >>= (@?= ExprLitBool True)
        evaluate ExprLitNull >>= (@?= ExprLitNull)
        evaluate (ExprVar "x") >>= (@?= ExprVar "x")
    ]

paramFieldTests :: TestTree
paramFieldTests =
  testGroup
    "Parameter/Field"
    [ testCase "Parameter" $ Parameter "p" TypeI32 @?= Parameter "p" TypeI32,
      testCase "Field" $ Field "f" TypeF64 @?= Field "f" TypeF64
    ]
