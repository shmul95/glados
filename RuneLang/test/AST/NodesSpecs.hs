module AST.NodesSpecs (astNodesTests) where

import Rune.AST.Nodes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestHelpers (dummyPos)

--
-- public
--

astNodesTests :: TestTree
astNodesTests =
  testGroup
    "Rune.AST.Nodes Specs"
    [ testTypes,
      testBinaryOps,
      testUnaryOps,
      testParameterAndField,
      testProgramAccessors,
      testTopLevelDefAccessors,
      testStatementAccessors,
      testConditionalAndLoopAccessors,
      testExpressionAccessors,
      testGetExprPos,
      testGetStmtPos
    ]

--
-- helpers constants
--

dummyBlock :: Block
dummyBlock = [StmtReturn dummyPos Nothing]

dummyExpr :: Expression
dummyExpr = ExprLitInt dummyPos 42

--
-- private
--

testTypes :: TestTree
testTypes =
  testGroup
    "Type"
    [ testCase "All Type constructors and Show" $
        let types =
              [ TypeI8, TypeI16, TypeI32, TypeI64,
                TypeF32, TypeF64, TypeBool,
                TypeU8, TypeU16, TypeU32, TypeU64,
                TypeChar, TypeString, TypeAny, TypeNull,
                TypeCustom "MyStruct"
              ]
            expectedStrs =
              [ "i8", "i16", "i32", "i64",
                "f32", "f64", "bool",
                "u8", "u16", "u32", "u64",
                "char", "str", "any", "null",
                "MyStruct"
              ]
         in map show types @?= expectedStrs,
      testCase "Eq/Ord derived instances" $
        (TypeI32 == TypeI32) @?= True
    ]

testBinaryOps :: TestTree
testBinaryOps =
  testGroup
    "BinaryOp"
    [ testCase "Constructors and Show" $
        let ops = [Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Lte, Gt, Gte, And, Or]
         in length ops @?= 13
    ]

testUnaryOps :: TestTree
testUnaryOps =
  testGroup
    "UnaryOp"
    [ testCase "Constructors and Show" $
        let ops = [Negate, Not, PropagateError, PrefixInc, PrefixDec, PostfixInc, PostfixDec]
         in length ops @?= 7
    ]

testParameterAndField :: TestTree
testParameterAndField =
  testGroup
    "Parameter and Field Accessors"
    [ testCase "Parameter accessors" $
        let p = Parameter {paramName = "x", paramType = TypeI32}
         in do
              paramName p @?= "x"
              paramType p @?= TypeI32
              show p @?= "Parameter {paramName = \"x\", paramType = i32}",
      testCase "Field accessors" $
        let f = Field {fieldName = "y", fieldType = TypeF64}
         in do
              fieldName f @?= "y"
              fieldType f @?= TypeF64
              show f @?= "Field {fieldName = \"y\", fieldType = f64}"
    ]

testProgramAccessors :: TestTree
testProgramAccessors =
  testCase "Program accessors" $
    let prog = Program {programName = "Main", programDefs = []}
     in do
          programName prog @?= "Main"
          programDefs prog @?= []
          show prog @?= "Program {programName = \"Main\", programDefs = []}"

testTopLevelDefAccessors :: TestTree
testTopLevelDefAccessors =
  testGroup
    "TopLevelDef Accessors"
    [ testCase "DefFunction accessors" $
        let def =
              DefFunction
                { funcName = "main",
                  funcParams = [],
                  funcReturnType = TypeNull,
                  funcBody = dummyBlock,
                  funcIsExport = False
                }
         in do
              funcName def @?= "main"
              funcParams def @?= []
              funcReturnType def @?= TypeNull
              funcBody def @?= dummyBlock,
      testCase "DefStruct accessors" $
        let def =
              DefStruct
                { structName = "Vec2",
                  structFields = [],
                  structMethods = []
                }
         in do
              structName def @?= "Vec2"
              structFields def @?= []
              structMethods def @?= [],
      testCase "DefOverride accessors" $
        let def =
              DefOverride
                { overrideName = "toString",
                  overrideParams = [],
                  overrideReturnType = TypeString,
                  overrideBody = dummyBlock,
                  overrideIsExport = False
                }
         in do
              overrideName def @?= "toString"
              overrideParams def @?= []
              overrideReturnType def @?= TypeString
              overrideBody def @?= dummyBlock
    ]

testStatementAccessors :: TestTree
testStatementAccessors =
  testGroup
    "Statement Accessors"
    [ testCase "StmtVarDecl accessors" $
        let stmt = StmtVarDecl {stmtPos = dummyPos, varName = "x", varType = Just TypeI32, varValue = dummyExpr}
         in do
              varName stmt @?= "x"
              varType stmt @?= Just TypeI32
              varValue stmt @?= dummyExpr
    , testCase "StmtAssignment accessors" $
        let stmt = StmtAssignment {stmtPos = dummyPos, assignLValue = ExprVar dummyPos "x", assignRValue = dummyExpr}
         in do
              assignLValue stmt @?= ExprVar dummyPos "x"
              assignRValue stmt @?= dummyExpr
    , testCase "StmtReturn accessors" $
        case StmtReturn dummyPos (Just dummyExpr) of
          StmtReturn _ val -> val @?= Just dummyExpr
    ]

testConditionalAndLoopAccessors :: TestTree
testConditionalAndLoopAccessors =
  testGroup
    "Conditional and Loop Accessors"
    [ testCase "StmtIf accessors" $
        let stmt = StmtIf {stmtPos = dummyPos, ifCond = ExprLitBool dummyPos True, ifThen = dummyBlock, ifElse = Just dummyBlock}
         in do
              ifCond stmt @?= ExprLitBool dummyPos True
              ifThen stmt @?= dummyBlock
              ifElse stmt @?= Just dummyBlock
    , testCase "StmtFor accessors" $
        let stmt =
              StmtFor
                { stmtPos = dummyPos,
                  forVar = "i",
                  forVarType = Just TypeI32,
                  forStart = Just (ExprLitInt dummyPos 0),
                  forEnd = ExprLitInt dummyPos 10,
                  forBody = dummyBlock
                }
         in do
              forVar stmt @?= "i"
              forVarType stmt @?= Just TypeI32
              forStart stmt @?= Just (ExprLitInt dummyPos 0)
              forEnd stmt @?= ExprLitInt dummyPos 10
              forBody stmt @?= dummyBlock,
      testCase "StmtForEach accessors" $
        let stmt =
              StmtForEach
                { stmtPos = dummyPos,
                  forEachVar = "item",
                  forEachVarType = Nothing,
                  forEachIterable = ExprVar dummyPos "list",
                  forEachBody = dummyBlock
                }
         in do
              forEachVar stmt @?= "item"
              forEachVarType stmt @?= Nothing
              forEachIterable stmt @?= ExprVar dummyPos "list"
              forEachBody stmt @?= dummyBlock,
      testCase "StmtLoop/Stop/Next/Expr constructors" $
        let list = [StmtLoop dummyPos dummyBlock, StmtStop dummyPos, StmtNext dummyPos, StmtExpr dummyPos dummyExpr]
         in length list @?= 4
    ]

testExpressionAccessors :: TestTree
testExpressionAccessors =
  testGroup
    "Expression Accessors"
    [ testCase "ExprCall accessors" $
        let expr = ExprCall {exprPos = dummyPos, callName = ExprVar dummyPos "foo", callArgs = [dummyExpr]}
         in do
              callName expr @?= ExprVar dummyPos "foo"
              callArgs expr @?= [dummyExpr]
    , testCase "ExprStructInit accessors" $
        let expr = ExprStructInit {exprPos = dummyPos, initStructName = "Point", initFields = [("x", dummyExpr)]}
         in do
              initStructName expr @?= "Point"
              initFields expr @?= [("x", dummyExpr)]
    , testCase "ExprAccess accessors" $
        let expr = ExprAccess dummyPos (ExprVar dummyPos "p") "x"
         in do
              accessTarget expr @?= ExprVar dummyPos "p"
              accessField expr @?= "x"
    , testCase "ExprBinary/Unary/Literals constructors" $
        let exprs =
              [ ExprBinary dummyPos Add dummyExpr dummyExpr,
                ExprUnary dummyPos Negate dummyExpr,
                ExprLitInt dummyPos 1,
                ExprLitFloat dummyPos 1.0,
                ExprLitString dummyPos "s",
                ExprLitChar dummyPos 'c',
                ExprLitBool dummyPos True,
                ExprLitNull dummyPos,
                ExprVar dummyPos "x"
              ]
         in length exprs @?= 9,
      testCase "ExprIndex accessors" $
        let expr = ExprIndex {exprPos = dummyPos, indexTarget = ExprVar dummyPos "arr", indexValue = dummyExpr}
         in do
          indexTarget expr @?= ExprVar dummyPos "arr"
          indexValue expr @?= dummyExpr
    ]

testGetExprPos :: TestTree
testGetExprPos =
  testGroup
    "getExprPos"
    [ testCase "All Expression constructors" $ do
        getExprPos (ExprBinary dummyPos Add dummyExpr dummyExpr) @?= dummyPos
        getExprPos (ExprUnary dummyPos Negate dummyExpr) @?= dummyPos
        getExprPos (ExprCall dummyPos (ExprVar dummyPos "f") []) @?= dummyPos
        getExprPos (ExprStructInit dummyPos "S" []) @?= dummyPos
        getExprPos (ExprAccess dummyPos dummyExpr "x") @?= dummyPos
        getExprPos (ExprIndex dummyPos dummyExpr dummyExpr) @?= dummyPos
        getExprPos (ExprCast dummyPos dummyExpr TypeI32) @?= dummyPos
        getExprPos (ExprLitInt dummyPos 1) @?= dummyPos
        getExprPos (ExprLitFloat dummyPos 1.0) @?= dummyPos
        getExprPos (ExprLitString dummyPos "s") @?= dummyPos
        getExprPos (ExprLitChar dummyPos 'c') @?= dummyPos
        getExprPos (ExprLitBool dummyPos True) @?= dummyPos
        getExprPos (ExprLitNull dummyPos) @?= dummyPos
        getExprPos (ExprVar dummyPos "v") @?= dummyPos
        getExprPos (ExprLitArray dummyPos []) @?= dummyPos
    ]

testGetStmtPos :: TestTree
testGetStmtPos =
  testGroup
    "getStmtPos"
    [ testCase "All Statement constructors" $ do
        getStmtPos (StmtVarDecl dummyPos "x" Nothing dummyExpr) @?= dummyPos
        getStmtPos (StmtAssignment dummyPos dummyExpr dummyExpr) @?= dummyPos
        getStmtPos (StmtReturn dummyPos Nothing) @?= dummyPos
        getStmtPos (StmtIf dummyPos dummyExpr [] Nothing) @?= dummyPos
        getStmtPos (StmtFor dummyPos "i" Nothing Nothing dummyExpr []) @?= dummyPos
        getStmtPos (StmtForEach dummyPos "i" Nothing dummyExpr []) @?= dummyPos
        getStmtPos (StmtLoop dummyPos []) @?= dummyPos
        getStmtPos (StmtStop dummyPos) @?= dummyPos
        getStmtPos (StmtNext dummyPos) @?= dummyPos
        getStmtPos (StmtExpr dummyPos dummyExpr) @?= dummyPos
    ]
