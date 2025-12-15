module AST.NodesSpecs (astNodesTests) where

import Rune.AST.Nodes
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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
      testExpressionAccessors
    ]

--
-- helpers constants
--

dummyBlock :: Block
dummyBlock = [StmtReturn Nothing]

dummyExpr :: Expression
dummyExpr = ExprLitInt 42

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
                  funcBody = dummyBlock
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
                  overrideBody = dummyBlock
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
        let stmt = StmtVarDecl {varName = "x", varType = Just TypeI32, varValue = dummyExpr}
         in do
              varName stmt @?= "x"
              varType stmt @?= Just TypeI32
              varValue stmt @?= dummyExpr,
      testCase "StmtAssignment accessors" $
        let stmt = StmtAssignment {assignLValue = ExprVar "x", assignRValue = dummyExpr}
         in do
              assignLValue stmt @?= ExprVar "x"
              assignRValue stmt @?= dummyExpr,

      testCase "StmtReturn accessors" $
        case StmtReturn (Just dummyExpr) of
          StmtReturn val -> val @?= Just dummyExpr,

      testCase "StmtIf accessors" $
        let stmt = StmtIf {ifCond = ExprLitBool True, ifThen = dummyBlock, ifElse = Just dummyBlock}
         in do
              ifCond stmt @?= ExprLitBool True
              ifThen stmt @?= dummyBlock
              ifElse stmt @?= Just dummyBlock,
      testCase "StmtFor accessors" $
        let stmt =
              StmtFor
                { forVar = "i",
                  forVarType = Just TypeI32,
                  forStart = Just (ExprLitInt 0),
                  forEnd = ExprLitInt 10,
                  forBody = dummyBlock
                }
         in do
              forVar stmt @?= "i"
              forVarType stmt @?= Just TypeI32
              forStart stmt @?= Just (ExprLitInt 0)
              forEnd stmt @?= ExprLitInt 10
              forBody stmt @?= dummyBlock,
      testCase "StmtForEach accessors" $
        let stmt =
              StmtForEach
                { forEachVar = "item",
                  forEachVarType = Nothing,
                  forEachIterable = ExprVar "list",
                  forEachBody = dummyBlock
                }
         in do
              forEachVar stmt @?= "item"
              forEachVarType stmt @?= Nothing
              forEachIterable stmt @?= ExprVar "list"
              forEachBody stmt @?= dummyBlock,
      testCase "StmtLoop/Stop/Next/Expr constructors" $
        let list = [StmtLoop dummyBlock, StmtStop, StmtNext, StmtExpr dummyExpr]
         in length list @?= 4
    ]

testExpressionAccessors :: TestTree
testExpressionAccessors =
  testGroup
    "Expression Accessors"
    [ testCase "ExprCall accessors" $
        let expr = ExprCall {callName = "foo", callArgs = [dummyExpr]}
         in do
              callName expr @?= "foo"
              callArgs expr @?= [dummyExpr],
      testCase "ExprStructInit accessors" $
        let expr = ExprStructInit {initStructName = "Point", initFields = [("x", dummyExpr)]}
         in do
              initStructName expr @?= "Point"
              initFields expr @?= [("x", dummyExpr)],
      testCase "ExprAccess accessors" $
        let expr = ExprAccess {accessTarget = ExprVar "p", accessField = "x"}
         in do
              accessTarget expr @?= ExprVar "p"
              accessField expr @?= "x",
      testCase "ExprBinary/Unary/Literals constructors" $
        let exprs =
              [ ExprBinary Add dummyExpr dummyExpr,
                ExprUnary Negate dummyExpr,
                ExprLitInt 1,
                ExprLitFloat 1.0,
                ExprLitString "s",
                ExprLitChar 'c',
                ExprLitBool True,
                ExprLitNull,
                ExprVar "x"
              ]
         in length exprs @?= 9
    ]
