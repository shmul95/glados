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
    [ testSourcePos,
      testTypes,
      testBinaryOps,
      testUnaryOps,
      testParameterAndField,
      testProgramAccessors,
      testTopLevelDefAccessors,
      testStatementAccessors,
      testConditionalAndLoopAccessors,
      testExpressionAccessors,
      testFunctionSignature,
      testGetExprPos,
      testGetStmtPos,
      testOrdInstances
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

testSourcePos :: TestTree
testSourcePos =
  testGroup
    "SourcePos"
    [ testCase "Accessors and Show" $ do
        let pos = SourcePos "test.rn" 10 5
        posFile pos @?= "test.rn"
        posLine pos @?= 10
        posCol pos @?= 5
        show pos @?= "test.rn:10:5"
    ]

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
                TypeCustom "MyStruct",
                TypeArray TypeI32, TypePtr TypeI32, TypeRef TypeI32, TypeVariadic TypeI32
              ]
            expectedStrs =
              [ "i8", "i16", "i32", "i64",
                "f32", "f64", "bool",
                "u8", "u16", "u32", "u64",
                "char", "str", "any", "null",
                "MyStruct",
                "arri32", "ptr_i32", "ref_i32", "...i32"
              ]
         in map show types @?= expectedStrs,
      testCase "Eq instance" $
        (TypeI32 == TypeI32) @?= True
    ]

testBinaryOps :: TestTree
testBinaryOps =
  testGroup
    "BinaryOp"
    [ testCase "Constructors and Show" $
        let ops = [Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Lte, Gt, Gte, And, Or, BitAnd]
         in do
           length ops @?= 14
           show Add @?= "Add"
    ]

testUnaryOps :: TestTree
testUnaryOps =
  testGroup
    "UnaryOp"
    [ testCase "Constructors and Show" $
        let ops = [Negate, Not, BitNot, PropagateError, PrefixInc, PrefixDec, PostfixInc, PostfixDec]
         in do
           length ops @?= 8
           show Negate @?= "Negate"
    ]

testParameterAndField :: TestTree
testParameterAndField =
  testGroup
    "Parameter and Field Accessors"
    [ testCase "Parameter accessors" $
        let p = Parameter {paramName = "x", paramType = TypeI32, paramDefault = Just dummyExpr}
         in do
              paramName p @?= "x"
              paramType p @?= TypeI32
              paramDefault p @?= Just dummyExpr
              show p @?= "Parameter {paramName = \"x\", paramType = i32, paramDefault = Just (ExprLitInt test.ru:0:0 42)}",
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
                  funcIsExport = True
                }
         in do
              funcName def @?= "main"
              funcParams def @?= []
              funcReturnType def @?= TypeNull
              funcBody def @?= dummyBlock
              funcIsExport def @?= True
              show def @?= "DefFunction {funcName = \"main\", funcParams = [], funcReturnType = null, funcBody = [StmtReturn test.ru:0:0 Nothing], funcIsExport = True}",
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
              structMethods def @?= []
    , testCase "DefSomewhere accessors" $
        let sig = FunctionSignature "foo" [] TypeNull False
            def = DefSomewhere [DeclFuncSig sig]
         in do
              somewhereDecls def @?= [DeclFuncSig sig]
    ]

testFunctionSignature :: TestTree
testFunctionSignature =
  testCase "FunctionSignature accessors" $
    let sig = FunctionSignature 
                { sigFuncName = "ext", 
                  sigParams = [TypeI32], 
                  sigReturnType = TypeBool, 
                  sigIsExtern = True 
                }
    in do
      sigFuncName sig @?= "ext"
      sigParams sig @?= [TypeI32]
      sigReturnType sig @?= TypeBool
      sigIsExtern sig @?= True
      (sig == sig) @?= True

testStatementAccessors :: TestTree
testStatementAccessors =
  testGroup
    "Statement Accessors"
    [ testCase "StmtVarDecl accessors" $
        let stmt = StmtVarDecl {stmtPos = dummyPos, varName = "x", varType = Just TypeI32, varValue = dummyExpr}
         in do
              stmtPos (stmt :: Statement) @?= dummyPos
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
          StmtReturn pos val -> do
            pos @?= dummyPos
            val @?= Just dummyExpr
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
              forEachBody stmt @?= dummyBlock
    ]

testExpressionAccessors :: TestTree
testExpressionAccessors =
  testGroup
    "Expression Accessors"
    [ testCase "ExprCall accessors" $
        let expr = ExprCall {exprPos = dummyPos, callName = ExprVar dummyPos "foo", callArgs = [dummyExpr]}
         in do
              exprPos (expr :: Expression) @?= dummyPos -- Appel explicite
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
    , testCase "ExprIndex accessors" $
        let expr = ExprIndex {exprPos = dummyPos, indexTarget = ExprVar dummyPos "arr", indexValue = dummyExpr}
         in do
          indexTarget expr @?= ExprVar dummyPos "arr"
          indexValue expr @?= dummyExpr
    , testCase "ExprCast accessors" $
        let expr = ExprCast dummyPos dummyExpr TypeF32
        in do
          castExpr expr @?= dummyExpr
          castType expr @?= TypeF32
    ]

testGetExprPos :: TestTree
testGetExprPos =
  testCase "getExprPos for all constructors" $ do
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
    getExprPos (ExprSizeof dummyPos (Left TypeI32)) @?= dummyPos

testGetStmtPos :: TestTree
testGetStmtPos =
  testCase "getStmtPos for all constructors" $ do
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

testOrdInstances :: TestTree
testOrdInstances =
  testCase "Ord instances coverage" $ do
    compare (SourcePos "a" 1 1) (SourcePos "b" 1 1) @?= LT
    compare TypeI8 TypeI16 @?= LT
