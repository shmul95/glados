module AST.NodesSpecs (astNodesTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Rune.AST.Nodes
import TestHelpers (dummyPos)

--
-- public
--

astNodesTests :: TestTree
astNodesTests =
  testGroup
    "AST Nodes Tests (Rune.AST.Nodes Show/Eq/Ord/Accessors Coverage)"
    [ showTypeTests,
      typeTests,
      binaryOpTests,
      unaryOpTests,
      parameterTests,
      fieldTests,
      programTests,
      topLevelDefTests,
      statementTests,
      expressionTests,
      fieldAccessorTests
    ]

--
-- private
--

showTypeTests :: TestTree
showTypeTests = testGroup "Show Type Tests"
  [
    testCase "Show Types" $
      let types = [TypeI8, TypeI16, TypeI32, TypeI64, TypeF32, TypeF64,
                   TypeBool, TypeU8, TypeU16, TypeU32, TypeU64,
                   TypeChar, TypeString, TypeAny, TypeNull,
                   TypeCustom "Zebi"]
          expected = ["i8", "i16", "i32", "i64", "f32", "f64",
                    "bool", "u8", "u16", "u32", "u64",
                    "char", "str", "any", "null", "Zebi"]
          results = map show types
    in assertEqual "Show Type results" expected results
  ]

typeTests :: TestTree
typeTests = testGroup "Type Tests"
  [
    testCase "Show for all primitive and custom types" $ do
      assertEqual "i8" "i8" (show TypeI8)
      assertEqual "i16" "i16" (show TypeI16)
      assertEqual "i32" "i32" (show TypeI32)
      assertEqual "i64" "i64" (show TypeI64)
      assertEqual "f32" "f32" (show TypeF32)
      assertEqual "f64" "f64" (show TypeF64)
      assertEqual "bool" "bool" (show TypeBool)
      assertEqual "u8" "u8" (show TypeU8)
      assertEqual "u16" "u16" (show TypeU16)
      assertEqual "u32" "u32" (show TypeU32)
      assertEqual "u64" "u64" (show TypeU64)
      assertEqual "char" "char" (show TypeChar)
      assertEqual "str" "str" (show TypeString)
      assertEqual "any" "any" (show TypeAny)
      assertEqual "null" "null" (show TypeNull)
      assertEqual "TypeCustom" "Vec2f" (show (TypeCustom "Vec2f"))
  , testCase "Eq for Type" $ do
      assertBool "Equal primitive types" (TypeI32 == TypeI32)
      assertBool "Unequal primitive types" (TypeI32 /= TypeF32)
      assertBool "Equal custom types" (TypeCustom "A" == TypeCustom "A")
      assertBool "Unequal custom types" (TypeCustom "A" /= TypeCustom "B")
      assertBool "Primitive vs custom" (TypeI32 /= TypeCustom "I32")
  , testCase "Ord for Type" $ do
      assertBool "TypeI8 < TypeI16" (TypeI8 < TypeI16)
      assertBool "TypeI16 < TypeI32" (TypeI16 < TypeI32)
      assertBool "TypeI32 < TypeI64" (TypeI32 < TypeI64)
      assertBool "TypeI64 < TypeF32" (TypeI64 < TypeF32)
      assertBool "TypeF32 < TypeF64" (TypeF32 < TypeF64)
      assertBool "TypeF64 < TypeBool" (TypeF64 < TypeBool)
      assertBool "TypeBool < TypeU8" (TypeBool < TypeU8)
      assertBool "TypeU8 < TypeU16" (TypeU8 < TypeU16)
      assertBool "TypeU16 < TypeU32" (TypeU16 < TypeU32)
      assertBool "TypeU32 < TypeU64" (TypeU32 < TypeU64)
      assertBool "TypeU64 < TypeChar" (TypeU64 < TypeChar)
      assertBool "TypeChar < TypeString" (TypeChar < TypeString)
      assertBool "TypeString < TypeAny" (TypeString < TypeAny)
      assertBool "TypeAny < TypeNull" (TypeAny < TypeNull)
      assertBool "TypeNull < TypeCustom" (TypeNull < TypeCustom "A")
      assertBool "TypeCustom ordering" (TypeCustom "A" < TypeCustom "B")
      assertBool "TypeCustom >= TypeCustom" (TypeCustom "B" >= TypeCustom "A")
  ]

binaryOpTests :: TestTree
binaryOpTests = testGroup "BinaryOp Tests"
  [
    testCase "Show for all BinaryOps" $ do
      assertEqual "Show Add" "Add" (show Add)
      assertEqual "Show Sub" "Sub" (show Sub)
      assertEqual "Show Mul" "Mul" (show Mul)
      assertEqual "Show Div" "Div" (show Div)
      assertEqual "Show Mod" "Mod" (show Mod)
      assertEqual "Show Eq" "Eq" (show Eq)
      assertEqual "Show Neq" "Neq" (show Neq)
      assertEqual "Show Lt" "Lt" (show Lt)
      assertEqual "Show Lte" "Lte" (show Lte)
      assertEqual "Show Gt" "Gt" (show Gt)
      assertEqual "Show Gte" "Gte" (show Gte)
      assertEqual "Show And" "And" (show And)
      assertEqual "Show Or" "Or" (show Or)
  , testCase "Eq for BinaryOp" $ do
      assertBool "Equal" (Add == Add)
      assertBool "Unequal" (Add /= Sub)
      assertBool "Gt == Gt" (Gt == Gt)
      assertBool "Gt /= Gte" (Gt /= Gte)
  ]

unaryOpTests :: TestTree
unaryOpTests = testGroup "UnaryOp Tests"
  [
    testCase "Show for all UnaryOps" $ do
      assertEqual "Show Negate" "Negate" (show Negate)
      assertEqual "Show PropagateError" "PropagateError" (show PropagateError)
      assertEqual "Show PrefixInc" "PrefixInc" (show PrefixInc)
      assertEqual "Show PrefixDec" "PrefixDec" (show PrefixDec)
      assertEqual "Show PostfixInc" "PostfixInc" (show PostfixInc)
      assertEqual "Show PostfixDec" "PostfixDec" (show PostfixDec)
  , testCase "Eq for UnaryOp" $ do
      assertBool "Equal" (Negate == Negate)
      assertBool "Unequal" (Negate /= PrefixInc)
      assertBool "PostfixInc /= PostfixDec" (PostfixInc /= PostfixDec)
  ]

parameterTests :: TestTree
parameterTests = testGroup "Parameter Tests"
  [
    testCase "Show Parameter" $ 
      assertEqual "Show" "Parameter {paramName = \"x\", paramType = i32}" (show (Parameter "x" TypeI32))
  , testCase "Eq Parameter" $ do
      let p1 = Parameter "x" TypeI32
      let p2 = Parameter "x" TypeI32
      let p3 = Parameter "y" TypeI32
      let p4 = Parameter "x" TypeF32
      assertBool "Equal" (p1 == p2)
      assertBool "Unequal name" (p1 /= p3)
      assertBool "Unequal type" (p1 /= p4)
  ]

fieldTests :: TestTree
fieldTests = testGroup "Field Tests"
  [
    testCase "Show Field" $ 
      assertEqual "Show" "Field {fieldName = \"x\", fieldType = f32}" (show (Field "x" TypeF32))
  , testCase "Eq Field" $ do
      let f1 = Field "x" TypeF32
      let f2 = Field "x" TypeF32
      let f3 = Field "y" TypeF32
      let f4 = Field "x" TypeI32
      assertBool "Equal" (f1 == f2)
      assertBool "Unequal name" (f1 /= f3)
      assertBool "Unequal type" (f1 /= f4)
  ]

programTests :: TestTree
programTests = testGroup "Program Tests"
  [
    testCase "Show Program" $ 
      assertEqual "Show" "Program {programName = \"main.rune\", programDefs = []}" (show (Program "main.rune" []))
  , testCase "Eq Program" $ do
      let p1 = Program "a" []
      let p2 = Program "a" []
      let p3 = Program "b" []
      let p4 = Program "a" [DefFunction "f" [] TypeNull []]
      assertBool "Equal" (p1 == p2)
      assertBool "Unequal name" (p1 /= p3)
      assertBool "Unequal defs" (p1 /= p4)
  , testCase "Create Program" $ do
      let p = Program{programName = "test.rune", programDefs = [DefFunction "main" [] TypeNull []]}
      assertEqual "programName" "test.rune" (programName p)
      assertEqual "programDefs length" 1 (length (programDefs p))
  ]

topLevelDefTests :: TestTree
topLevelDefTests = testGroup "TopLevelDef Tests"
  [
    testCase "Show DefFunction" $ 
      assertBool "Show" (not (null (show defFunc)))
  , testCase "Show DefStruct" $ 
      assertBool "Show" (not (null (show defStruct)))
  , testCase "Show DefOverride" $ 
      assertBool "Show" (not (null (show defOverride)))
  , testCase "Eq DefFunction" $ do
      assertBool "Equal" (defFunc == defFunc)
      assertBool "Unequal name" (defFunc /= defFunc {funcName = "bar"})
      assertBool "Unequal params" (defFunc /= defFunc {funcParams = []})
      assertBool "Unequal retType" (defFunc /= defFunc {funcReturnType = TypeF64})
      assertBool "Unequal body" (defFunc /= defFunc {funcBody = []})
  , testCase "Eq DefStruct" $ do
      assertBool "Equal" (defStruct == defStruct)
      assertBool "Unequal name" (defStruct /= defStruct {structName = "Vec3f"})
      assertBool "Unequal fields" (defStruct /= defStruct {structFields = []})
      assertBool "Unequal methods" (defStruct /= defStruct {structMethods = []})
  , testCase "Eq DefOverride" $ do
      assertBool "Equal" (defOverride == defOverride)
      assertBool "Unequal name" (defOverride /= defOverride {overrideName = "display"})
      assertBool "Unequal params" (defOverride /= defOverride {overrideParams = []})
      assertBool "Unequal retType" (defOverride /= defOverride {overrideReturnType = TypeI32})
      assertBool "Unequal body" (defOverride /= defOverride {overrideBody = blockB})
  , testCase "Eq Different Constructors" $ do
      assertBool "DefFunction /= DefStruct" (defFunc /= defStruct)
      assertBool "DefStruct /= DefOverride" (defStruct /= defOverride)
  ]
  where
    param = Parameter "x" TypeI32
    field = Field "x" TypeI32
    blockA = [StmtStop dummyPos]
    blockB = [StmtNext dummyPos]
    defFunc = DefFunction "foo" [param] TypeI32 blockA
    defStruct = DefStruct "Vec2f" [field] [defFunc]
    defOverride = DefOverride "show" [param] TypeNull blockA

statementTests :: TestTree
statementTests = testGroup "Statement Tests"
  [
    testCase "Show StmtVarDecl" $ 
      assertBool "Show" (not (null (show (StmtVarDecl dummyPos "x" (Just TypeI32) exprVar))))
  , testCase "Show StmtAssignment" $ 
      assertBool "Show" (not (null (show (StmtAssignment dummyPos exprVar exprLitInt))))
  , testCase "Show StmtReturn (Just)" $ 
      assertBool "Show" (not (null (show (StmtReturn dummyPos (Just exprLitInt)))))
  , testCase "Show StmtReturn (Nothing)" $ 
      assertBool "Show" (not (null (show (StmtReturn dummyPos Nothing))))
  , testCase "Show StmtIf (Just else)" $ 
      assertBool "Show" (not (null (show (StmtIf dummyPos exprLitBool block block'))))
  , testCase "Show StmtIf (Nothing else)" $ 
      assertBool "Show" (not (null (show (StmtIf dummyPos exprLitBool block Nothing))))
  , testCase "Show StmtFor (Full)" $ 
      assertBool "Show" (not (null (show (StmtFor dummyPos "i" (Just TypeI32) (Just exprLitInt) exprLitInt block))))
  , testCase "Show StmtFor (No type/start)" $ 
      assertBool "Show" (not (null (show (StmtFor dummyPos "i" Nothing Nothing exprLitInt block))))
  , testCase "Show StmtForEach" $ 
      assertBool "Show" (not (null (show (StmtForEach dummyPos "item" Nothing exprVar block))))
  , testCase "Show StmtLoop" $ 
      assertBool "Show" (not (null (show (StmtLoop dummyPos block))))
  , testCase "Show StmtStop" $ 
      assertBool "Show" (not (null (show (StmtStop dummyPos))))
  , testCase "Show StmtNext" $ 
      assertBool "Show" (not (null (show (StmtNext dummyPos))))
  , testCase "Show StmtExpr" $ 
      assertBool "Show" (not (null (show (StmtExpr dummyPos exprLitInt))))
  , testCase "Eq StmtVarDecl" $ 
      assertBool "Unequal name" (StmtVarDecl dummyPos "x" Nothing exprLitInt /= StmtVarDecl dummyPos "y" Nothing exprLitInt)
  , testCase "Eq StmtAssignment" $ do
      let s1 = StmtAssignment dummyPos exprVar exprLitInt
      let s2 = StmtAssignment dummyPos (ExprVar dummyPos "y") exprLitInt
      let s3 = StmtAssignment dummyPos exprVar (ExprLitInt dummyPos 2)
      assertBool "Equal" (s1 == s1)
      assertBool "Unequal L" (s1 /= s2)
      assertBool "Unequal R" (s1 /= s3)
  , testCase "Eq StmtReturn" $ do
      assertBool "Eq Just" (StmtReturn dummyPos (Just exprLitInt) == StmtReturn dummyPos (Just exprLitInt))
      assertBool "Eq Nothing" (StmtReturn dummyPos Nothing == StmtReturn dummyPos Nothing)
      assertBool "Neq" (StmtReturn dummyPos (Just exprLitInt) /= StmtReturn dummyPos Nothing)
  , testCase "Eq StmtIf" $ 
      assertBool "Unequal else" (StmtIf dummyPos exprLitBool block block' /= StmtIf dummyPos exprLitBool block Nothing)
  , testCase "Eq StmtFor" $ do
      let f1 = StmtFor dummyPos "i" Nothing Nothing exprLitInt block
      let f2 = StmtFor dummyPos "j" Nothing Nothing exprLitInt block
      assertBool "Equal" (f1 == f1)
      assertBool "Unequal var" (f1 /= f2)
  , testCase "Eq StmtForEach" $ do
      let fe1 = StmtForEach dummyPos "i" Nothing exprVar block
      let fe2 = StmtForEach dummyPos "j" Nothing exprVar block
      assertBool "Equal" (fe1 == fe1)
      assertBool "Unequal var" (fe1 /= fe2)
  , testCase "Eq StmtLoop" $ 
      assertBool "Equal" (StmtLoop dummyPos block == StmtLoop dummyPos block)
  , testCase "Eq StmtStop/StmtNext" $ 
      assertBool "Unequal constructors" (StmtStop dummyPos /= StmtNext dummyPos)
  , testCase "Eq StmtExpr" $ 
      assertBool "Unequal" (StmtExpr dummyPos exprLitInt /= StmtExpr dummyPos (ExprLitInt dummyPos 999))
  ]
  where
    exprLitInt = ExprLitInt dummyPos 1
    exprLitBool = ExprLitBool dummyPos True
    exprVar = ExprVar dummyPos "list"
    block = [StmtStop dummyPos]
    block' = Just [StmtNext dummyPos]

expressionTests :: TestTree
expressionTests = testGroup "Expression Tests"
  [
    testCase "Show ExprBinary" $ 
      assertBool "Show" (not (null (show (ExprBinary dummyPos Add exprLitInt exprLitInt))))
  , testCase "Show ExprUnary" $ 
      assertBool "Show" (not (null (show (ExprUnary dummyPos Negate exprLitInt))))
  , testCase "Show ExprCall" $ 
      assertBool "Show" (not (null (show (ExprCall dummyPos "foo" [exprLitInt]))))
  , testCase "Show ExprStructInit" $ 
      assertBool "Show" (not (null (show (ExprStructInit dummyPos "Vec2f" [("x", exprLitInt)]))))
  , testCase "Show ExprAccess" $ 
      assertBool "Show" (not (null (show (ExprAccess dummyPos exprVar "field"))))
  , testCase "Show ExprLitInt" $ 
      assertBool "Show" (not (null (show exprLitInt)))
  , testCase "Show ExprLitFloat" $ 
      assertBool "Show" (not (null (show (ExprLitFloat dummyPos 3.14))))
  , testCase "Show ExprLitString" $ 
      assertBool "Show" (not (null (show (ExprLitString dummyPos "hi"))))
  , testCase "Show ExprLitChar" $ 
      assertBool "Show" (not (null (show (ExprLitChar dummyPos 'a'))))
  , testCase "Show ExprLitBool" $ 
      assertBool "Show" (not (null (show (ExprLitBool dummyPos False))))
  , testCase "Show ExprLitNull" $ 
      assertBool "Show" (not (null (show (ExprLitNull dummyPos))))
  , testCase "Show ExprVar" $ 
      assertBool "Show" (not (null (show exprVar)))
  , testCase "Eq ExprBinary" $ 
      assertBool "Unequal op" (ExprBinary dummyPos Add exprLitInt exprLitInt /= ExprBinary dummyPos Sub exprLitInt exprLitInt)
  , testCase "Eq ExprUnary" $ 
      assertBool "Unequal op" (ExprUnary dummyPos Negate exprLitInt /= ExprUnary dummyPos PropagateError exprLitInt)
  , testCase "Eq ExprCall" $ 
      assertBool "Unequal name" (ExprCall dummyPos "foo" [] /= ExprCall dummyPos "bar" [])
  , testCase "Eq ExprStructInit" $ 
      assertBool "Unequal fields" (ExprStructInit dummyPos "V" [("x", exprLitInt)] /= ExprStructInit dummyPos "V" [("y", exprLitInt)])
  , testCase "Eq ExprAccess" $ 
      assertBool "Unequal target" (ExprAccess dummyPos exprVar "f" /= ExprAccess dummyPos (ExprLitInt dummyPos 0) "f")
  , testCase "Eq ExprVar" $ 
      assertBool "Unequal var" (ExprVar dummyPos "a" /= ExprVar dummyPos "b")
  , testCase "Eq ExprLitInt" $ 
      assertBool "Unequal val" (ExprLitInt dummyPos 1 /= ExprLitInt dummyPos 2)
  , testCase "Eq ExprLitFloat" $ 
      assertBool "Unequal val" (ExprLitFloat dummyPos 1.0 /= ExprLitFloat dummyPos 2.0)
  , testCase "Eq ExprLitString" $ 
      assertBool "Unequal val" (ExprLitString dummyPos "a" /= ExprLitString dummyPos "b")
  , testCase "Eq ExprLitChar" $ 
      assertBool "Unequal val" (ExprLitChar dummyPos 'a' /= ExprLitChar dummyPos 'b')
  , testCase "Eq ExprLitBool" $ 
      assertBool "Unequal val" (ExprLitBool dummyPos True /= ExprLitBool dummyPos False)
  , testCase "Eq ExprLitNull" $ 
      assertBool "Equal" (ExprLitNull dummyPos == ExprLitNull dummyPos)
  , testCase "Eq Different Constructors" $ 
      assertBool "ExprLitInt /= ExprLitFloat" (exprLitInt /= ExprLitFloat dummyPos 1.0)
  ]
  where
    exprLitInt = ExprLitInt dummyPos 1
    exprVar = ExprVar dummyPos "v"

fieldAccessorTests :: TestTree
fieldAccessorTests = testGroup "Field Accessor Tests"
  [
    testCase "Program accessors" $ do
      let p = Program {programName = "name", programDefs = []}
      case p of
        Program {programName = name, programDefs = defs} -> do
          assertEqual "programName" "name" name
          assertEqual "programDefs" [] defs
  
  , testCase "Parameter accessors" $ do
      let p = Parameter {paramName = "n", paramType = TypeI32}
      case p of
        Parameter {paramName = name, paramType = ptype} -> do
          assertEqual "paramName" "n" name
          assertEqual "paramType" TypeI32 ptype
  
  , testCase "Field accessors" $ do
      let f = Field {fieldName = "n", fieldType = TypeF32}
      case f of
        Field {fieldName = name, fieldType = ftype} -> do
          assertEqual "fieldName" "n" name
          assertEqual "fieldType" TypeF32 ftype
  
  , testCase "TopLevelDef accessors" $ do
      let func = DefFunction {funcName = "f", funcParams = [], funcReturnType = TypeNull, funcBody = []}
      case func of
        DefFunction {funcName = name, funcParams = params, funcReturnType = retType, funcBody = body} -> do
          assertEqual "funcName" "f" name
          assertEqual "funcParams" [] params
          assertEqual "funcReturnType" TypeNull retType
          assertEqual "funcBody" [] body
        _ -> assertFailure "Expected DefFunction"

      let st = DefStruct {structName = "S", structFields = [], structMethods = []}
      case st of
        DefStruct {structName = name, structFields = fields, structMethods = methods} -> do
          assertEqual "structName" "S" name
          assertEqual "structFields" [] fields
          assertEqual "structMethods" [] methods
        _ -> assertFailure "Expected DefStruct"

      let ov = DefOverride {overrideName = "o", overrideParams = [], overrideReturnType = TypeNull, overrideBody = []}
      case ov of
        DefOverride {overrideName = name, overrideParams = params, overrideReturnType = retType, overrideBody = body} -> do
          assertEqual "overrideName" "o" name
          assertEqual "overrideParams" [] params
          assertEqual "overrideReturnType" TypeNull retType
          assertEqual "overrideBody" [] body
        _ -> assertFailure "Expected DefOverride"
  
  , testCase "Statement accessors" $ do
      let vd = StmtVarDecl {stmtPos = dummyPos, varName = "x", varType = Just TypeI32, varValue = ExprLitInt dummyPos 0}
      case vd of
        StmtVarDecl {varName = name, varType = vtype, varValue = value} -> do
          assertEqual "varName" "x" name
          assertEqual "varType" (Just TypeI32) vtype
          assertEqual "varValue" (ExprLitInt dummyPos 0) value
        _ -> assertFailure "Expected StmtVarDecl"

      let assign = StmtAssignment {stmtPos = dummyPos, assignLValue = ExprVar dummyPos "x", assignRValue = ExprLitInt dummyPos 1}
      case assign of
        StmtAssignment {assignLValue = lval, assignRValue = rval} -> do
          assertEqual "assignLValue" (ExprVar dummyPos "x") lval
          assertEqual "assignRValue" (ExprLitInt dummyPos 1) rval
        _ -> assertFailure "Expected StmtAssignment"

      let ret = StmtReturn dummyPos (Just (ExprLitInt dummyPos 42))
      case ret of
        StmtReturn _ x -> assertEqual "StmtReturn value" (Just (ExprLitInt dummyPos 42)) x
        _ -> assertFailure "Expected StmtReturn"

      let ifStmt = StmtIf {stmtPos = dummyPos, ifCond = ExprLitBool dummyPos True, ifThen = [StmtStop dummyPos], ifElse = Just [StmtNext dummyPos]}
      case ifStmt of
        StmtIf {ifCond = cond, ifThen = thenBlock, ifElse = elseBlock} -> do
          assertEqual "ifCond" (ExprLitBool dummyPos True) cond
          assertEqual "ifThen" [StmtStop dummyPos] thenBlock
          assertEqual "ifElse" (Just [StmtNext dummyPos]) elseBlock
        _ -> assertFailure "Expected StmtIf"

      let forStmt = StmtFor {stmtPos = dummyPos, forVar = "i", forVarType = Just TypeI32, forStart = Just (ExprLitInt dummyPos 0), forEnd = ExprLitInt dummyPos 10, forBody = []}
      case forStmt of
        StmtFor {forVar = var, forVarType = vtype, forStart = start, forEnd = end, forBody = body} -> do
          assertEqual "forVar" "i" var
          assertEqual "forVarType" (Just TypeI32) vtype
          assertEqual "forStart" (Just (ExprLitInt dummyPos 0)) start
          assertEqual "forEnd" (ExprLitInt dummyPos 10) end
          assertEqual "forBody" [] body
        _ -> assertFailure "Expected StmtFor"

      let forEach = StmtForEach {stmtPos = dummyPos, forEachVar = "i", forEachVarType = Just TypeI32, forEachIterable = ExprVar dummyPos "list", forEachBody = []}
      case forEach of
        StmtForEach {forEachVar = var, forEachVarType = vtype, forEachIterable = iter, forEachBody = body} -> do
          assertEqual "forEachVar" "i" var
          assertEqual "forEachVarType" (Just TypeI32) vtype
          assertEqual "forEachIterable" (ExprVar dummyPos "list") iter
          assertEqual "forEachBody" [] body
        _ -> assertFailure "Expected StmtForEach"

      let loopStmt = StmtLoop dummyPos [StmtStop dummyPos]
      case loopStmt of
        StmtLoop _ body -> assertEqual "StmtLoop body" [StmtStop dummyPos] body
        _ -> assertFailure "Expected StmtLoop"

      let exprStmt = StmtExpr dummyPos (ExprLitInt dummyPos 1)
      case exprStmt of
        StmtExpr _ expr -> assertEqual "StmtExpr value" (ExprLitInt dummyPos 1) expr
        _ -> assertFailure "Expected StmtExpr"

  , testCase "Expression accessors" $ do
      let bin = ExprBinary dummyPos Add (ExprLitInt dummyPos 1) (ExprLitInt dummyPos 2)
      case bin of
        ExprBinary _ op left right -> do
          assertEqual "ExprBinary (op)" Add op
          assertEqual "ExprBinary (left)" (ExprLitInt dummyPos 1) left
          assertEqual "ExprBinary (right)" (ExprLitInt dummyPos 2) right
        _ -> assertFailure "Expected ExprBinary"

      let un = ExprUnary dummyPos Negate (ExprLitInt dummyPos 1)
      case un of
        ExprUnary _ op value -> do
          assertEqual "ExprUnary (op)" Negate op
          assertEqual "ExprUnary (value)" (ExprLitInt dummyPos 1) value
        _ -> assertFailure "Expected ExprUnary"

      let call = ExprCall {exprPos = dummyPos, callName = "f", callArgs = [ExprLitInt dummyPos 1]}
      case call of
        ExprCall {callName = name, callArgs = args} -> do
          assertEqual "callName" "f" name
          assertEqual "callArgs" [ExprLitInt dummyPos 1] args
        _ -> assertFailure "Expected ExprCall"

      let initExpr = ExprStructInit {exprPos = dummyPos, initStructName = "S", initFields = [("x", ExprLitInt dummyPos 1)]}
      case initExpr of
        ExprStructInit {initStructName = name, initFields = fields} -> do
          assertEqual "initStructName" "S" name
          assertEqual "initFields" [("x", ExprLitInt dummyPos 1)] fields
        _ -> assertFailure "Expected ExprStructInit"

      let acc = ExprAccess {exprPos = dummyPos, accessTarget = ExprVar dummyPos "x", accessField = "y"}
      case acc of
        ExprAccess {accessTarget = target, accessField = field} -> do
          assertEqual "accessTarget" (ExprVar dummyPos "x") target
          assertEqual "accessField" "y" field
        _ -> assertFailure "Expected ExprAccess"
  ]
