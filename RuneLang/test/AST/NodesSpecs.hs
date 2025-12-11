module AST.NodesSpecs (astNodesTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Rune.AST.Nodes

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
    blockA = [StmtStop]
    blockB = [StmtNext]
    defFunc = DefFunction "foo" [param] TypeI32 blockA
    defStruct = DefStruct "Vec2f" [field] [defFunc]
    defOverride = DefOverride "show" [param] TypeNull blockA

statementTests :: TestTree
statementTests = testGroup "Statement Tests"
  [
    testCase "Show StmtVarDecl" $ 
      assertBool "Show" (not (null (show (StmtVarDecl "x" (Just TypeI32) exprVar))))
  , testCase "Show StmtAssignment" $ 
      assertBool "Show" (not (null (show (StmtAssignment exprVar exprLitInt))))
  , testCase "Show StmtReturn (Just)" $ 
      assertBool "Show" (not (null (show (StmtReturn (Just exprLitInt)))))
  , testCase "Show StmtReturn (Nothing)" $ 
      assertBool "Show" (not (null (show (StmtReturn Nothing))))
  , testCase "Show StmtIf (Just else)" $ 
      assertBool "Show" (not (null (show (StmtIf exprLitBool block block'))))
  , testCase "Show StmtIf (Nothing else)" $ 
      assertBool "Show" (not (null (show (StmtIf exprLitBool block Nothing))))
  , testCase "Show StmtFor (Full)" $ 
      assertBool "Show" (not (null (show (StmtFor "i" (Just TypeI32) (Just exprLitInt) exprLitInt block))))
  , testCase "Show StmtFor (No type/start)" $ 
      assertBool "Show" (not (null (show (StmtFor "i" Nothing Nothing exprLitInt block))))
  , testCase "Show StmtForEach" $ 
      assertBool "Show" (not (null (show (StmtForEach "item" Nothing exprVar block))))
  , testCase "Show StmtLoop" $ 
      assertBool "Show" (not (null (show (StmtLoop block))))
  , testCase "Show StmtStop" $ 
      assertBool "Show" (not (null (show StmtStop)))
  , testCase "Show StmtNext" $ 
      assertBool "Show" (not (null (show StmtNext)))
  , testCase "Show StmtExpr" $ 
      assertBool "Show" (not (null (show (StmtExpr exprLitInt))))
  , testCase "Eq StmtVarDecl" $ 
      assertBool "Unequal name" (StmtVarDecl "x" Nothing exprLitInt /= StmtVarDecl "y" Nothing exprLitInt)
  , testCase "Eq StmtAssignment" $ do
      let s1 = StmtAssignment exprVar exprLitInt
      let s2 = StmtAssignment (ExprVar "y") exprLitInt
      let s3 = StmtAssignment exprVar (ExprLitInt 2)
      assertBool "Equal" (s1 == s1)
      assertBool "Unequal L" (s1 /= s2)
      assertBool "Unequal R" (s1 /= s3)
  , testCase "Eq StmtReturn" $ do
      assertBool "Eq Just" (StmtReturn (Just exprLitInt) == StmtReturn (Just exprLitInt))
      assertBool "Eq Nothing" (StmtReturn Nothing == StmtReturn Nothing)
      assertBool "Neq" (StmtReturn (Just exprLitInt) /= StmtReturn Nothing)
  , testCase "Eq StmtIf" $ 
      assertBool "Unequal else" (StmtIf exprLitBool block block' /= StmtIf exprLitBool block Nothing)
  , testCase "Eq StmtFor" $ do
      let f1 = StmtFor "i" Nothing Nothing exprLitInt block
      let f2 = StmtFor "j" Nothing Nothing exprLitInt block
      assertBool "Equal" (f1 == f1)
      assertBool "Unequal var" (f1 /= f2)
  , testCase "Eq StmtForEach" $ do
      let fe1 = StmtForEach "i" Nothing exprVar block
      let fe2 = StmtForEach "j" Nothing exprVar block
      assertBool "Equal" (fe1 == fe1)
      assertBool "Unequal var" (fe1 /= fe2)
  , testCase "Eq StmtLoop" $ 
      assertBool "Equal" (StmtLoop block == StmtLoop block)
  , testCase "Eq StmtStop/StmtNext" $ 
      assertBool "Unequal constructors" (StmtStop /= StmtNext)
  , testCase "Eq StmtExpr" $ 
      assertBool "Unequal" (StmtExpr exprLitInt /= StmtExpr (ExprLitInt 999))
  ]
  where
    exprLitInt = ExprLitInt 1
    exprLitBool = ExprLitBool True
    exprVar = ExprVar "list"
    block = [StmtStop]
    block' = Just [StmtNext]

expressionTests :: TestTree
expressionTests = testGroup "Expression Tests"
  [
    testCase "Show ExprBinary" $ 
      assertBool "Show" (not (null (show (ExprBinary Add exprLitInt exprLitInt))))
  , testCase "Show ExprUnary" $ 
      assertBool "Show" (not (null (show (ExprUnary Negate exprLitInt))))
  , testCase "Show ExprCall" $ 
      assertBool "Show" (not (null (show (ExprCall "foo" [exprLitInt]))))
  , testCase "Show ExprStructInit" $ 
      assertBool "Show" (not (null (show (ExprStructInit "Vec2f" [("x", exprLitInt)]))))
  , testCase "Show ExprAccess" $ 
      assertBool "Show" (not (null (show (ExprAccess exprVar "field"))))
  , testCase "Show ExprLitInt" $ 
      assertBool "Show" (not (null (show exprLitInt)))
  , testCase "Show ExprLitFloat" $ 
      assertBool "Show" (not (null (show (ExprLitFloat 3.14))))
  , testCase "Show ExprLitString" $ 
      assertBool "Show" (not (null (show (ExprLitString "hi"))))
  , testCase "Show ExprLitChar" $ 
      assertBool "Show" (not (null (show (ExprLitChar 'a'))))
  , testCase "Show ExprLitBool" $ 
      assertBool "Show" (not (null (show (ExprLitBool False))))
  , testCase "Show ExprLitNull" $ 
      assertBool "Show" (not (null (show ExprLitNull)))
  , testCase "Show ExprVar" $ 
      assertBool "Show" (not (null (show exprVar)))
  , testCase "Eq ExprBinary" $ 
      assertBool "Unequal op" (ExprBinary Add exprLitInt exprLitInt /= ExprBinary Sub exprLitInt exprLitInt)
  , testCase "Eq ExprUnary" $ 
      assertBool "Unequal op" (ExprUnary Negate exprLitInt /= ExprUnary PropagateError exprLitInt)
  , testCase "Eq ExprCall" $ 
      assertBool "Unequal name" (ExprCall "foo" [] /= ExprCall "bar" [])
  , testCase "Eq ExprStructInit" $ 
      assertBool "Unequal fields" (ExprStructInit "V" [("x", exprLitInt)] /= ExprStructInit "V" [("y", exprLitInt)])
  , testCase "Eq ExprAccess" $ 
      assertBool "Unequal target" (ExprAccess exprVar "f" /= ExprAccess (ExprLitInt 0) "f")
  , testCase "Eq ExprVar" $ 
      assertBool "Unequal var" (ExprVar "a" /= ExprVar "b")
  , testCase "Eq ExprLitInt" $ 
      assertBool "Unequal val" (ExprLitInt 1 /= ExprLitInt 2)
  , testCase "Eq ExprLitFloat" $ 
      assertBool "Unequal val" (ExprLitFloat 1.0 /= ExprLitFloat 2.0)
  , testCase "Eq ExprLitString" $ 
      assertBool "Unequal val" (ExprLitString "a" /= ExprLitString "b")
  , testCase "Eq ExprLitChar" $ 
      assertBool "Unequal val" (ExprLitChar 'a' /= ExprLitChar 'b')
  , testCase "Eq ExprLitBool" $ 
      assertBool "Unequal val" (ExprLitBool True /= ExprLitBool False)
  , testCase "Eq ExprLitNull" $ 
      assertBool "Equal" (ExprLitNull == ExprLitNull)
  , testCase "Eq Different Constructors" $ 
      assertBool "ExprLitInt /= ExprLitFloat" (exprLitInt /= ExprLitFloat 1.0)
  ]
  where
    exprLitInt = ExprLitInt 1
    exprVar = ExprVar "v"

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
      let vd = StmtVarDecl {varName = "x", varType = Just TypeI32, varValue = ExprLitInt 0}
      case vd of
        StmtVarDecl {varName = name, varType = vtype, varValue = value} -> do
          assertEqual "varName" "x" name
          assertEqual "varType" (Just TypeI32) vtype
          assertEqual "varValue" (ExprLitInt 0) value
        _ -> assertFailure "Expected StmtVarDecl"

      let assign = StmtAssignment {assignLValue = ExprVar "x", assignRValue = ExprLitInt 1}
      case assign of
        StmtAssignment {assignLValue = lval, assignRValue = rval} -> do
          assertEqual "assignLValue" (ExprVar "x") lval
          assertEqual "assignRValue" (ExprLitInt 1) rval
        _ -> assertFailure "Expected StmtAssignment"

      let ret = StmtReturn (Just (ExprLitInt 42))
      case ret of
        StmtReturn x -> assertEqual "StmtReturn value" (Just (ExprLitInt 42)) x
        _ -> assertFailure "Expected StmtReturn"

      let ifStmt = StmtIf {ifCond = ExprLitBool True, ifThen = [StmtStop], ifElse = Just [StmtNext]}
      case ifStmt of
        StmtIf {ifCond = cond, ifThen = thenBlock, ifElse = elseBlock} -> do
          assertEqual "ifCond" (ExprLitBool True) cond
          assertEqual "ifThen" [StmtStop] thenBlock
          assertEqual "ifElse" (Just [StmtNext]) elseBlock
        _ -> assertFailure "Expected StmtIf"

      let forStmt = StmtFor {forVar = "i", forVarType = Just TypeI32, forStart = Just (ExprLitInt 0), forEnd = ExprLitInt 10, forBody = []}
      case forStmt of
        StmtFor {forVar = var, forVarType = vtype, forStart = start, forEnd = end, forBody = body} -> do
          assertEqual "forVar" "i" var
          assertEqual "forVarType" (Just TypeI32) vtype
          assertEqual "forStart" (Just (ExprLitInt 0)) start
          assertEqual "forEnd" (ExprLitInt 10) end
          assertEqual "forBody" [] body
        _ -> assertFailure "Expected StmtFor"

      let forEach = StmtForEach {forEachVar = "i", forEachVarType = Just TypeI32, forEachIterable = ExprVar "list", forEachBody = []}
      case forEach of
        StmtForEach {forEachVar = var, forEachVarType = vtype, forEachIterable = iter, forEachBody = body} -> do
          assertEqual "forEachVar" "i" var
          assertEqual "forEachVarType" (Just TypeI32) vtype
          assertEqual "forEachIterable" (ExprVar "list") iter
          assertEqual "forEachBody" [] body
        _ -> assertFailure "Expected StmtForEach"

      let loopStmt = StmtLoop [StmtStop]
      case loopStmt of
        StmtLoop body -> assertEqual "StmtLoop body" [StmtStop] body
        _ -> assertFailure "Expected StmtLoop"

      let exprStmt = StmtExpr (ExprLitInt 1)
      case exprStmt of
        StmtExpr expr -> assertEqual "StmtExpr value" (ExprLitInt 1) expr
        _ -> assertFailure "Expected StmtExpr"

  , testCase "Expression accessors" $ do
      let bin = ExprBinary Add (ExprLitInt 1) (ExprLitInt 2)
      case bin of
        ExprBinary op left right -> do
          assertEqual "ExprBinary (op)" Add op
          assertEqual "ExprBinary (left)" (ExprLitInt 1) left
          assertEqual "ExprBinary (right)" (ExprLitInt 2) right
        _ -> assertFailure "Expected ExprBinary"

      let un = ExprUnary Negate (ExprLitInt 1)
      case un of
        ExprUnary op value -> do
          assertEqual "ExprUnary (op)" Negate op
          assertEqual "ExprUnary (value)" (ExprLitInt 1) value
        _ -> assertFailure "Expected ExprUnary"

      let call = ExprCall {callName = "f", callArgs = [ExprLitInt 1]}
      case call of
        ExprCall {callName = name, callArgs = args} -> do
          assertEqual "callName" "f" name
          assertEqual "callArgs" [ExprLitInt 1] args
        _ -> assertFailure "Expected ExprCall"

      let initExpr = ExprStructInit {initStructName = "S", initFields = [("x", ExprLitInt 1)]}
      case initExpr of
        ExprStructInit {initStructName = name, initFields = fields} -> do
          assertEqual "initStructName" "S" name
          assertEqual "initFields" [("x", ExprLitInt 1)] fields
        _ -> assertFailure "Expected ExprStructInit"

      let acc = ExprAccess {accessTarget = ExprVar "x", accessField = "y"}
      case acc of
        ExprAccess {accessTarget = target, accessField = field} -> do
          assertEqual "accessTarget" (ExprVar "x") target
          assertEqual "accessField" "y" field
        _ -> assertFailure "Expected ExprAccess"
  ]
