module AST.Parser.ParseTopLevelSpecs (parseTopLevelTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Data.List (isInfixOf)
import Rune.AST.Parser.ParseTopLevel
import Rune.AST.Types (Parser (..), ParserState (..))
import Rune.AST.Nodes
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseTopLevelTests :: TestTree
parseTopLevelTests =
  testGroup
    "AST Parser ParseTopLevel Specs"
    [ testParseTopLevels,
      testParseTopLevelDef,
      testParseExportedDef,
      testParseFunction,
      testParseStruct,
      testParseStructBody,
      testParseStructItem,
      testParseParams,
      testParseParameter,
      testParseSelfParam,
      testParseTypedParam,
      testParseReturnType,
      testParseField,
      testParseSomewhere,
      testParseFunctionSignatures,
      testParseFunctionSignature,
      testParseParamTypeInSignature,
      testParseSomewhereDecl,
      testStructureSignature
    ]

--
-- helpers
--

tok :: T.TokenKind -> T.Token
tok k = T.Token k "" 1 1

run :: Parser a -> [T.Token] -> Either String a
run p ts =
  let state = ParserState (ts ++ [tok T.EOF]) 0 "test" 0
   in case runParser p state of
        Right (res, _) -> Right res
        Left err -> Left err

assertS :: (Eq a, Show a) => String -> Parser a -> [T.Token] -> a -> IO ()
assertS msg p tokens expected =
  case run p tokens of
    Right actual -> assertEqual msg expected actual
    Left err -> assertBool ("Parse failed: " ++ err) False

assertF :: Show a => String -> Parser a -> [T.Token] -> IO ()
assertF msg p tokens =
  case run p tokens of
    Left _ -> pure ()
    Right res -> assertBool (msg ++ " (Expected failure but got: " ++ show res ++ ")") False

--
-- tests
--

testParseTopLevels :: TestTree
testParseTopLevels = testGroup "parseTopLevels"
  [ testCase "EOF branch (True)" $ assertS "empty" parseTopLevels [] []
  , testCase "EOF branch (False)" $ assertS "not empty" parseTopLevels 
      [tok T.KwSomewhere, tok T.LBrace, tok T.RBrace] [DefSomewhere []]
  ]

testParseTopLevelDef :: TestTree
testParseTopLevelDef = testGroup "parseTopLevelDef"
  [ testCase "KwExport" $ assertS "export" parseTopLevelDef [tok T.KwExport, tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.LBrace, tok T.RBrace] (DefFunction "f" [] TypeNull [] True)
  , testCase "KwDef"    $ assertS "def"    parseTopLevelDef [tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.LBrace, tok T.RBrace] (DefFunction "f" [] TypeNull [] False)
  , testCase "KwStruct" $ assertS "struct" parseTopLevelDef [tok T.KwStruct, tok (T.Identifier "S"), tok T.LBrace, tok T.RBrace] (DefStruct "S" [] [])
  , testCase "KwSomewhere"$ assertS "somewhere" parseTopLevelDef [tok T.KwSomewhere, tok T.LBrace, tok T.RBrace] (DefSomewhere [])
  , testCase "Fallthrough error" $ assertF "error msg" parseTopLevelDef [tok T.Semicolon]
  ]

testParseExportedDef :: TestTree
testParseExportedDef = testGroup "parseExportedDef"
  [ testCase "KwDef"      $ assertS "export def" parseExportedDef [tok T.KwExport, tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.LBrace, tok T.RBrace] (DefFunction "f" [] TypeNull [] True)
  , testCase "Fallthrough error" $ assertF "error after export" parseExportedDef [tok T.KwExport, tok T.KwStruct]
  ]

testParseFunction :: TestTree
testParseFunction = testGroup "parseFunction"
  [ testCase "Success" $ assertS "normal" (parseFunction False) [tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.LBrace, tok T.RBrace] (DefFunction "f" [] TypeNull [] False)
  , testCase "Context fail params" $ assertF "params ctx" (parseFunction False) [tok T.KwDef, tok (T.Identifier "f"), tok (T.Identifier "missing_paren")]
  , testCase "Context fail return" $ assertF "return ctx" (parseFunction False) [tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.LBrace]
  ]

testParseStruct :: TestTree
testParseStruct = testGroup "parseStruct"
  [ testCase "Success" $ assertS "struct" parseStruct [tok T.KwStruct, tok (T.Identifier "S"), tok T.LBrace, tok T.RBrace] (DefStruct "S" [] [])
  , testCase "Context fail body" $ assertF "body ctx" parseStruct [tok T.KwStruct, tok (T.Identifier "S"), tok T.LBrace, tok T.KwExport]
  ]

testParseStructBody :: TestTree
testParseStructBody = testCase "parseStructBody" $
  assertS "partitionEithers" parseStructBody [tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32, tok T.Semicolon, tok T.RBrace] ([Field "x" TypeI32], [])

testParseStructItem :: TestTree
testParseStructItem = testGroup "parseStructItem"
  [ testCase "KwDef" $ assertS "method" parseStructItem [tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.LBrace, tok T.RBrace] (Right (DefFunction "f" [] TypeNull [] False))
  , testCase "Identifier" $ assertS "field" parseStructItem [tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32, tok T.Semicolon] (Left (Field "x" TypeI32))
  , testCase "Fallthrough error" $ assertF "item error" parseStructItem [tok T.KwSomewhere]
  ]

testParseParams :: TestTree
testParseParams = testGroup "parseParams"
  [ testCase "Empty" $ assertS "()" parseParams [tok T.LParen, tok T.RParen] []
  , testCase "List with comma" $ assertS "(x:i32, y:i32)" parseParams 
      [tok T.LParen, tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32, tok T.Comma, tok (T.Identifier "y"), tok T.Colon, tok T.TypeI32, tok T.RParen] 
      [Parameter "x" TypeI32 Nothing, Parameter "y" TypeI32 Nothing]
  ]

testParseParameter :: TestTree
testParseParameter = testGroup "parseParameter"
  [ testCase "Self branch"  $ assertS "self" parseParameter [tok (T.Identifier "self")] (Parameter "self" TypeAny Nothing)
  , testCase "Typed branch" $ assertS "typed" parseParameter [tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32] (Parameter "x" TypeI32 Nothing)
  ]

testParseSelfParam :: TestTree
testParseSelfParam = testCase "parseSelfParam" $
  assertS "self" parseSelfParam [tok (T.Identifier "self")] (Parameter "self" TypeAny Nothing)

testParseTypedParam :: TestTree
testParseTypedParam = testGroup "parseTypedParam"
  [ testCase "Success" $ assertS "x: i32" parseTypedParam [tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32] (Parameter "x" TypeI32 Nothing)
  , testCase "failParse branch" $ assertF "no colon error" parseTypedParam [tok (T.Identifier "x"), tok T.Semicolon]
  ]

testParseReturnType :: TestTree
testParseReturnType = testGroup "parseReturnType"
  [ testCase "OpArrow"      $ assertS "->" parseReturnType [tok T.OpArrow, tok T.TypeI32] TypeI32
  , testCase "OpSquigArrow" $ assertS "~>" parseReturnType [tok T.OpSquigArrow, tok T.TypeI32] TypeI32
  ]

testParseField :: TestTree
testParseField = testCase "parseField" $
  assertS "field" parseField [tok (T.Identifier "f"), tok T.Colon, tok T.TypeString] (Field "f" TypeString)

testParseSomewhere :: TestTree
testParseSomewhere = testCase "parseSomewhere" $
  assertS "somewhere" parseSomewhere [tok T.KwSomewhere, tok T.LBrace, tok T.RBrace] (DefSomewhere [])

testParseFunctionSignatures :: TestTree
testParseFunctionSignatures = testGroup "parseFunctionSignatures"
  [ testCase "isEnd True"  $ assertS "empty" parseFunctionSignatures [tok T.RBrace] []
  , testCase "isEnd False" $ assertS "list"  parseFunctionSignatures 
      [tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.Semicolon, tok T.RBrace] 
      [FunctionSignature { sigFuncName = "f", sigParams = [], sigReturnType = TypeNull, sigIsExtern = False }]
  ]

testParseFunctionSignature :: TestTree
testParseFunctionSignature = testGroup "parseFunctionSignature"
  [ testCase "def" $ assertS "def" parseFunctionSignature [tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.Semicolon] (FunctionSignature { sigFuncName = "f", sigParams = [], sigReturnType = TypeNull, sigIsExtern = False })
  , testCase "Comma coverage" $ assertS "params" parseFunctionSignature 
      [tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.TypeI32, tok T.Comma, tok T.TypeF32, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.Semicolon] 
      (FunctionSignature { sigFuncName = "f", sigParams = [TypeI32, TypeF32], sigReturnType = TypeNull, sigIsExtern = False })
  ]

testParseParamTypeInSignature :: TestTree
testParseParamTypeInSignature = testGroup "parseParamTypeInSignature"
  [ testCase "try named success" $ assertS "named" parseParamTypeInSignature [tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32] TypeI32
  , testCase "parseType branch"   $ assertS "anon"  parseParamTypeInSignature [tok T.TypeI32] TypeI32
  ]

testParseSomewhereDecl :: TestTree 
testParseSomewhereDecl = testGroup "parseSomewhereDecl"
  [ testCase "parse use statement with .sw extension" $ 
      assertS "use statement" parseSomewhereDecl 
      [tok T.KwUse, tok (T.Identifier "Vec"), tok T.Dot, tok (T.Identifier "sw"), tok T.Semicolon]
      (DeclUse "Vec.sw")
  , testCase "parse use statement with .somewhere extension" $ 
      assertS "use statement" parseSomewhereDecl 
      [tok T.KwUse, tok (T.Identifier "Vec"), tok T.Dot, tok (T.Identifier "somewhere"), tok T.Semicolon]
      (DeclUse "Vec.somewhere")
  , testCase "parse use statement without extension" $ 
      assertS "use statement" parseSomewhereDecl 
      [tok T.KwUse, tok (T.Identifier "Vec"), tok T.Semicolon]
      (DeclUse "Vec")
  , testCase "parse function signature" $
      assertS "function signature" parseSomewhereDecl
      [tok T.KwDef, tok (T.Identifier "test"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeI32, tok T.Semicolon]
      (DeclFuncSig (FunctionSignature { sigFuncName = "test", sigParams = [], sigReturnType = TypeI32, sigIsExtern = False }))
  , testCase "parse struct definition" $
      assertS "struct definition" parseSomewhereDecl
      [tok T.KwStruct, tok (T.Identifier "Test"), tok T.LBrace, tok T.RBrace]
      (DeclDefs (DefStruct "Test" [] []))
  , testCase "invalid token fails" $
      let result = runParser parseSomewhereDecl (ParserState [tok T.TypeI32] 0 "" 0)
      in case result of
        Left err -> assertBool "Should contain error message" ("Expected" `isInfixOf` err && ("use" `isInfixOf` err || "function" `isInfixOf` err || "struct" `isInfixOf` err))
        Right _ -> fail "Should have failed on invalid token"
  ]

testStructureSignature :: TestTree
testStructureSignature = testGroup "StructureSignature"
  [ testCase "create StructureSignature" $ do
      let sig = StructureSignature { sigStructName = "TestStruct", sigAttributes = [("field1", TypeI32), ("field2", TypeF32)], sigMethods = [FunctionSignature { sigFuncName = "method1", sigParams = [TypeI32], sigReturnType = TypeNull, sigIsExtern = False }] }
      assertEqual "name should match" "TestStruct" (sigStructName sig)
      assertEqual "attributes should match" [("field1", TypeI32), ("field2", TypeF32)] (sigAttributes sig) 
      assertEqual "methods should match" [FunctionSignature { sigFuncName = "method1", sigParams = [TypeI32], sigReturnType = TypeNull, sigIsExtern = False }] (sigMethods sig)
  , testCase "empty StructureSignature" $ do
      let sig = StructureSignature { sigStructName = "Empty", sigAttributes = [], sigMethods = [] }
      assertEqual "name should match" "Empty" (sigStructName sig)
      assertEqual "attributes should be empty" [] (sigAttributes sig)
      assertEqual "methods should be empty" [] (sigMethods sig)
  , testCase "StructureSignature equality" $ do
      let sig1 = StructureSignature { sigStructName = "Test", sigAttributes = [("x", TypeI32)], sigMethods = [] }
      let sig2 = StructureSignature { sigStructName = "Test", sigAttributes = [("x", TypeI32)], sigMethods = [] }
      let sig3 = StructureSignature { sigStructName = "Test", sigAttributes = [("y", TypeI32)], sigMethods = [] }
      assertEqual "identical signatures should be equal" sig1 sig2
      assertBool "different signatures should not be equal" (sig1 /= sig3)
  , testCase "StructureSignature show instance" $ do
      let sig = StructureSignature { sigStructName = "ShowTest", sigAttributes = [("a", TypeI32)], sigMethods = [FunctionSignature { sigFuncName = "f", sigParams = [], sigReturnType = TypeNull, sigIsExtern = False }] }
      let shown = show sig
      assertBool "should contain struct name" ("ShowTest" `isInfixOf` shown)
      assertBool "should contain field info" ("i32" `isInfixOf` shown)
      assertBool "should contain method info" ("FunctionSignature" `isInfixOf` shown)
  ]
