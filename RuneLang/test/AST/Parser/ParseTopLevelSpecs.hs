module AST.Parser.ParseTopLevelSpecs (parseTopLevelTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
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
      testParseParamTypeInSignature
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
      [FunctionSignature "f" [] TypeNull]
  ]

testParseFunctionSignature :: TestTree
testParseFunctionSignature = testGroup "parseFunctionSignature"
  [ testCase "def" $ assertS "def" parseFunctionSignature [tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.Semicolon] (FunctionSignature "f" [] TypeNull)
  , testCase "Comma coverage" $ assertS "params" parseFunctionSignature 
      [tok T.KwDef, tok (T.Identifier "f"), tok T.LParen, tok T.TypeI32, tok T.Comma, tok T.TypeF32, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.Semicolon] 
      (FunctionSignature "f" [TypeI32, TypeF32] TypeNull)
  ]

testParseParamTypeInSignature :: TestTree
testParseParamTypeInSignature = testGroup "parseParamTypeInSignature"
  [ testCase "try named success" $ assertS "named" parseParamTypeInSignature [tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32] TypeI32
  , testCase "parseType branch"   $ assertS "anon"  parseParamTypeInSignature [tok T.TypeI32] TypeI32
  ]
