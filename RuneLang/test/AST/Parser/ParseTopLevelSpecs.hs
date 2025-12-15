module AST.Parser.ParseTopLevelSpecs (parseTopLevelTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Rune.AST.Parser.ParseTopLevel (parseTopLevels)
import Rune.AST.Types (Parser(..), ParserState(..))
import Rune.AST.Nodes
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseTopLevelTests :: TestTree
parseTopLevelTests =
  testGroup
    "AST Parser ParseTopLevel Tests"
    [ functionTests
    , structTests
    , overrideTests
    ]

--
-- helpers
--

tok :: T.TokenKind -> T.Token
tok k = T.Token k "" 1 1

run :: [T.Token] -> Either String [TopLevelDef]
run ts =
  let state = ParserState (ts ++ [tok T.EOF]) 0 "test" 0
  in case runParser parseTopLevels state of
       Right (defs, _) -> Right defs
       Left err -> Left err

assertParse :: String -> [T.Token] -> [TopLevelDef] -> IO ()
assertParse msg tokens expected =
  case run tokens of
    Right actual -> assertEqual msg expected actual
    Left err -> assertBool ("Parse failed: " ++ err) False

--
-- tests
--

functionTests :: TestTree
functionTests = testGroup "Function Tests"
  [ testCase "Function (no params)" $
      assertParse "def main() -> i32 { return 0; }"
        [tok T.KwDef, tok (T.Identifier "main"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeI32, tok T.LBrace, tok T.KwReturn, tok (T.LitInt 0), tok T.Semicolon, tok T.RBrace]
        [DefFunction "main" [] TypeI32 [StmtReturn (Just (ExprLitInt 0))]]

  , testCase "Function (params)" $
      assertParse "def add(x: i32, y: i32) -> i32 {}"
        [tok T.KwDef, tok (T.Identifier "add"), tok T.LParen, tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32, tok T.Comma, tok (T.Identifier "y"), tok T.Colon, tok T.TypeI32, tok T.RParen, tok T.OpArrow, tok T.TypeI32, tok T.LBrace, tok T.RBrace]
        [DefFunction "add" [Parameter "x" TypeI32, Parameter "y" TypeI32] TypeI32 []]
  ]

structTests :: TestTree
structTests = testGroup "Struct Tests"
  [ testCase "Empty Struct" $
      assertParse "struct Empty {}"
        [tok T.KwStruct, tok (T.Identifier "Empty"), tok T.LBrace, tok T.RBrace]
        [DefStruct "Empty" [] []]

  , testCase "Struct with fields and methods" $
      assertParse "struct Point { x: i32; y: i32; def show() -> null {} }"
        [tok T.KwStruct, tok (T.Identifier "Point"), tok T.LBrace, tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32, tok T.Semicolon, tok (T.Identifier "y"), tok T.Colon, tok T.TypeI32, tok T.Semicolon, tok T.KwDef, tok (T.Identifier "show"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeNull, tok T.LBrace, tok T.RBrace, tok T.RBrace]
        [DefStruct "Point" [Field "x" TypeI32, Field "y" TypeI32] [DefFunction "show" [] TypeNull []]]
  ]

overrideTests :: TestTree
overrideTests = testGroup "Override Tests"
  [ testCase "Override" $
      assertParse "override def main() -> i32 {}"
        [tok T.KwOverride, tok T.KwDef, tok (T.Identifier "main"), tok T.LParen, tok T.RParen, tok T.OpArrow, tok T.TypeI32, tok T.LBrace, tok T.RBrace]
        [DefOverride "main" [] TypeI32 []]
  ]
