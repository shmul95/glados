module ASTParserSpec (astParserTests) where

import Data.Either (isLeft)
import Rune.AST.Nodes
import Rune.AST.Parser (parseRune)
import qualified Rune.Lexer.Tokens as T
import Test.Tasty
import Test.Tasty.HUnit

--
-- public
--

astParserTests :: TestTree
astParserTests =
  testGroup
    "AST Parser Tests"
    [ literalsTests,
      expressionTests,
      statementTests,
      structTests,
      functionTests,
      errorTests
    ]

--
-- Helpers
--

t :: T.TokenKind -> T.Token
t k = T.Token k "" 0 0

idTok :: String -> T.Token
idTok s = T.Token (T.Identifier s) s 0 0

intTok :: Int -> T.Token
intTok n = T.Token (T.LitInt n) (show n) 0 0

boolTok :: Bool -> T.Token
boolTok b = T.Token (T.LitBool b) (show b) 0 0

runP :: [T.Token] -> Either String Program
runP tokens = parseRune "test.ru" (tokens ++ [t T.EOF])

assertParse :: [T.Token] -> [TopLevelDef] -> Assertion
assertParse tokens expected =
  case runP tokens of
    Left err -> assertFailure $ "Parse failed: " ++ err
    Right (Program _ defs) -> defs @?= expected

assertParseFail :: [T.Token] -> Assertion
assertParseFail tokens =
  assertBool "Parser should have failed" (isLeft $ runP tokens)

--
-- Tests
--

literalsTests :: TestTree
literalsTests =
  testGroup
    "Literals & Basic Types"
    [ testCase "Parses basic function with return 0" $ do
        let tokens =
              [ t T.KwDef,
                idTok "main",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeI32,
                t T.LBrace,
                intTok 0,
                t T.RBrace
              ]
        let expected =
              [ DefFunction
                  "main"
                  []
                  TypeI32
                  [StmtReturn (Just (ExprLitInt 0))]
              ]
        assertParse tokens expected
    ]

expressionTests :: TestTree
expressionTests =
  testGroup
    "Expressions & Precedence"
    [ testCase "Respects PEMDAS (Mul before Add)" $ do
        let tokens =
              [ t T.KwDef,
                idTok "main",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeNull,
                t T.LBrace,
                idTok "x",
                t T.OpAssign,
                intTok 1,
                t T.OpPlus,
                intTok 2,
                t T.OpMul,
                intTok 3,
                t T.Semicolon,
                t T.RBrace
              ]
        let expectedExpr =
              ExprBinary
                Add
                (ExprLitInt 1)
                (ExprBinary Mul (ExprLitInt 2) (ExprLitInt 3))

        case runP tokens of
          Right (Program _ [DefFunction _ _ _ [StmtVarDecl "x" Nothing expr]]) ->
            expr @?= expectedExpr
          r -> assertFailure $ "Unexpected result: " ++ show r,
      testCase "Parses nested function calls" $ do
        let tokens =
              [ t T.KwDef,
                idTok "main",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeNull,
                t T.LBrace,
                idTok "foo",
                t T.LParen,
                idTok "bar",
                t T.LParen,
                intTok 1,
                t T.RParen,
                t T.RParen,
                t T.Semicolon,
                t T.RBrace
              ]
        let expectedStmt =
              StmtExpr $ ExprCall "foo" [ExprCall "bar" [ExprLitInt 1]]

        case runP tokens of
          Right (Program _ [DefFunction _ _ _ [s]]) -> s @?= expectedStmt
          r -> assertFailure $ "Unexpected result: " ++ show r
    ]

statementTests :: TestTree
statementTests =
  testGroup
    "Statements & Control Flow"
    [ testCase "Parses Implicit Return (No Semicolon at end of block)" $ do
        let tokens =
              [ t T.KwDef,
                idTok "f",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeI32,
                t T.LBrace,
                intTok 10,
                t T.RBrace
              ]
        let expected = [DefFunction "f" [] TypeI32 [StmtReturn (Just (ExprLitInt 10))]]
        assertParse tokens expected,
      testCase "Parses Explicit Expr Stmt (With Semicolon)" $ do
        let tokens =
              [ t T.KwDef,
                idTok "f",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeI32,
                t T.LBrace,
                intTok 10,
                t T.Semicolon,
                t T.RBrace
              ]
        let expected = [DefFunction "f" [] TypeI32 [StmtExpr (ExprLitInt 10)]]
        assertParse tokens expected,
      testCase "Parses If/Else Block" $ do
        let tokens =
              [ t T.KwDef,
                idTok "main",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeNull,
                t T.LBrace,
                t T.KwIf,
                boolTok True,
                t T.LBrace,
                t T.RBrace,
                t T.KwElse,
                t T.LBrace,
                t T.RBrace,
                t T.RBrace
              ]
        let expectedBody =
              [StmtIf (ExprLitBool True) [] (Just [])]

        case runP tokens of
          Right (Program _ [DefFunction _ _ _ body]) -> body @?= expectedBody
          r -> assertFailure $ "Failed: " ++ show r,
      testCase "Parses For Range Loop" $ do
        let tokens =
              [ t T.KwDef,
                idTok "main",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeNull,
                t T.LBrace,
                t T.KwFor,
                idTok "i",
                t T.OpAssign,
                intTok 0,
                t T.KwTo,
                intTok 10,
                t T.LBrace,
                t T.RBrace,
                t T.RBrace
              ]
        let expectedBody =
              [StmtFor "i" (ExprLitInt 0) (ExprLitInt 10) []]

        case runP tokens of
          Right (Program _ [DefFunction _ _ _ body]) -> body @?= expectedBody
          r -> assertFailure $ "Failed: " ++ show r
    ]

structTests :: TestTree
structTests =
  testGroup
    "Struct Parsing"
    [ testCase "Parses Struct with Fields and Methods" $ do
        let tokens =
              [ t T.KwStruct,
                idTok "Vec",
                t T.LBrace,
                idTok "x",
                t T.Colon,
                t T.TypeI32,
                t T.Semicolon,
                t T.KwDef,
                idTok "make",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeNull,
                t T.LBrace,
                t T.RBrace,
                t T.RBrace
              ]
        let expected =
              [ DefStruct
                  "Vec"
                  [Field "x" TypeI32]
                  [DefFunction "make" [] TypeNull []]
              ]
        assertParse tokens expected,
      testCase "Parses Struct Initialization Expression" $ do
        let tokens =
              [ t T.KwDef,
                idTok "main",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeNull,
                t T.LBrace,
                idTok "val",
                t T.OpAssign,
                idTok "Vec",
                t T.LBrace,
                idTok "x",
                t T.Colon,
                intTok 1,
                t T.RBrace,
                t T.Semicolon,
                t T.RBrace
              ]
        let expectedExpr = ExprStructInit "Vec" [("x", ExprLitInt 1)]

        case runP tokens of
          Right (Program _ [DefFunction _ _ _ [StmtVarDecl "val" Nothing expr]]) ->
            expr @?= expectedExpr
          r -> assertFailure $ "Struct Init failed: " ++ show r
    ]

functionTests :: TestTree
functionTests =
  testGroup
    "Function Definitions"
    [ testCase "Parses Override Function" $ do
        let tokens =
              [ t T.KwOverride,
                t T.KwDef,
                idTok "show",
                t T.LParen,
                idTok "v",
                t T.Colon,
                t T.TypeAny,
                t T.RParen,
                t T.OpArrow,
                t T.TypeNull,
                t T.LBrace,
                t T.RBrace
              ]
        let expected = [DefOverride "show" [Parameter "v" TypeAny] TypeNull []]
        assertParse tokens expected,
      testCase "Parses Error Propagation (?)" $ do
        let tokens =
              [ t T.KwDef,
                idTok "main",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeNull,
                t T.LBrace,
                idTok "x",
                t T.OpAssign,
                idTok "foo",
                t T.LParen,
                t T.RParen,
                t T.OpErrorProp,
                t T.Semicolon,
                t T.RBrace
              ]
        let expectedExpr = ExprUnary PropagateError (ExprCall "foo" [])

        case runP tokens of
          Right (Program _ [DefFunction _ _ _ [StmtVarDecl "x" Nothing expr]]) ->
            expr @?= expectedExpr
          r -> assertFailure $ "Error Prop failed: " ++ show r
    ]

errorTests :: TestTree
errorTests =
  testGroup
    "Error Handling"
    [ testCase "Fails on missing semicolon in block middle" $ do
        let tokens =
              [ t T.KwDef,
                idTok "f",
                t T.LParen,
                t T.RParen,
                t T.OpArrow,
                t T.TypeI32,
                t T.LBrace,
                intTok 10,
                intTok 20,
                t T.RBrace
              ]
        assertParseFail tokens,
      testCase "Fails on double struct closing brace" $ do
        let tokens =
              [t T.KwStruct, idTok "A", t T.LBrace, t T.RBrace, t T.RBrace]
        assertParseFail tokens
    ]
