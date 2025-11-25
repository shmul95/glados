module Lexer.LexerSpec (lexerTests) where

import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens
import Test.Tasty
import Test.Tasty.HUnit

--
-- public
--

lexerTests :: TestTree
lexerTests =
  testGroup
    "Rune lexer Tests"
    [ exampleTests,
      unitTests,
      positionTests
    ]

--
-- private
--

checkLexer :: String -> [TokenKind] -> Assertion
checkLexer input expectedKinds =
  case lexer "<test>" input of
    Left err -> assertFailure $ "Lexing failed:\n" ++ show err
    Right tokens ->
      assertEqual "Token kinds mismatch" expectedKinds (map tokenKind tokens)

checkLexerError :: String -> Assertion
checkLexerError input =
  case lexer "<test>" input of
    Left _ -> assertBool "Lexing failed as expected" True
    Right tokens -> assertFailure $ "Lexing succeeded, but should have failed. Tokens:\n" ++ show tokens

--
-- this section contains tests for the ./examples/**.ru files to be sure they will lex correctly
--
exampleTests :: TestTree
exampleTests =
  testGroup
    "Example Files Tests"
    [ testCase "hello_rune.ru" $
        checkLexer
          "def main() -> null\n{\n    show(\"Hello, Rune!\\n\");\n}"
          [ KwDef,
            Identifier "main",
            LParen,
            RParen,
            OpArrow,
            TypeNull,
            LBrace,
            Identifier "show",
            LParen,
            LitString "Hello, Rune!\n",
            RParen,
            Semicolon,
            RBrace,
            EOF
          ],
      testCase "fibonacci.ru" $
        checkLexer
          "def fibonacci(n: i32) -> i32\n{\n    if n <= 0 {\n        0\n    }\n    if n == 1 {\n        1\n    }\n    fibonacci(n - 1) + fibonacci(n - 2)\n}\n\ndef main() -> i32\n{\n    fibonacci(10)\n}"
          [ KwDef,
            Identifier "fibonacci",
            LParen,
            Identifier "n",
            Colon,
            TypeI32,
            RParen,
            OpArrow,
            TypeI32,
            LBrace,
            KwIf,
            Identifier "n",
            OpLte,
            LitInt 0,
            LBrace,
            LitInt 0,
            RBrace,
            KwIf,
            Identifier "n",
            OpEq,
            LitInt 1,
            LBrace,
            LitInt 1,
            RBrace,
            Identifier "fibonacci",
            LParen,
            Identifier "n",
            OpMinus,
            LitInt 1,
            RParen,
            OpPlus,
            Identifier "fibonacci",
            LParen,
            Identifier "n",
            OpMinus,
            LitInt 2,
            RParen,
            RBrace,
            KwDef,
            Identifier "main",
            LParen,
            RParen,
            OpArrow,
            TypeI32,
            LBrace,
            Identifier "fibonacci",
            LParen,
            LitInt 10,
            RParen,
            RBrace,
            EOF
          ],
      testCase "error_handling.ru" $
        checkLexer
          "def maybe_function() ~> i32\n{\n    num = 42;\n\n    if num == 42 {\n        return num;\n    }\n    return error(\"An error occurred\");\n}\n\ndef main() -> i32\n{\n    a = maybe_function()?;\n\n    return 0;\n}"
          [ KwDef,
            Identifier "maybe_function",
            LParen,
            RParen,
            OpSquigArrow,
            TypeI32,
            LBrace,
            Identifier "num",
            OpAssign,
            LitInt 42,
            Semicolon,
            KwIf,
            Identifier "num",
            OpEq,
            LitInt 42,
            LBrace,
            KwReturn,
            Identifier "num",
            Semicolon,
            RBrace,
            KwReturn,
            Identifier "error",
            LParen,
            LitString "An error occurred",
            RParen,
            Semicolon,
            RBrace,
            KwDef,
            Identifier "main",
            LParen,
            RParen,
            OpArrow,
            TypeI32,
            LBrace,
            Identifier "a",
            OpAssign,
            Identifier "maybe_function",
            LParen,
            RParen,
            OpErrorProp,
            Semicolon,
            KwReturn,
            LitInt 0,
            Semicolon,
            RBrace,
            EOF
          ],
      testCase "with spaces and comments" $
        checkLexer
          "def main /* block comment */ () -> i32 // line comment\n{ 1 + 2 }"
          [KwDef, Identifier "main", LParen, RParen, OpArrow, TypeI32, LBrace, LitInt 1, OpPlus, LitInt 2, RBrace, EOF]
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testGroup
        "Keywords"
        [ testCase "kw def" $ checkLexer "def" [KwDef, EOF],
          testCase "kw if followed by id" $ checkLexer "iface" [Identifier "iface", EOF],
          testCase "kw struct followed by _" $ checkLexer "struct_foo" [Identifier "struct_foo", EOF]
        ],
      testGroup
        "Primitives"
        [ testCase "i32" $ checkLexer "i32" [TypeI32, EOF],
          testCase "bool" $ checkLexer "bool" [TypeBool, EOF],
          testCase "null" $ checkLexer "null" [TypeNull, EOF],
          testCase "i32a" $ checkLexer "i32a" [Identifier "i32a", EOF]
        ],
      testGroup
        "Operators"
        [ testCase "OpLte vs OpLt" $ checkLexer "<=" [OpLte, EOF],
          testCase "OpLt" $ checkLexer "<" [OpLt, EOF],
          testCase "OpArrow vs OpMinus" $ checkLexer "a - > b" [Identifier "a", OpMinus, OpGt, Identifier "b", EOF],
          testCase "OpArrow" $ checkLexer "a->b" [Identifier "a", OpArrow, Identifier "b", EOF],
          testCase "OpEq vs OpAssign" $ checkLexer "a == b" [Identifier "a", OpEq, Identifier "b", EOF],
          testCase "OpAssign" $ checkLexer "a = b" [Identifier "a", OpAssign, Identifier "b", EOF]
        ],
      testGroup
        "Delimiters"
        [ testCase "all delimiters" $ checkLexer "(){} ,;:" [LParen, RParen, LBrace, RBrace, Comma, Semicolon, Colon, EOF]
        ],
      testGroup
        "Literals"
        [ testCase "int positive" $ checkLexer "123" [LitInt 123, EOF],
          testCase "int negative" $ checkLexer "-456" [LitInt (-456), EOF],
          testCase "int zero" $ checkLexer "0" [LitInt 0, EOF],
          testCase "float positive" $ checkLexer "12.34" [LitFloat 12.34, EOF],
          testCase "float negative" $ checkLexer "-5.0" [LitFloat (-5.0), EOF],
          testCase "float leading zero" $ checkLexer "0.1" [LitFloat 0.1, EOF],
          testCase "float error (no decimal)" $ checkLexerError "12.",
          testCase "bool true" $ checkLexer "true" [LitBool True, EOF],
          testCase "bool false" $ checkLexer "false" [LitBool False, EOF],
          testCase "string simple" $ checkLexer "\"hello\"" [LitString "hello", EOF],
          testCase "string with escapes" $ checkLexer "\"\\n\\t\\r\\\\\\\"\"" [LitString "\n\t\r\\\"", EOF]
        ],
      testGroup
        "Identifiers"
        [ testCase "normal id" $ checkLexer "foo_bar99" [Identifier "foo_bar99", EOF],
          testCase "id starting with underscore" $ checkLexer "_temp" [Identifier "_temp", EOF],
          testCase "id mixed with keyword" $ checkLexer "if_else" [Identifier "if_else", EOF]
        ]
    ]

positionTests :: TestTree
positionTests =
  testGroup
    "Position Tests"
    [ testCase "single token (1, 1)" $
        case lexer "<test>" "def" of
          Right [tdef, _] -> do
            assertEqual "Def line" 1 (tokenLine tdef)
            assertEqual "Def col" 1 (tokenColumn tdef)
          Left err -> assertFailure $ show err
          Right tokens -> assertFailure $ "Expected 2 tokens, got " ++ show (length tokens),
      testCase "tokens with space (1, 1) and (1, 5)" $
        case lexer "<test>" "def main" of
          Right [tdef, tmain, _] -> do
            assertEqual "Def line" 1 (tokenLine tdef)
            assertEqual "Def col" 1 (tokenColumn tdef)
            assertEqual "Main line" 1 (tokenLine tmain)
            assertEqual "Main col" 5 (tokenColumn tmain)
          Left err -> assertFailure $ show err
          Right tokens -> assertFailure $ "Expected 3 tokens, got " ++ show (length tokens),
      testCase "tokens on new line" $
        case lexer "<test>" "def\nmain" of
          Right [tdef, tmain, _] -> do
            assertEqual "Def line" 1 (tokenLine tdef)
            assertEqual "Def col" 1 (tokenColumn tdef)
            assertEqual "Main line" 2 (tokenLine tmain)
            assertEqual "Main col" 1 (tokenColumn tmain)
          Left err -> assertFailure $ show err
          Right tokens -> assertFailure $ "Expected 3 tokens, got " ++ show (length tokens),
      testCase "tokens after comment" $
        case lexer "<test>" "// comment\nmain" of
          Right [tmain, _] -> do
            assertEqual "Main line" 2 (tokenLine tmain)
            assertEqual "Main col" 1 (tokenColumn tmain)
          Left err -> assertFailure $ show err
          Right tokens -> assertFailure $ "Expected 2 tokens, got " ++ show (length tokens)
    ]
