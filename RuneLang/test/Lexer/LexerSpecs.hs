module Lexer.LexerSpecs (lexerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Rune.Lexer.Tokens (TokenKind (..))

import Lexer.Utils (tok, lexTest, lexFailTest)

--
-- public
--

lexerTests :: TestTree
lexerTests =
  testGroup
    "Rune Lexer Component Tests"
    [ mainLexerTests
    ]

--
-- helpers
--


--
-- private
--

test_empty_input :: TestTree
test_empty_input = testCase "Empty input" $
  lexTest "" [tok EOF "" 1 1]

test_whitespace_only :: TestTree
test_whitespace_only = testCase "Whitespace only" $
  lexTest " \t\n" [tok EOF "" 2 1]

test_single_line_comment :: TestTree
test_single_line_comment = testCase "Single line comment" $
  lexTest "// comment\n" [tok EOF "" 2 1]

test_block_comment :: TestTree
test_block_comment = testCase "Block comment" $
  lexTest "/* multi\nline\ncomment */" [tok EOF "" 3 11]

test_mixed_comments_and_tokens :: TestTree
test_mixed_comments_and_tokens = testCase "Comments, Whitespace and Tokens" $
  lexTest "/*block*/ 123 //line\n\n\t def"
    [ tok (LitInt 123) "123" 1 11,
      tok (KwDef) "def" 3 10,
      tok EOF "" 3 13 ]

test_full_snippet_position :: TestTree
test_full_snippet_position = testCase "Full snippet and position check" $
  lexTest "def main() -> i32 {\n  return 0;\n}"
    [ tok (KwDef) "def" 1 1,
      tok (Identifier "main") "main" 1 5,
      tok LParen "(" 1 9,
      tok RParen ")" 1 10,
      tok OpArrow "->" 1 12,
      tok TypeI32 "i32" 1 15,
      tok LBrace "{" 1 19,
      tok KwReturn "return" 2 3,
      tok (LitInt 0) "0" 2 10,
      tok Semicolon ";" 2 11,
      tok RBrace "}" 3 1,
      tok EOF "" 3 2 ]

test_invalid_character :: TestTree
test_invalid_character = testCase "Invalid character failure" $
  lexFailTest "@"

mainLexerTests :: TestTree
mainLexerTests =
  testGroup
    "Lexer Core (Integration, Comments, Position)"
    [ test_empty_input,
      test_whitespace_only,
      test_single_line_comment,
      test_block_comment,
      test_mixed_comments_and_tokens,
      test_full_snippet_position,
      test_invalid_character
    ]
