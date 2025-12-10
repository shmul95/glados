module Lexer.LexerLiteralsSpecs (lexerLiteralsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Rune.Lexer.Tokens (TokenKind (..))
import Lexer.Utils (tok, lexTest, lexFailTest)

--
-- public
--

lexerLiteralsTests :: TestTree
lexerLiteralsTests =
  testGroup
    "LexerLiterals Tests"
    [ test_lit_int_positive
    , test_lit_int_negative
    , test_lit_float_positive
    , test_lit_float_negative
    , test_lit_float_priority
    , test_lit_bool_true
    , test_lit_bool_false
    , test_lit_string_empty
    , test_lit_string_with_escapes
    , test_lit_string_unclosed_failure
    , test_lit_char_simple
    , test_lit_char_escape_n
    , test_lit_char_escape_backslash
    , test_lit_char_unclosed_failure
    , test_lit_char_too_many_chars_failure
    , test_lit_int_followed_by_dot_failure
    ]

--
-- private
--

test_lit_int_positive :: TestTree
test_lit_int_positive = testCase "Positive Integer Literal" $
  lexTest "123" [tok (LitInt 123) "123" 1 1, tok EOF "" 1 4]

test_lit_int_negative :: TestTree
test_lit_int_negative = testCase "Negative Integer Literal" $
  lexTest "-456" [tok (LitInt (-456)) "-456" 1 1, tok EOF "" 1 5]

test_lit_float_positive :: TestTree
test_lit_float_positive = testCase "Positive Float Literal" $
  lexTest "1.23" [tok (LitFloat 1.23) "1.23" 1 1, tok EOF "" 1 5]

test_lit_float_negative :: TestTree
test_lit_float_negative = testCase "Negative Float Literal" $
  lexTest "-4.56" [tok (LitFloat (-4.56)) "-4.56" 1 1, tok EOF "" 1 6]

test_lit_float_priority :: TestTree
test_lit_float_priority = testCase "Float priority over Int (1.23 should not be Int, Dot, Int)" $
  lexTest "1.23" [tok (LitFloat 1.23) "1.23" 1 1, tok EOF "" 1 5]

test_lit_bool_true :: TestTree
test_lit_bool_true = testCase "Boolean Literal true" $
  lexTest "true" [tok (LitBool True) "true" 1 1, tok EOF "" 1 5]

test_lit_bool_false :: TestTree
test_lit_bool_false = testCase "Boolean Literal false" $
  lexTest "false" [tok (LitBool False) "false" 1 1, tok EOF "" 1 6]

test_lit_string_empty :: TestTree
test_lit_string_empty = testCase "Empty String Literal" $
  lexTest "\"\"" [tok (LitString "") "\"\"" 1 1, tok EOF "" 1 3]

test_lit_string_with_escapes :: TestTree
test_lit_string_with_escapes = testCase "String with all escapes" $
  let input = "\"a\\n\\t\\r\\\\\\\"'b\""
      expectedValue = "a\n\t\r\\\"'b"
      expectedTokenValue = "\"" ++ expectedValue ++ "\""
  in lexTest input [tok (LitString expectedValue) expectedTokenValue 1 1, tok EOF "" 1 16]

test_lit_string_unclosed_failure :: TestTree
test_lit_string_unclosed_failure = testCase "Unclosed string failure" $
  lexFailTest "\"abc"

test_lit_char_simple :: TestTree
test_lit_char_simple = testCase "Simple Char Literal" $
  lexTest "'a'" [tok (LitChar 'a') "'a'" 1 1, tok EOF "" 1 4]

test_lit_char_escape_n :: TestTree
test_lit_char_escape_n = testCase "Char Literal with escape '\\n'" $
  let charVal = '\n'
      expectedTokenValue = "'" ++ [charVal] ++ "'"
  in lexTest "'\\n'" [tok (LitChar charVal) expectedTokenValue 1 1, tok EOF "" 1 5]

test_lit_char_escape_backslash :: TestTree
test_lit_char_escape_backslash = testCase "Char Literal with escape '\\\\'" $
  let charVal = '\\'
      expectedTokenValue = "'" ++ [charVal] ++ "'"
  in lexTest "'\\\\'" [tok (LitChar charVal) expectedTokenValue 1 1, tok EOF "" 1 5]

test_lit_char_unclosed_failure :: TestTree
test_lit_char_unclosed_failure = testCase "Unclosed char failure" $
  lexFailTest "'a"

test_lit_char_too_many_chars_failure :: TestTree
test_lit_char_too_many_chars_failure = testCase "Char literal with too many chars failure" $
  lexFailTest "'abc'"

test_lit_int_followed_by_dot_failure :: TestTree
test_lit_int_followed_by_dot_failure = testCase "Int followed by dot (no decimal) failure" $
  lexFailTest "123."
