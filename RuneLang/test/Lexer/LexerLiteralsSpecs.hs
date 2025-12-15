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
    , test_lit_int_separators
    , test_lit_int_separators_negative
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
    , test_lit_string_octal_escape_esc
    , test_lit_string_hex_escape_esc
    , test_lit_string_unicode_escape_braced
    , test_lit_string_unicode_escape_unbraced
    , test_lit_string_ansi_red_octal
    , test_lit_string_ansi_green_hex
    , test_lit_string_ansi_blue_unicode
    , test_lit_char_octal_escape
    , test_lit_char_hex_escape
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

test_lit_int_separators :: TestTree
test_lit_int_separators = testCase "Integer Literal with separators" $
  lexTest "1_000_000" [tok (LitInt 1000000) "1_000_000" 1 1, tok EOF "" 1 10]

test_lit_int_separators_negative :: TestTree
test_lit_int_separators_negative = testCase "Negative Integer Literal with separators" $
  lexTest "-1_234" [tok (LitInt (-1234)) "-1_234" 1 1, tok EOF "" 1 7]

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

test_lit_string_octal_escape_esc :: TestTree
test_lit_string_octal_escape_esc = testCase "String with octal escape \\033 (ESC)" $
  let escChar = '\ESC'
      expectedValue = [escChar]
      input = "\"\\033\""
      expectedTokenValue = "\"" ++ expectedValue ++ "\""
  in lexTest input [tok (LitString expectedValue) expectedTokenValue 1 1, tok EOF "" 1 7]

test_lit_string_hex_escape_esc :: TestTree
test_lit_string_hex_escape_esc = testCase "String with hex escape \\x1b (ESC)" $
  let escChar = '\ESC'
      expectedValue = [escChar]
      input = "\"\\x1b\""
      expectedTokenValue = "\"" ++ expectedValue ++ "\""
  in lexTest input [tok (LitString expectedValue) expectedTokenValue 1 1, tok EOF "" 1 7]

test_lit_string_unicode_escape_braced :: TestTree
test_lit_string_unicode_escape_braced = testCase "String with unicode escape \\u{1b} (ESC)" $
  let escChar = '\ESC'
      expectedValue = [escChar]
      input = "\"\\u{1b}\""
      expectedTokenValue = "\"" ++ expectedValue ++ "\""
  in lexTest input [tok (LitString expectedValue) expectedTokenValue 1 1, tok EOF "" 1 9]

test_lit_string_unicode_escape_unbraced :: TestTree
test_lit_string_unicode_escape_unbraced = testCase "String with unicode escape \\u001b (ESC)" $
  let escChar = '\ESC'
      expectedValue = [escChar]
      input = "\"\\u001b\""
      expectedTokenValue = "\"" ++ expectedValue ++ "\""
  in lexTest input [tok (LitString expectedValue) expectedTokenValue 1 1, tok EOF "" 1 9]

test_lit_string_ansi_red_octal :: TestTree
test_lit_string_ansi_red_octal = testCase "ANSI red color with octal escape" $
  let escChar = '\ESC'
      expectedValue = [escChar] ++ "[31mRed" ++ [escChar] ++ "[0m"
      input = "\"\\033[31mRed\\033[0m\""
      expectedTokenValue = "\"" ++ expectedValue ++ "\""
  in lexTest input [tok (LitString expectedValue) expectedTokenValue 1 1, tok EOF "" 1 21]

test_lit_string_ansi_green_hex :: TestTree
test_lit_string_ansi_green_hex = testCase "ANSI green color with hex escape" $
  let escChar = '\ESC'
      expectedValue = [escChar] ++ "[32mGreen" ++ [escChar] ++ "[0m"
      input = "\"\\x1b[32mGreen\\x1b[0m\""
      expectedTokenValue = "\"" ++ expectedValue ++ "\""
  in lexTest input [tok (LitString expectedValue) expectedTokenValue 1 1, tok EOF "" 1 23]

test_lit_string_ansi_blue_unicode :: TestTree
test_lit_string_ansi_blue_unicode = testCase "ANSI blue color with unicode escape" $
  let escChar = '\ESC'
      expectedValue = [escChar] ++ "[34mBlue" ++ [escChar] ++ "[0m"
      input = "\"\\u{1b}[34mBlue\\u{1b}[0m\""
      expectedTokenValue = "\"" ++ expectedValue ++ "\""
  in lexTest input [tok (LitString expectedValue) expectedTokenValue 1 1, tok EOF "" 1 26]

test_lit_char_octal_escape :: TestTree
test_lit_char_octal_escape = testCase "Char with octal escape \\033" $
  let escChar = '\ESC'
      input = "'\\033'"
      expectedTokenValue = "'" ++ [escChar] ++ "'"
  in lexTest input [tok (LitChar escChar) expectedTokenValue 1 1, tok EOF "" 1 7]

test_lit_char_hex_escape :: TestTree
test_lit_char_hex_escape = testCase "Char with hex escape \\x1b" $
  let escChar = '\ESC'
      input = "'\\x1b'"
      expectedTokenValue = "'" ++ [escChar] ++ "'"
  in lexTest input [tok (LitChar escChar) expectedTokenValue 1 1, tok EOF "" 1 7]
