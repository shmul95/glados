module Lexer.LexerIdentifiersSpecs (lexerIdentifiersTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (TokenKind (..))

import Lexer.Utils (tok, lexTest)

--
-- public
--

lexerIdentifiersTests :: TestTree
lexerIdentifiersTests =
  testGroup
    "LexerIdentifiers Tests"
    [ test_id_simple
    , test_id_underscore_start
    , test_id_with_numbers
    , test_id_underscore_and_numbers
    , test_id_single_underscore
    , test_id_fail_with_leading_number
    ]

--
-- private
--

test_id_simple :: TestTree
test_id_simple = testCase "Simple identifier (start with letter)" $
  lexTest "myVar" [tok (Identifier "myVar") "myVar" 1 1, tok EOF "" 1 6]

test_id_underscore_start :: TestTree
test_id_underscore_start = testCase "Identifier starting with underscore" $
  lexTest "_temp_var" [tok (Identifier "_temp_var") "_temp_var" 1 1, tok EOF "" 1 10]

test_id_with_numbers :: TestTree
test_id_with_numbers = testCase "Identifier with numbers" $
  lexTest "var123" [tok (Identifier "var123") "var123" 1 1, tok EOF "" 1 7]

test_id_underscore_and_numbers :: TestTree
test_id_underscore_and_numbers = testCase "Identifier with underscore and numbers" $
  lexTest "_1_var" [tok (Identifier "_1_var") "_1_var" 1 1, tok EOF "" 1 7]

test_id_single_underscore :: TestTree
test_id_single_underscore = testCase "Single underscore is an identifier" $
  lexTest "_" [tok (Identifier "_") "_" 1 1, tok EOF "" 1 2]

test_id_fail_with_leading_number :: TestTree
test_id_fail_with_leading_number = testCase "Identifier starting with number fails" $
  case lexer "test.rune" "1var" of
    Right tokens -> assertEqual "Should tokenize as LitInt then Id" 
      [ tok (LitInt 1) "1" 1 1
      , tok (Identifier "var") "var" 1 2
      , tok EOF "" 1 5
      ] tokens
    Left err -> assertFailure $ "Lexing failed unexpectedly: " ++ show err
