module Lexer.LexerDelimitersSpecs (lexerDelimitersTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Rune.Lexer.Tokens (TokenKind (..))
import Lexer.Utils (tok, lexTest)

--
-- public
--

lexerDelimitersTests :: TestTree
lexerDelimitersTests =
  testGroup
    "LexerDelimiters Tests"
    [ test_delimiters_parentheses
    , test_delimiters_braces
    , test_delimiters_comma
    , test_delimiters_semicolon
    , test_delimiters_colon
    , test_delimiters_dot
    , test_delimiters_brackets
    , test_delimiters_all_together
    ]

--
-- private
--

test_delimiters_parentheses :: TestTree
test_delimiters_parentheses = testCase "Parentheses ()" $
  lexTest "()" [tok LParen "(" 1 1, tok RParen ")" 1 2, tok EOF "" 1 3]

test_delimiters_braces :: TestTree
test_delimiters_braces = testCase "Braces {}" $
  lexTest "{}" [tok LBrace "{" 1 1, tok RBrace "}" 1 2, tok EOF "" 1 3]

test_delimiters_comma :: TestTree
test_delimiters_comma = testCase "Comma ," $
  lexTest "," [tok Comma "," 1 1, tok EOF "" 1 2]

test_delimiters_semicolon :: TestTree
test_delimiters_semicolon = testCase "Semicolon ;" $
  lexTest ";" [tok Semicolon ";" 1 1, tok EOF "" 1 2]

test_delimiters_colon :: TestTree
test_delimiters_colon = testCase "Colon :" $
  lexTest ":" [tok Colon ":" 1 1, tok EOF "" 1 2]

test_delimiters_dot :: TestTree
test_delimiters_dot = testCase "Dot ." $
  lexTest "." [tok Dot "." 1 1, tok EOF "" 1 2]

test_delimiters_brackets :: TestTree
test_delimiters_brackets = testCase "Brackets []" $
  lexTest "[]" [tok LBracket "[" 1 1, tok RBracket "]" 1 2, tok EOF "" 1 3]

test_delimiters_all_together :: TestTree
test_delimiters_all_together = testCase "All delimiters separated" $
  lexTest "( ) { } , ; : . []"
    [ tok LParen "(" 1 1
    , tok RParen ")" 1 3
    , tok LBrace "{" 1 5
    , tok RBrace "}" 1 7
    , tok Comma "," 1 9
    , tok Semicolon ";" 1 11
    , tok Colon ":" 1 13
    , tok Dot "." 1 15
    , tok LBracket "[" 1 17
    , tok RBracket "]" 1 18
    , tok EOF "" 1 19
    ]
