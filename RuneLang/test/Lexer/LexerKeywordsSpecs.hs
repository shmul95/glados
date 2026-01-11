module Lexer.LexerKeywordsSpecs (lexerKeywordsTests) where

import Test.Tasty (TestTree, testGroup)
import Rune.Lexer.Tokens (TokenKind (..))
import Test.Tasty.HUnit (testCase)
import Lexer.Utils (tok, lexTest)

--
-- public
--

lexerKeywordsTests :: TestTree
lexerKeywordsTests =
  testGroup
    "LexerKeywords Tests"
    [ test_kw_definition
    , test_kw_control_flow_if_for
    , test_kw_control_flow_loop_next_stop
    , test_kw_override
    , test_kw_logical
    , test_kw_somewhere_export
    , test_kw_reserved_check
    , test_kw_sizeof
    ]

--
-- private
--

test_kw_logical :: TestTree
test_kw_logical = testCase "Kw And, Or, Not (mapped to OpAnd, OpOr, OpNot)" $
  lexTest "and or not"
    [ tok OpAnd "and" 1 1, tok OpOr "or" 1 5, tok OpNot "not" 1 8, tok EOF "" 1 11 ]

test_kw_definition :: TestTree
test_kw_definition = testCase "Kw Def, Return, Struct" $
  lexTest "def return struct"
    [ tok KwDef "def" 1 1, tok KwReturn "return" 1 5, tok KwStruct "struct" 1 12, tok EOF "" 1 18 ]

test_kw_control_flow_if_for :: TestTree
test_kw_control_flow_if_for = testCase "Kw If, Else, For, To, In" $
  lexTest "if else for to in"
    [ tok KwIf "if" 1 1, tok KwElse "else" 1 4, tok KwFor "for" 1 9, tok KwTo "to" 1 13, tok KwIn "in" 1 16, tok EOF "" 1 18 ]

test_kw_control_flow_loop_next_stop :: TestTree
test_kw_control_flow_loop_next_stop = testCase "Kw Loop, Next, Stop" $
  lexTest "loop next stop"
    [ tok KwLoop "loop" 1 1, tok KwNext "next" 1 6, tok KwStop "stop" 1 11, tok EOF "" 1 15 ]

test_kw_override :: TestTree
test_kw_override = testCase "Kw Override" $
  lexTest "override"
    [ tok KwOverride "override" 1 1, tok EOF "" 1 9 ]

test_kw_somewhere_export :: TestTree
test_kw_somewhere_export = testCase "Kw Somewhere, Export" $
  lexTest "somewhere export"
    [ tok KwSomewhere "somewhere" 1 1, tok KwExport "export" 1 11, tok EOF "" 1 17 ]

test_kw_reserved_check :: TestTree
test_kw_reserved_check = testCase "Keyword reserved check (defX should be ID)" $
  lexTest "defX struct_new"
    [ tok (Identifier "defX") "defX" 1 1, tok (Identifier "struct_new") "struct_new" 1 6, tok EOF "" 1 16 ]

test_kw_sizeof :: TestTree
test_kw_sizeof = testCase "Kw Sizeof" $
  lexTest "sizeof"
    [ tok KwSizeof "sizeof" 1 1, tok EOF "" 1 7 ]
