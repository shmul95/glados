module Lexer.LexerOperatorsSpecs (lexerOperatorsTests) where

import Test.Tasty (TestTree, testGroup)
import Rune.Lexer.Tokens (TokenKind (..))
import Test.Tasty.HUnit (testCase)
import Lexer.Utils (tok, lexTest)

--
-- public
--

lexerOperatorsTests :: TestTree
lexerOperatorsTests =
  testGroup
    "LexerOperators Tests"
    [ test_op_arrows
    , test_op_equality
    , test_op_comparison
    , test_op_logical
    , test_op_assign_compound
    , test_op_inc_dec
    , test_op_arithmetic_simple
    , test_op_error_prop
    , test_op_priority_lt_lte
    , test_op_priority_gt_gte
    ]

--
-- private
--

test_op_arrows :: TestTree
test_op_arrows = testCase "Op SquigArrow (~>) and Op Arrow (->)" $
  lexTest "~> ->" [tok OpSquigArrow "~>" 1 1, tok OpArrow "->" 1 4, tok EOF "" 1 6]

test_op_equality :: TestTree
test_op_equality = testCase "Op Eq (==) and Op Neq (!=)" $
  lexTest "== !=" [tok OpEq "==" 1 1, tok OpNeq "!=" 1 4, tok EOF "" 1 6]

test_op_comparison :: TestTree
test_op_comparison = testCase "Op Lt (<), Gt (>)" $
  lexTest "< >" [tok OpLt "<" 1 1, tok OpGt ">" 1 3, tok EOF "" 1 4]

test_op_logical :: TestTree
test_op_logical = testCase "Op And (&&) and Op Or (||)" $
  lexTest "&& ||" [tok OpAnd "&&" 1 1, tok OpOr "||" 1 4, tok EOF "" 1 6]

test_op_assign_compound :: TestTree
test_op_assign_compound = testCase "Compound Assignments (+=-=*=/=%=)" $
  lexTest "+= -= *= /= %="
    [ tok OpAddAssign "+=" 1 1, tok OpSubAssign "-=" 1 4, tok OpMulAssign "*=" 1 7,
      tok OpDivAssign "/=" 1 10, tok OpModAssign "%=" 1 13, tok EOF "" 1 15 ]

test_op_inc_dec :: TestTree
test_op_inc_dec = testCase "Op Inc (++) and Op Dec (--)" $
  lexTest "++ --" [tok OpInc "++" 1 1, tok OpDec "--" 1 4, tok EOF "" 1 6]

test_op_arithmetic_simple :: TestTree
test_op_arithmetic_simple = testCase "Simple Arithmetic (+ - * / % =)" $
  lexTest "+ - * / % ="
    [ tok OpPlus "+" 1 1, tok OpMinus "-" 1 3, tok OpMul "*" 1 5, tok OpDiv "/" 1 7,
      tok OpMod "%" 1 9, tok OpAssign "=" 1 11, tok EOF "" 1 12 ]

test_op_error_prop :: TestTree
test_op_error_prop = testCase "Op Error Prop (?)" $
  lexTest "?" [tok OpErrorProp "?" 1 1, tok EOF "" 1 2]

test_op_priority_lt_lte :: TestTree
test_op_priority_lt_lte = testCase "Priority check: < vs <=" $
  lexTest "<=" [tok OpLte "<=" 1 1, tok EOF "" 1 3]

test_op_priority_gt_gte :: TestTree
test_op_priority_gt_gte = testCase "Priority check: > vs >=" $
  lexTest ">=" [tok OpGte ">=" 1 1, tok EOF "" 1 3]
