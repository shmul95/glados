module Lexer.TokensSpecs (tokensTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))

--
-- public
--

tokensTests :: TestTree
tokensTests =
  testGroup
    "Tokens Data Types Tests"
    [ tokenKindDerivingTests
    , tokenDerivingTests
    , tokenFieldAccessTests
    ]

--
-- private
--

--
-- TokenKind Deriving Tests
--

testTokenKindGroup :: String -> [TokenKind] -> TestTree
testTokenKindGroup name kinds =
  testGroup name
    [ testCase "Show Coverage" $ mapM_ (\k -> assertBool ("Show " ++ show k) (not (null (show k)))) kinds
    , testCase "Eq Coverage (Self-Equality)" $ mapM_ (\k -> assertBool (show k ++ " == " ++ show k) (k == k)) kinds
    , testCase "Eq/Ord Coverage (Difference)" $
        case kinds of
          k1:k2:_ ->
            assertBool "Different are Unequal" (k1 /= k2)
          _ -> return ()
    ]

tokenKindDerivingTests :: TestTree
tokenKindDerivingTests =
  testGroup
    "TokenKind Deriving (Show, Eq, Ord)"
    [ testTokenKindGroup "Keywords"
        [ KwDef, KwReturn, KwStruct, KwIf, KwElse, KwFor, KwTo, KwIn, KwLoop, KwStop, KwNext, KwSomewhere, KwExport, KwExtern, KwSizeof, KwAs ]
    , testTokenKindGroup "Primitives"
        [ TypeI8, TypeI16, TypeI32, TypeI64, TypeF32, TypeF64, TypeBool, TypeU8, TypeU16, TypeU32, TypeU64, TypeChar, TypeString, TypeAny, TypeNull ]
    , testTokenKindGroup "Literals"
        [ LitInt 1, LitInt 2, LitFloat 1.0, LitFloat 2.0, LitString "a", LitString "b", LitChar 'a', LitChar 'b', LitBool True, LitBool False, LitNull ]
    , testTokenKindGroup "Identifiers"
        [ Identifier "a", Identifier "b" ]
    , testTokenKindGroup "Operators"
        [ OpPlus, OpMinus, OpMul, OpDiv, OpMod, OpAssign, OpAddAssign, OpSubAssign, OpMulAssign, OpDivAssign, OpModAssign, OpInc, OpDec, OpEq, OpNeq, OpLt, OpLte, OpGt, OpGte, OpAnd, OpOr, OpBitAnd, OpNot, OpBitNot, OpErrorProp, OpArrow, OpSquigArrow ]
    , testTokenKindGroup "Delimiters & EOF"
        [ LParen, RParen, LBrace, RBrace, Comma, Semicolon, Colon, Dot, Elipsis, RBracket, LBracket, EOF ]
    ]

test_field_accessors :: TestTree
test_field_accessors = testCase "Field Accessors coverage (tokenKind, tokenValue, tokenLine, tokenColumn)" $ do
  let kind = LitInt 42
  let value = "42"
  let line = 10
  let col = 5
  let t = Token kind value line col

  assertEqual "tokenKind accessor" kind (tokenKind t)
  assertEqual "tokenValue accessor" value (tokenValue t)
  assertEqual "tokenLine accessor" line (tokenLine t)
  assertEqual "tokenColumn accessor" col (tokenColumn t)

tokenFieldAccessTests :: TestTree
tokenFieldAccessTests =
  testGroup
    "Token Field Accessors"
    [ test_field_accessors
    ]

--
-- deriving
--

test_token_show :: TestTree
test_token_show = testCase "Token Show coverage" $ do
  let t = Token (LitInt 42) "42" 1 10
  let expected = "Token {tokenKind = LitInt 42, tokenValue = \"42\", tokenLine = 1, tokenColumn = 10}"
  assertEqual "Show should output all fields" expected (show t)

test_token_eq :: TestTree
test_token_eq = testCase "Token Eq coverage" $ do
  let t1 = Token KwDef "def" 1 1
  let t2 = Token KwDef "def" 1 1
  let t3 = Token KwDef "def" 1 2
  let t4 = Token KwReturn "return" 1 1

  assertBool "Equal tokens" (t1 == t2)
  assertBool "Different column" (t1 /= t3)
  assertBool "Different kind" (t1 /= t4)

test_token_ord :: TestTree
test_token_ord = testCase "Token Ord coverage" $ do
  let t1 = Token KwDef "def" 1 1
  let t2 = Token KwDef "def" 1 2
  let t3 = Token KwReturn "return" 1 1
  let t4 = Token (LitInt 1) "1" 1 1
  let t5 = Token (LitInt 2) "2" 1 1

  assertBool "Column Ord: t1 < t2" (t1 < t2)
  assertBool "Kind Ord: t1 < t3" (t1 < t3)
  assertBool "Kind Ord: LitInt 1 < LitInt 2" (t4 < t5)

tokenDerivingTests :: TestTree
tokenDerivingTests =
  testGroup
    "Token Deriving (Show, Eq, Ord) and Field Coverage"
    [ test_token_show
    , test_token_eq
    , test_token_ord
    ]
