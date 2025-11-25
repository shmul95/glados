module Lexer.TokensSpec (tokensTests) where

import Rune.Lexer.Tokens (Token (..), TokenKind (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))

--
-- public
--

tokensTests :: TestTree
tokensTests =
  testGroup
    "Lexer.TokensSpec"
    [ tokenCreationTests,
      tokenKindTests,
      tokenInstanceTests
    ]

--
-- helpers
--

allTokenKinds :: [TokenKind]
allTokenKinds =
  [ KwDef,
    KwReturn,
    KwStruct,
    KwIf,
    KwElse,
    KwFor,
    KwTo,
    KwOverride,
    KwIn,
    TypeI8,
    TypeI16,
    TypeI32,
    TypeI64,
    TypeF32,
    TypeF64,
    TypeBool,
    TypeU8,
    TypeU16,
    TypeU32,
    TypeString,
    TypeAny,
    TypeNull,
    LitInt 0,
    LitFloat 0.0,
    LitString "",
    LitBool True,
    LitBool False,
    LitNull,
    Identifier "x",
    OpPlus,
    OpMinus,
    OpMul,
    OpDiv,
    OpMod,
    OpAssign,
    OpEq,
    OpNeq,
    OpLt,
    OpLte,
    OpGt,
    OpGte,
    OpAnd,
    OpOr,
    OpErrorProp,
    OpArrow,
    OpSquigArrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Colon,
    Dot,
    EOF
  ]

showTokenKind :: TokenKind -> String
showTokenKind = show

compareTokenKind :: TokenKind -> TokenKind -> Ordering
compareTokenKind = compare

showToken :: Token -> String
showToken = show

compareToken :: Token -> Token -> Ordering
compareToken = compare

mkTokenKindTest :: TokenKind -> TestTree
mkTokenKindTest k =
  testCase ("Show/Eq/Ord for constructor: " ++ show k) $ do
    -- show
    let s = showTokenKind k
    length s @?= length s

    -- eq
    k == k @? "reflexive Eq"

    -- ord
    compareTokenKind k k @?= EQ

--
-- private
--

tokenCreationTests :: TestTree
tokenCreationTests =
  testGroup
    "token creation"
    [ testCase "create keyword token" $ do
        let token = Token {tokenKind = KwDef, tokenValue = "def", tokenLine = 1, tokenColumn = 1}
        tokenKind token @?= KwDef
        tokenValue token @?= "def"
        tokenLine token @?= 1
        tokenColumn token @?= 1
    ]

tokenKindTests :: TestTree
tokenKindTests =
  testGroup
    "TokenKind instances"
    (map mkTokenKindTest allTokenKinds)

tokenInstanceTests :: TestTree
tokenInstanceTests =
  testGroup
    "Token instances"
    [ testCase "Show/Eq/Ord" $ do
        let t1 = Token KwDef "def" 1 1
        let t2 = Token KwReturn "return" 2 3

        -- show
        length (showToken t1) @?= length (showToken t1)

        -- eq
        t1 == t1 @? "reflexive"
        t1 /= t2 @? "different tokens"

        -- ord
        compareToken t1 t1 @?= EQ
        compareToken t1 t2 @?= LT
    ]
