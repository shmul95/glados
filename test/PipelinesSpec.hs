module PipelinesSpec (pipelinesTests) where

import Rune.Lexer.Tokens (Token (..), TokenKind (..))
import Rune.Pipelines (compilePipeline, interpretPipeline)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- public
--

pipelinesTests :: TestTree
pipelinesTests =
  testGroup
    "Rune Pipelines Tests"
    [ compilePipelineTests,
      interpretPipelineTests
    ]

--
-- private
--

expectedTokens :: [Token]
expectedTokens =
  [ Token {tokenKind = KwDef, tokenValue = "def", tokenLine = 1, tokenColumn = 1},
    Token {tokenKind = Identifier "main", tokenValue = "main", tokenLine = 1, tokenColumn = 5},
    Token {tokenKind = LParen, tokenValue = "(", tokenLine = 1, tokenColumn = 9},
    Token {tokenKind = RParen, tokenValue = ")", tokenLine = 1, tokenColumn = 10},
    Token {tokenKind = OpArrow, tokenValue = "->", tokenLine = 1, tokenColumn = 12},
    Token {tokenKind = TypeNull, tokenValue = "null", tokenLine = 1, tokenColumn = 15},
    Token {tokenKind = LBrace, tokenValue = "{", tokenLine = 2, tokenColumn = 1},
    Token {tokenKind = Identifier "show", tokenValue = "show", tokenLine = 3, tokenColumn = 5},
    Token {tokenKind = LParen, tokenValue = "(", tokenLine = 3, tokenColumn = 9},
    Token {tokenKind = LitString "Hello, Rune!\n", tokenValue = "\"Hello, Rune!\n\"", tokenLine = 3, tokenColumn = 10},
    Token {tokenKind = RParen, tokenValue = ")", tokenLine = 3, tokenColumn = 26},
    Token {tokenKind = Semicolon, tokenValue = ";", tokenLine = 3, tokenColumn = 27},
    Token {tokenKind = RBrace, tokenValue = "}", tokenLine = 4, tokenColumn = 1},
    Token {tokenKind = EOF, tokenValue = "", tokenLine = 5, tokenColumn = 1}
  ]

compilePipelineTests :: TestTree
compilePipelineTests = testCase "compilePipeline" $ do
  tokens <- compilePipeline "examples/hello_rune.ru" "out.ir"
  tokens @?= expectedTokens

interpretPipelineTests :: TestTree
interpretPipelineTests = testCase "interpretPipeline" $ do
  tokens <- interpretPipeline "examples/hello_rune.ru"
  tokens @?= expectedTokens
