module Main (main) where

import Test.Tasty
import Test.Tasty.Runners (NumThreads(..))

import Core.CLISpecs (cliTests)
import Core.LibSpecs (libTests)
import Core.PipelinesSpecs (pipelinesTests)
import Core.LoggerSpecs (loggerTests)

import Lexer.LexerSpecs (lexerTests)
import Lexer.LexerIdentifiersSpecs (lexerIdentifiersTests)
import Lexer.LexerLiteralsSpecs (lexerLiteralsTests)
import Lexer.LexerParserSpecs (lexerParserTests)
import Lexer.LexerDelimitersSpecs (lexerDelimitersTests)
import Lexer.LexerKeywordsSpecs (lexerKeywordsTests)
import Lexer.LexerOperatorsSpecs (lexerOperatorsTests)
import Lexer.LexerPrimitivesSpecs (lexerPrimitivesTests)
import Lexer.TokensSpecs (tokensTests)

import AST.TypesSpecs (astTypesTests)

--
-- public
--

main :: IO ()
main =
  defaultMain $
    localOption (NumThreads 1) $
      testGroup
        "All Tests"
        [ coreSpecs
        , lexerSpecs
        , astSpecs
        ]

--
-- private
--

coreSpecs :: TestTree
coreSpecs =
  testGroup
    "Core Tests"
    [ libTests
    , pipelinesTests
    , cliTests
    , loggerTests
    ]

lexerSpecs :: TestTree
lexerSpecs =
  testGroup
    "Lexer Tests"
    [ lexerTests
    , lexerIdentifiersTests
    , lexerLiteralsTests
    , lexerParserTests
    , lexerDelimitersTests
    , lexerKeywordsTests
    , lexerOperatorsTests
    , lexerPrimitivesTests
    , tokensTests
    ]

astSpecs :: TestTree
astSpecs =
  testGroup
    "AST Tests"
    [ astTypesTests
    ]
