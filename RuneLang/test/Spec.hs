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
    ]
