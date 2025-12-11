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
import AST.NodesSpecs (astNodesTests)
import AST.ParserHelperSpecs (parserHelperTests)
import AST.PrinterSpecs (astPrinterTests)
import AST.ParserSpecs (parserTests)

import Semantics.VarsSpecs (varsSemanticsTests)
import Semantics.OpTypeSpecs (opTypeSemanticsTests)
import Semantics.HelperSpecs (helperSemanticsTests)
import Semantics.FuncSpecs (funcSemanticsTests)

import IR.NodesSpecs (irNodesTests)
import IR.IRHelpersSpecs (irHelpersTests)
import IR.GeneratorSpecs (generatorTests)
import IR.PrinterSpecs (irPrinterTests)
import IR.OptimizerSpecs (optimizerTests)

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
        , semanticsSpecs
        , irSpecs
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
    , parserHelperTests
    , astNodesTests
    , astPrinterTests
    , parserTests
    ]

semanticsSpecs :: TestTree
semanticsSpecs =
  testGroup
    "Semantics Tests"
    [ varsSemanticsTests
    , opTypeSemanticsTests
    , helperSemanticsTests
    , funcSemanticsTests
    ]

irSpecs :: TestTree
irSpecs =
  testGroup
    "IR Tests"
    [ irNodesTests
    , irHelpersTests
    , generatorTests
    , irPrinterTests
    , optimizerTests
    ]
