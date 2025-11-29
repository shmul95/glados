module Main (main) where

import AST.NodesSpec (astNodesTests)
import AST.ParserSpec (astParserTests)
import AST.PrinterSpec (astPrinterTests)
import AST.ProgramSyntaxSpec (programSyntaxTests)
import CLISpec (cliTests)
import Lexer.LexerSpec (lexerTests)
import Lexer.TokensSpec (tokensTests)
import LoggerSpec (loggerTests)
import PipelinesSpec (pipelinesTests)
import Semantics.FuncSpec (funcSemanticsTests)
import Semantics.VarsSpec (varsSemanticsTests)
import Test.Tasty

--
-- public
--

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ coreSpecs,
        lexerSpecs,
        astSpecs,
        semanticsSpecs
      ]

--
-- private
--

coreSpecs :: TestTree
coreSpecs =
  testGroup
    "Core Tests"
    [ loggerTests,
      pipelinesTests,
      cliTests
    ]

lexerSpecs :: TestTree
lexerSpecs =
  testGroup
    "Lexer Tests"
    [ tokensTests,
      lexerTests
    ]

astSpecs :: TestTree
astSpecs =
  testGroup
    "AST Tests"
    [ astNodesTests,
      astParserTests,
      programSyntaxTests,
      astPrinterTests
    ]

semanticsSpecs :: TestTree
semanticsSpecs =
  testGroup
    "Semantics Tests"
    [ funcSemanticsTests,
      varsSemanticsTests,
      cliTests,
      lexerTests,
      pipelinesTests,
      loggerTests,
      astNodesTests,
      astParserTests,
      tokensTests
    ]
