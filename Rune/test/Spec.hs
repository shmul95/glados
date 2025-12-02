module Main (main) where

import AST.NodesSpec (astNodesTests)
import AST.ParserSpec (astParserTests)
import AST.PrinterSpec (astPrinterTests)
import AST.ProgramSyntaxSpec (programSyntaxTests)
import Backend.X86_64Spec (x86_64Tests)
import CLISpec (cliTests)
import IR.IRNodesSpecs (irNodesTests)
import IR.IRSpec (irTests)
import Lexer.LexerSpec (lexerTests)
import Lexer.TokensSpec (tokensTests)
import PipelinesSpec (pipelinesTests)
import Semantics.FuncSpec (funcSemanticsTests)
import Semantics.VarsSpec (varsSemanticsTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

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
        semanticsSpecs,
        irSpecs,
        backendSpecs
      ]

--
-- private
--

coreSpecs :: TestTree
coreSpecs =
  testGroup
    "Core Tests"
    [ pipelinesTests,
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
      varsSemanticsTests
    ]

irSpecs :: TestTree
irSpecs =
  testGroup
    "IR Tests"
    [ irTests,
      irNodesTests
    ]

backendSpecs :: TestTree
backendSpecs =
  testGroup
    "Backend Tests"
    [ x86_64Tests
    ]
