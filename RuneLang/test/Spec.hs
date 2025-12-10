module Main (main) where

import AST.NodesSpec (astNodesTests)
import AST.ParserSpec (astParserTests)
import AST.ParseTypesSpec (parseTypesTests)
import AST.PrinterSpec (astPrinterTests)
import AST.ProgramSyntaxSpec (programSyntaxTests)
import Backend.HelpersSpec (backendHelpersTests)
import Backend.TypesSpec (backendTypesTests)
import Backend.X86_64.CodegenExtendedSpec (codegenExtendedTests)
import Backend.X86_64.CodegenSpec (codegenTests)
import Backend.X86_64.CompareSpec (compareTests)
import Backend.X86_64.RegistersSpec (registersTests)
import CLISpec (cliTests)
import IR.IRExpressionsSpec (irExpressionsTests)
import IR.IRStatementsSpec (irStatementsTests)
import IR.IRGeneratorSpec (irGeneratorTests)
import IR.IRHelpersSpec (irHelpersTests)
import IR.IRNodesSpecs (irNodesTests)
import IR.IRPrinterSpec (irPrinterTests)
import IR.IRShowCallSpec (irShowCallTests)
import IR.IRControlFlowSpec (irControlFlowTests)
import Lexer.LexerSpec (lexerTests)
import Lexer.TokensSpec (tokensTests)
import LibSpec (libTests)
import PipelinesSpec (pipelinesTests)
import Semantics.FuncSpec (funcSemanticsTests)
import Semantics.OpTypeSpec (opTypeSemanticsTests)
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
    [ libTests,
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
      parseTypesTests,
      programSyntaxTests,
      astPrinterTests
    ]

semanticsSpecs :: TestTree
semanticsSpecs =
  testGroup
    "Semantics Tests"
    [ funcSemanticsTests,
      opTypeSemanticsTests,
      varsSemanticsTests
    ]

irSpecs :: TestTree
irSpecs =
  testGroup
    "IR Tests"
    [ irNodesTests,
      irHelpersTests,
      irGeneratorTests,
      irStatementsTests,
      irPrinterTests,
      irExpressionsTests,
      irControlFlowTests,
      irShowCallTests
    ]

backendSpecs :: TestTree
backendSpecs =
  testGroup
    "Backend Tests"
    [ backendTypesTests,
      backendHelpersTests,
      registersTests,
      compareTests,
      codegenTests,
      codegenExtendedTests
    ]
