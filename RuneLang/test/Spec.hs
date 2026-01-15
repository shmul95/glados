module Main (main) where

import Test.Tasty
import Test.Tasty.Runners (NumThreads(..))

import Core.CLISpecs (cliTests)
import Core.LibSpecs (libTests)
import Core.PipelinesSpecs (pipelinesTests)
import Core.LoggerSpecs (loggerTests)
import Core.SanityChecksSpec (sanityChecksTests)

import PreprocessSpecs (preprocessTests)

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
import Semantics.StructSpecs (structSemanticsTests)
import Semantics.GenericSpecs (genericSemanticsTests)

import IR.NodesSpecs (irNodesTests)
import IR.IRHelpersSpecs (irHelpersTests)
import IR.ArraySpecs (arrayTests)
import IR.Generator.Expression.ArraySpecs (arrayExprTests)
import IR.GeneratorSpecs (generatorTests)
import IR.Generator.GenTopLevelSpecs (genTopLevelTests)
import IR.Generator.GenStatementSpecs (genStatementTests)
import IR.Generator.GenExpressionSpecs (genExpressionTests)
import IR.Generator.Expression.BinarySpecs (binaryExprTests)
import IR.Generator.Expression.UnarySpecs (unaryExprTests)
import IR.Generator.Expression.LiteralsSpecs (literalsTests)
import IR.Generator.Expression.CallSpecs (callExprTests)
import IR.Generator.Expression.StructSpecs (structExprTests)
import IR.Generator.Statement.ControlFlowSpecs (controlFlowTests)
import IR.Generator.Statement.LoopsSpecs (loopsTests)
import IR.PrinterSpecs (irPrinterTests)
import IR.OptimizerSpecs (optimizerTests)

import Backend.HelpersSpecs (backendHelpersTests)
import Backend.TypesSpecs (backendTypesTests)
import Backend.X86_64.RegistersSpecs (registersTests)
import Backend.X86_64.CompareSpecs (compareTests)
import Backend.X86_64.CodegenSpecs (codegenTests)
import Backend.X86_64.LoadStoreSpecs (loadStoreTests)
import Backend.X86_64.OperationsSpecs (operationsTests)
import Backend.X86_64.StructSpecs (structTests)

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
        , backendSpecs
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
    , sanityChecksTests
    , preprocessTests
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
    , structSemanticsTests
    , genericSemanticsTests
    ]

irSpecs :: TestTree
irSpecs =
  testGroup
    "IR Tests"
    [ irNodesTests
    , irHelpersTests
    , arrayTests
    , arrayExprTests
    , generatorTests
    , genTopLevelTests
    , genStatementTests
    , genExpressionTests
    , binaryExprTests
    , unaryExprTests
    , literalsTests
    , callExprTests
    , structExprTests
    , controlFlowTests
    , loopsTests
    , irPrinterTests
    , optimizerTests
    ]

backendSpecs :: TestTree
backendSpecs =
  testGroup
    "Backend Tests"
    [ backendHelpersTests
    , backendTypesTests
    , registersTests
    , compareTests
    , codegenTests
    , loadStoreTests
    , operationsTests
    , structTests
    ]
