module AST.Parser.ParseBlockSpecs (parseBlockTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Rune.AST.Parser.ParseBlock
  ( parseBlock,
    parseBlockLoop,
    parseStatement,
    parseReturn,
    parseIf,
    parseFor,
    parseForRangeRest,
    parseForRangeRestNoInit,
    parseForEachRest,
    parseLoop,
    parseStop,
    parseNext,
  )
import Rune.AST.Types (Parser(..), ParserState(..))
import Rune.AST.Nodes
import qualified Rune.Lexer.Tokens as T
import Data.List (isInfixOf)

--
-- public
--

parseBlockTests :: TestTree
parseBlockTests =
  testGroup
    "AST Parser ParseBlock Tests"
    [ emptyBlockTest
    , implicitReturnTest
    , statementTests
    , parseBlockLoopTests
    , parseReturnTests
    , parseIfTests
    , parseForTests
    , parseForRangeRestTests
    , parseForRangeRestNoInitTests
    , parseForEachRestTests
    , parseLoopTests
    , parseStopTests
    , parseNextTests
    , parseStatementTests
    , contextErrorTests
    ]

--
-- helpers
--

-- | Helper to create a test SourcePos
testPos :: SourcePos
testPos = SourcePos "test" 1 1

tok :: T.TokenKind -> T.Token
tok k = T.Token k "" 1 1

runBlock :: [T.Token] -> Either String Block
runBlock ts =
  let state = ParserState (ts ++ [tok T.EOF]) 0 "test" 0
  in case runParser parseBlock state of
       Right (block, _) -> Right block
       Left err -> Left err

assertBlockParse :: String -> [T.Token] -> Block -> IO ()
assertBlockParse msg tokens expected =
  case runBlock tokens of
    Right actual -> assertEqual msg expected actual
    Left err -> assertBool ("Parse failed: " ++ err) False

runP :: Parser a -> [T.Token] -> Int -> Either String a
runP parser ts loopDepth =
  let state = ParserState (ts ++ [tok T.EOF]) 0 "test" loopDepth
  in case runParser parser state of
       Right (res, _) -> Right res
       Left err -> Left err

assertP :: (Eq a, Show a) => String -> Parser a -> [T.Token] -> Int -> a -> IO ()
assertP msg parser tokens loopDepth expected =
  case runP parser tokens loopDepth of
    Right actual -> assertEqual msg expected actual
    Left err -> assertBool ("Parse failed: " ++ err) False

assertFail :: Show a => String -> Parser a -> [T.Token] -> Int -> String -> IO ()
assertFail _ parser tokens loopDepth expectedSubstr =
  case runP parser tokens loopDepth of
    Right actual -> assertBool ("Parse unexpectedly succeeded with: " ++ show actual) False
    Left err -> assertBool ("Expected error containing: " ++ expectedSubstr ++ "\nActual error: " ++ err) (isInfixOf expectedSubstr err)

--
-- tests
--

emptyBlockTest :: TestTree
emptyBlockTest = testCase "Empty Block" $
  assertBlockParse "Empty" [tok T.LBrace, tok T.RBrace] []

implicitReturnTest :: TestTree
implicitReturnTest = testCase "Implicit Return" $
  assertBlockParse "Implicit Return"
    [tok T.LBrace, tok (T.LitInt 1), tok T.RBrace]
    [StmtReturn testPos (Just (ExprLitInt testPos 1))]

statementTests :: TestTree
statementTests = testGroup "Integration Stmt Tests (via parseBlock)"
  [ testCase "Multiple Statements" $
      assertBlockParse "Return; Return;"
        [tok T.LBrace, tok T.KwReturn, tok T.Semicolon, tok T.KwReturn, tok T.Semicolon, tok T.RBrace]
        [StmtReturn testPos Nothing, StmtReturn testPos Nothing]
  , testCase "Var Decl with init" $
      assertBlockParse "x = 1;"
        [tok T.LBrace, tok (T.Identifier "x"), tok T.OpAssign, tok (T.LitInt 1), tok T.Semicolon, tok T.RBrace]
        [StmtVarDecl testPos "x" Nothing (ExprLitInt testPos 1)]
  , testCase "Compound Assign" $
      assertBlockParse "x += 1;"
        [tok T.LBrace, tok (T.Identifier "x"), tok T.OpAddAssign, tok (T.LitInt 1), tok T.Semicolon, tok T.RBrace]
        [StmtAssignment testPos (ExprVar testPos "x") (ExprBinary testPos Add (ExprVar testPos "x") (ExprLitInt testPos 1))]
  , testCase "Expr;" $
      assertBlockParse "1;"
        [tok T.LBrace, tok (T.LitInt 1), tok T.Semicolon, tok T.RBrace]
        [StmtExpr (SourcePos "test" 1 1) (ExprLitInt (SourcePos "test" 1 1) 1)]
  ]

parseBlockLoopTests :: TestTree
parseBlockLoopTests = testGroup "parseBlockLoop Tests"
  [ testCase "Empty block body" $
      assertP "Returns [] on RBrace" parseBlockLoop [tok T.RBrace] 0 []
  , testCase "Single statement" $
      assertP "Parses one statement" parseBlockLoop [tok T.KwReturn, tok T.Semicolon, tok T.RBrace] 0 [StmtReturn testPos Nothing]
  , testCase "Multiple statements" $
      assertP "Parses multiple statements" parseBlockLoop [tok T.KwStop, tok T.Semicolon, tok T.KwNext, tok T.Semicolon, tok T.RBrace] 1 [StmtStop testPos, StmtNext testPos]
  ]

parseStatementTests :: TestTree
parseStatementTests = testGroup "parseStatement Tests"
  [ testCase "Routes to parseReturn" $
      assertP "Parses return;" parseStatement [tok T.KwReturn, tok T.Semicolon] 0 (StmtReturn testPos Nothing)
  , testCase "Routes to parseIf" $
      assertP "Parses if true {}" parseStatement [tok T.KwIf, tok (T.LitBool True), tok T.LBrace, tok T.RBrace] 0 (StmtIf testPos (ExprLitBool testPos True) [] Nothing)
  , testCase "Routes to parseLoop" $
      assertP "Parses loop {}" parseStatement [tok T.KwLoop, tok T.LBrace, tok T.RBrace] 0 (StmtLoop testPos [])
  , testCase "Routes to parseStop" $
      assertP "Parses stop;" parseStatement [tok T.KwStop, tok T.Semicolon] 1 (StmtStop testPos)
  , testCase "Routes to parseNext" $
      assertP "Parses next;" parseStatement [tok T.KwNext, tok T.Semicolon] 1 (StmtNext testPos)
  , testCase "Routes to parseVariable (var decl)" $
      assertP "Parses x: i32;" parseStatement [tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32, tok T.Semicolon] 0 (StmtVarDecl testPos "x" (Just TypeI32) (ExprLitNull testPos))
  ]

parseReturnTests :: TestTree
parseReturnTests = testGroup "parseReturn Tests"
  [ testCase "Return with no expression" $
      assertP "Parses return;" parseReturn [tok T.KwReturn, tok T.Semicolon] 0 (StmtReturn testPos Nothing)
  , testCase "Return with expression" $
      assertP "Parses return 1;" parseReturn [tok T.KwReturn, tok (T.LitInt 1), tok T.Semicolon] 0 (StmtReturn testPos (Just (ExprLitInt testPos 1)))
  , testCase "Fails on missing semicolon" $
      assertFail "return 1 without ;" parseReturn [tok T.KwReturn, tok (T.LitInt 1)] 0 "Expected Semicolon"
  ]

parseIfTests :: TestTree
parseIfTests = testGroup "parseIf Tests"
  [ testCase "If (no else)" $
      let tokens = [tok T.KwIf, tok (T.LitBool True), tok T.LBrace, tok T.RBrace]
      in assertP "Parses if true {}" parseIf tokens 0 (StmtIf testPos (ExprLitBool testPos True) [] Nothing)
  , testCase "If Else (block)" $
      let tokens = [tok T.KwIf, tok (T.LitBool True), tok T.LBrace, tok T.RBrace, tok T.KwElse, tok T.LBrace, tok T.RBrace]
      in assertP "Parses if {} else {}" parseIf tokens 0 (StmtIf testPos (ExprLitBool testPos True) [] (Just []))
  , testCase "If Else If" $
      let tokens = [tok T.KwIf, tok (T.LitBool True), tok T.LBrace, tok T.RBrace, tok T.KwElse, tok T.KwIf, tok (T.LitBool False), tok T.LBrace, tok T.RBrace]
      in assertP "Parses if {} else if {}" parseIf tokens 0 (StmtIf testPos (ExprLitBool testPos True) [] (Just [StmtIf testPos (ExprLitBool testPos False) [] Nothing]))
  , testCase "Fails on missing condition" $
      assertFail "if without condition" parseIf [tok T.KwIf, tok T.LBrace, tok T.RBrace] 0 "in if condition"
  ]

parseForTests :: TestTree
parseForTests = testGroup "parseFor Tests"
  [ testCase "Routes to parseForRangeRest (with '=')" $
      let tokens = [tok T.KwFor, tok (T.Identifier "i"), tok T.OpAssign, tok (T.LitInt 0), tok T.KwTo, tok (T.LitInt 10), tok T.LBrace, tok T.RBrace]
      in assertP "for i = 0 to 10 {}" parseFor tokens 0 (StmtFor testPos "i" Nothing (Just (ExprLitInt testPos 0)) (ExprLitInt testPos 10) [])
  , testCase "Routes to parseForRangeRestNoInit (with 'to')" $
      let tokens = [tok T.KwFor, tok (T.Identifier "i"), tok T.KwTo, tok (T.LitInt 10), tok T.LBrace, tok T.RBrace]
      in assertP "for i to 10 {}" parseFor tokens 0 (StmtFor testPos "i" Nothing Nothing (ExprLitInt testPos 10) [])
  , testCase "Routes to parseForEachRest (with 'in')" $
      let tokens = [tok T.KwFor, tok (T.Identifier "x"), tok T.KwIn, tok (T.Identifier "list"), tok T.LBrace, tok T.KwReturn, tok T.Semicolon, tok T.RBrace]
          expected = StmtForEach testPos "x" Nothing (ExprVar testPos "list") [StmtReturn testPos Nothing]
      in assertP "for x in list {return;}" parseFor tokens 0 expected
  , testCase "Fails on invalid token after variable" $
      assertFail "for i [invalid] to" parseFor [tok T.KwFor, tok (T.Identifier "i"), tok T.LBrace] 0 "Expected '=' or 'to' or 'in'"
  ]

parseForRangeRestTests :: TestTree
parseForRangeRestTests = testGroup "parseForRangeRest Tests"
  [ testCase "Full range for loop" $
      let tokens = [tok T.OpAssign, tok (T.LitInt 0), tok T.KwTo, tok (T.LitInt 10), tok T.LBrace, tok T.KwNext, tok T.Semicolon, tok T.RBrace]
          expected = StmtFor testPos "i" Nothing (Just (ExprLitInt testPos 0)) (ExprLitInt testPos 10) [StmtNext testPos]
      in assertP "for i = 0 to 10 {next;}" (parseForRangeRest "i" Nothing) tokens 0 expected
  , testCase "With type annotation" $
      let tokens = [tok T.OpAssign, tok (T.LitInt 0), tok T.KwTo, tok (T.LitInt 10), tok T.LBrace, tok T.RBrace]
          expected = StmtFor testPos "i" (Just TypeI32) (Just (ExprLitInt testPos 0)) (ExprLitInt testPos 10) []
      in assertP "for i: i32 = 0 to 10 {}" (parseForRangeRest "i" (Just TypeI32)) tokens 0 expected
  ]

parseForRangeRestNoInitTests :: TestTree
parseForRangeRestNoInitTests = testGroup "parseForRangeRestNoInit Tests"
  [ testCase "Range for loop no init" $
      let tokens = [tok T.KwTo, tok (T.LitInt 10), tok T.LBrace, tok T.RBrace]
          expected = StmtFor testPos "i" Nothing Nothing (ExprLitInt testPos 10) []
      in assertP "for i to 10 {}" (parseForRangeRestNoInit "i" Nothing) tokens 0 expected
  , testCase "With type annotation" $
      let tokens = [tok T.KwTo, tok (T.LitInt 10), tok T.LBrace, tok T.RBrace]
          expected = StmtFor testPos "i" (Just TypeI32) Nothing (ExprLitInt testPos 10) []
      in assertP "for i: i32 to 10 {}" (parseForRangeRestNoInit "i" (Just TypeI32)) tokens 0 expected
  ]

parseForEachRestTests :: TestTree
parseForEachRestTests = testGroup "parseForEachRest Tests"
  [ testCase "For each loop" $
      let tokens = [tok T.KwIn, tok (T.Identifier "list"), tok T.LBrace, tok T.KwReturn, tok T.Semicolon, tok T.RBrace]
          expected = StmtForEach testPos "x" Nothing (ExprVar testPos "list") [StmtReturn testPos Nothing]
      in assertP "for x in list {return;}" (parseForEachRest "x" Nothing) tokens 0 expected
  , testCase "With type annotation" $
      let tokens = [tok T.KwIn, tok (T.Identifier "list"), tok T.LBrace, tok T.KwReturn, tok T.Semicolon, tok T.RBrace]
          expected = StmtForEach testPos "x" (Just TypeI32) (ExprVar testPos "list") [StmtReturn testPos Nothing]
      in assertP "for x: i32 in list {return;}" (parseForEachRest "x" (Just TypeI32)) tokens 0 expected
  ]

parseLoopTests :: TestTree
parseLoopTests = testGroup "parseLoop Tests"
  [ testCase "Loop with body" $
      let tokens = [tok T.KwLoop, tok T.LBrace, tok T.KwNext, tok T.Semicolon, tok T.RBrace]
      in assertP "loop {next;}" parseLoop tokens 0 (StmtLoop testPos [StmtNext testPos])
  , testCase "Loop with empty body" $
      let tokens = [tok T.KwLoop, tok T.LBrace, tok T.RBrace]
      in assertP "loop {}" parseLoop tokens 0 (StmtLoop testPos [])
  ]

parseStopTests :: TestTree
parseStopTests = testGroup "parseStop Tests"
  [ testCase "Stop inside loop" $
      assertP "stop; (in loop)" parseStop [tok T.KwStop, tok T.Semicolon] 1 (StmtStop testPos)
  , testCase "Stop outside loop fails" $
      assertFail "stop; (outside loop)" parseStop [tok T.KwStop, tok T.Semicolon] 0 "'stop' statement can only be used inside a loop"
  , testCase "Fails on missing semicolon" $
      assertFail "stop without ;" parseStop [tok T.KwStop] 1 "Expected Semicolon"
  ]

parseNextTests :: TestTree
parseNextTests = testGroup "parseNext Tests"
  [ testCase "Next inside loop" $
      assertP "next; (in loop)" parseNext [tok T.KwNext, tok T.Semicolon] 1 (StmtNext testPos)
  , testCase "Next outside loop fails" $
      assertFail "next; (outside loop)" parseNext [tok T.KwNext, tok T.Semicolon] 0 "'next' statement can only be used inside a loop"
  , testCase "Fails on missing semicolon" $
      assertFail "next without ;" parseNext [tok T.KwNext] 1 "Expected Semicolon"
  ]

contextErrorTests :: TestTree
contextErrorTests = testGroup "Context Error Tests"
  [ testCase "parseStatement T.KwIf context" $
      assertFail "parseStatement KwIf" parseStatement [tok T.KwIf, tok T.KwIf] 0 "If statement"
  , testCase "parseStatement T.KwFor context" $
      assertFail "parseStatement KwFor" parseStatement [tok T.KwFor, tok T.KwFor] 0 "For loop"
  , testCase "parseStatement T.KwLoop context" $
      assertFail "parseStatement KwLoop" parseStatement [tok T.KwLoop, tok T.KwLoop] 0 "Loop statement"
  , testCase "parseStatement T.KwStop context" $
      assertFail "parseStatement KwStop" parseStatement [tok T.KwStop, tok T.KwStop] 1 "Stop statement"
  , testCase "parseStatement T.KwNext context" $
      assertFail "parseStatement KwNext" parseStatement [tok T.KwNext, tok T.KwNext] 1 "Next statement"

  , testCase "parseReturn 'return value' context" $
      assertFail "parseReturn return value" parseReturn [tok T.KwReturn, tok (T.Identifier "x"), tok T.OpPlus] 0 "Expected Semicolon"

  , testCase "parseIf 'if block' context" $
      assertFail "parseIf if block" parseIf [tok T.KwIf, tok (T.LitBool True), tok T.KwIf] 0 "if block"

  , testCase "parseIf 'else block' context" $
      let tokens = [ tok T.KwIf , tok (T.LitBool True) , tok T.LBrace , tok T.RBrace , tok T.KwElse , tok T.LBrace , tok T.RBrace ]
      in assertP "parseIf else block" parseIf tokens 0 (StmtIf testPos (ExprLitBool testPos True) [] (Just []))

  , testCase "parseForRangeRest 'start index' context" $
      assertFail "parseForRangeRest start index" (parseForRangeRest "i" Nothing) [tok T.OpAssign, tok T.KwTo] 0 "start index"
  , testCase "parseForRangeRest 'end index' context" $
      assertFail "parseForRangeRest end index" (parseForRangeRest "i" Nothing) [tok T.OpAssign, tok (T.LitInt 0), tok T.KwTo, tok T.LBrace] 0 "end index"
  , testCase "parseForRangeRest 'for block' context" $
      assertFail "parseForRangeRest for block" (parseForRangeRest "i" Nothing) [tok T.OpAssign, tok (T.LitInt 0), tok T.KwTo, tok (T.LitInt 10), tok T.KwTo] 0 "for block"
  , testCase "parseForRangeRestNoInit 'end index' context" $
      assertFail "parseForRangeRestNoInit end index" (parseForRangeRestNoInit "i" Nothing) [tok T.KwTo, tok T.LBrace] 0 "end index"
  , testCase "parseForRangeRestNoInit 'for block' context" $
      assertFail "parseForRangeRestNoInit for block" (parseForRangeRestNoInit "i" Nothing) [tok T.KwTo, tok (T.LitInt 10), tok T.KwTo] 0 "for block"
  , testCase "parseForEachRest 'iterable expression' context" $
      assertFail "parseForEachRest iterable expression" (parseForEachRest "x" Nothing) [tok T.KwIn, tok T.LBrace] 0 "iterable expression"
  , testCase "parseForEachRest 'for-each block' context" $
      assertFail "parseForEachRest for-each block" (parseForEachRest "x" Nothing) [tok T.KwIn, tok (T.Identifier "list"), tok T.KwIn] 0 "for-each block"
  , testCase "parseLoop 'loop block' context" $
      assertFail "parseLoop loop block" parseLoop [tok T.KwLoop, tok T.KwLoop] 0 "loop block"
  ]
