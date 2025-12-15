module AST.Parser.ParseBlockSpecs (parseBlockTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Rune.AST.Parser.ParseBlock (parseBlock)
import Rune.AST.Types (Parser(..), ParserState(..))
import Rune.AST.Nodes
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseBlockTests :: TestTree
parseBlockTests =
  testGroup
    "AST Parser ParseBlock Tests"
    [ emptyBlockTest
    , statementTests
    , controlFlowTests
    , loopTests
    , varDeclTests
    , assignmentTests
    , exprStmtTests
    , implicitReturnTest
    ]

--
-- helpers
--

tok :: T.TokenKind -> T.Token
tok k = T.Token k "" 1 1

run :: [T.Token] -> Either String Block
run ts =
  let state = ParserState (ts ++ [tok T.EOF]) 0 "test" 0
  in case runParser parseBlock state of
       Right (block, _) -> Right block
       Left err -> Left err

assertParse :: String -> [T.Token] -> Block -> IO ()
assertParse msg tokens expected =
  case run tokens of
    Right actual -> assertEqual msg expected actual
    Left err -> assertBool ("Parse failed: " ++ err) False

--
-- tests
--

emptyBlockTest :: TestTree
emptyBlockTest = testCase "Empty Block" $
  assertParse "Empty" [tok T.LBrace, tok T.RBrace] []

statementTests :: TestTree
statementTests = testGroup "Statement Tests"
  [ testCase "Multiple Statements" $
      assertParse "Return; Return;"
        [tok T.LBrace, tok T.KwReturn, tok T.Semicolon, tok T.KwReturn, tok T.Semicolon, tok T.RBrace]
        [StmtReturn (SourcePos "test" 1 1) Nothing, StmtReturn (SourcePos "test" 1 1) Nothing]
  ]

controlFlowTests :: TestTree
controlFlowTests = testGroup "Control Flow Tests"
  [ testCase "Return (void)" $
      assertParse "return;"
        [tok T.LBrace, tok T.KwReturn, tok T.Semicolon, tok T.RBrace]
        [StmtReturn (SourcePos "test" 1 1) Nothing]
  
  , testCase "Return (value)" $
      assertParse "return 1;"
        [tok T.LBrace, tok T.KwReturn, tok (T.LitInt 1), tok T.Semicolon, tok T.RBrace]
        [StmtReturn (SourcePos "test" 1 1) (Just (ExprLitInt (SourcePos "test" 1 1) 1))]

  , testCase "If (no else)" $
      assertParse "if true { return; }"
        [tok T.LBrace, tok T.KwIf, tok (T.LitBool True), tok T.LBrace, tok T.KwReturn, tok T.Semicolon, tok T.RBrace, tok T.RBrace]
        [StmtIf (SourcePos "test" 1 1) (ExprLitBool (SourcePos "test" 1 1) True) [StmtReturn (SourcePos "test" 1 1) Nothing] Nothing]
  
  , testCase "If Else" $
      assertParse "if true {} else {}"
        [tok T.LBrace, tok T.KwIf, tok (T.LitBool True), tok T.LBrace, tok T.RBrace, tok T.KwElse, tok T.LBrace, tok T.RBrace, tok T.RBrace]
        [StmtIf (SourcePos "test" 1 1) (ExprLitBool (SourcePos "test" 1 1) True) [] (Just [])]
  ]

loopTests :: TestTree
loopTests = testGroup "Loop Tests"
  [ testCase "Loop" $
      assertParse "loop { stop; }"
        [tok T.LBrace, tok T.KwLoop, tok T.LBrace, tok T.KwStop, tok T.Semicolon, tok T.RBrace, tok T.RBrace]
        [StmtLoop (SourcePos "test" 1 1) [StmtStop (SourcePos "test" 1 1)]]
  
  , testCase "For (Range)" $
      assertParse "for i = 0 to 10 {}"
        [tok T.LBrace, tok T.KwFor, tok (T.Identifier "i"), tok T.OpAssign, tok (T.LitInt 0), tok T.KwTo, tok (T.LitInt 10), tok T.LBrace, tok T.RBrace, tok T.RBrace]
        [StmtFor (SourcePos "test" 1 1) "i" Nothing (Just (ExprLitInt (SourcePos "test" 1 1) 0)) (ExprLitInt (SourcePos "test" 1 1) 10) []]

  , testCase "For (Range No Init)" $
      assertParse "for i to 10 {}"
        [tok T.LBrace, tok T.KwFor, tok (T.Identifier "i"), tok T.KwTo, tok (T.LitInt 10), tok T.LBrace, tok T.RBrace, tok T.RBrace]
        [StmtFor (SourcePos "test" 1 1) "i" Nothing Nothing (ExprLitInt (SourcePos "test" 1 1) 10) []]
  
  , testCase "ForEach" $
      assertParse "for x in (list) {}"
        [tok T.LBrace, tok T.KwFor, tok (T.Identifier "x"), tok T.KwIn, tok T.LParen, tok (T.Identifier "list"), tok T.RParen, tok T.LBrace, tok T.RBrace, tok T.RBrace]
        [StmtForEach (SourcePos "test" 1 1) "x" Nothing (ExprVar (SourcePos "test" 1 1) "list") []]
  ]

varDeclTests :: TestTree
varDeclTests = testGroup "Var Decl Tests"
  [ testCase "Decl with init" $
      assertParse "x = 1;"
        [tok T.LBrace, tok (T.Identifier "x"), tok T.OpAssign, tok (T.LitInt 1), tok T.Semicolon, tok T.RBrace]
        [StmtVarDecl (SourcePos "test" 1 1) "x" Nothing (ExprLitInt (SourcePos "test" 1 1) 1)]
  
  , testCase "Decl with type" $
      assertParse "x: i32 = 1;"
        [tok T.LBrace, tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32, tok T.OpAssign, tok (T.LitInt 1), tok T.Semicolon, tok T.RBrace]
        [StmtVarDecl (SourcePos "test" 1 1) "x" (Just TypeI32) (ExprLitInt (SourcePos "test" 1 1) 1)]
  
  , testCase "Decl without init (but with type)" $
      assertParse "x: i32;"
        [tok T.LBrace, tok (T.Identifier "x"), tok T.Colon, tok T.TypeI32, tok T.Semicolon, tok T.RBrace]
        [StmtVarDecl (SourcePos "test" 1 1) "x" (Just TypeI32) (ExprLitNull (SourcePos "test" 1 1))]
  ]

assignmentTests :: TestTree
assignmentTests = testGroup "Assignment Tests"
  [ testCase "Assign" $
      -- Note: x = 1 is treated as VarDecl if "x" is identifier.
      -- To test Assignment, we need context where it is NOT VarDecl?
      -- parseVariable checks lookAheadIsVarDecl.
      -- x = 1; -> VarDecl
      -- x = 1; -> If we want assignment, it must NOT look like var decl?
      -- VarDecl: ID [Colon Type] = Expr ; OR ID : Type ;
      -- Assignment: LValue = Expr ;
      -- If I write "x = 1;", lookAheadIsVarDecl sees ID then Assign -> True.
      -- So it parses as VarDecl.
      -- To parse as Assignment, it should fail IsVarDecl check?
      -- But "x = 1" IS a valid VarDecl.
      -- What if I use field access? "x.y = 1;"
      assertParse "x.y = 1;"
        [tok T.LBrace, tok (T.Identifier "x"), tok T.Dot, tok (T.Identifier "y"), tok T.OpAssign, tok (T.LitInt 1), tok T.Semicolon, tok T.RBrace]
        [StmtAssignment (SourcePos "test" 1 1) (ExprAccess (SourcePos "test" 1 1) (ExprVar (SourcePos "test" 1 1) "x") "y") (ExprLitInt (SourcePos "test" 1 1) 1)]
  
  , testCase "Compound Assign" $
      assertParse "x += 1;"
        [tok T.LBrace, tok (T.Identifier "x"), tok T.OpAddAssign, tok (T.LitInt 1), tok T.Semicolon, tok T.RBrace]
        [StmtAssignment (SourcePos "test" 1 1) (ExprVar (SourcePos "test" 1 1) "x") (ExprBinary (SourcePos "test" 1 1) Add (ExprVar (SourcePos "test" 1 1) "x") (ExprLitInt (SourcePos "test" 1 1) 1))]
  ]

exprStmtTests :: TestTree
exprStmtTests = testGroup "Expr Stmt Tests"
  [ testCase "Expr;" $
      assertParse "1;"
        [tok T.LBrace, tok (T.LitInt 1), tok T.Semicolon, tok T.RBrace]
        [StmtExpr (SourcePos "test" 1 1) (ExprLitInt (SourcePos "test" 1 1) 1)]
  ]

implicitReturnTest :: TestTree
implicitReturnTest = testCase "Implicit Return" $
  assertParse "Implicit Return"
    [tok T.LBrace, tok (T.LitInt 1), tok T.RBrace]
    [StmtReturn (SourcePos "test" 1 1) (Just (ExprLitInt (SourcePos "test" 1 1) 1))]
