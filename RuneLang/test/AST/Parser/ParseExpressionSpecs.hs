module AST.Parser.ParseExpressionSpecs (parseExpressionTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Rune.AST.Parser.ParseExpression (parseExpression)
import Rune.AST.Types (Parser(..), ParserState(..))
import Rune.AST.Nodes
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseExpressionTests :: TestTree
parseExpressionTests =
  testGroup
    "AST Parser ParseExpression Tests"
    [ literalTests
    , variableTests
    , binaryOpTests
    , unaryOpTests
    , postfixTests
    , structInitTests
    , parenthesesTests
    ]

--
-- helpers
--

tok :: T.TokenKind -> T.Token
tok k = T.Token k "" 1 1

run :: [T.Token] -> Either String Expression
run ts =
  let state = ParserState (ts ++ [tok T.EOF]) 0 "test" 0
  in case runParser parseExpression state of
       Right (expr, _) -> Right expr
       Left err -> Left err

assertParse :: String -> [T.Token] -> Expression -> IO ()
assertParse msg tokens expected =
  case run tokens of
    Right actual -> assertEqual msg expected actual
    Left err -> assertBool ("Parse failed: " ++ err) False

--
-- tests
--

literalTests :: TestTree
literalTests = testGroup "Literal Tests"
  [ testCase "Int" $ assertParse "Int" [tok (T.LitInt 1)] (ExprLitInt 1)
  , testCase "Float" $ assertParse "Float" [tok (T.LitFloat 1.5)] (ExprLitFloat 1.5)
  , testCase "String" $ assertParse "String" [tok (T.LitString "s")] (ExprLitString "s")
  , testCase "Char" $ assertParse "Char" [tok (T.LitChar 'c')] (ExprLitChar 'c')
  , testCase "Bool True" $ assertParse "Bool" [tok (T.LitBool True)] (ExprLitBool True)
  , testCase "Null" $ assertParse "Null" [tok T.LitNull] ExprLitNull
  ]

variableTests :: TestTree
variableTests = testGroup "Variable Tests"
  [ testCase "Identifier" $ assertParse "Var" [tok (T.Identifier "x")] (ExprVar "x")
  ]

binaryOpTests :: TestTree
binaryOpTests = testGroup "Binary Op Tests"
  [ testCase "Add" $
      assertParse "1 + 2" 
        [tok (T.LitInt 1), tok T.OpPlus, tok (T.LitInt 2)]
        (ExprBinary Add (ExprLitInt 1) (ExprLitInt 2))
  
  , testCase "Precedence Mul > Add" $
      assertParse "1 + 2 * 3"
        [tok (T.LitInt 1), tok T.OpPlus, tok (T.LitInt 2), tok T.OpMul, tok (T.LitInt 3)]
        (ExprBinary Add (ExprLitInt 1) (ExprBinary Mul (ExprLitInt 2) (ExprLitInt 3)))

  , testCase "Precedence Mul > Add (reverse)" $
      assertParse "1 * 2 + 3"
        [tok (T.LitInt 1), tok T.OpMul, tok (T.LitInt 2), tok T.OpPlus, tok (T.LitInt 3)]
        (ExprBinary Add (ExprBinary Mul (ExprLitInt 1) (ExprLitInt 2)) (ExprLitInt 3))

  , testCase "Logical Or < And" $
      assertParse "true || false && true"
        [tok (T.LitBool True), tok T.OpOr, tok (T.LitBool False), tok T.OpAnd, tok (T.LitBool True)]
        (ExprBinary Or (ExprLitBool True) (ExprBinary And (ExprLitBool False) (ExprLitBool True)))
  ]

unaryOpTests :: TestTree
unaryOpTests = testGroup "Unary Op Tests"
  [ testCase "Negate" $
      assertParse "-1" [tok T.OpMinus, tok (T.LitInt 1)] (ExprUnary Negate (ExprLitInt 1))
  , testCase "Not" $
      assertParse "!x" [tok T.OpNot, tok (T.Identifier "x")] (ExprUnary Not (ExprVar "x"))
  , testCase "PrefixInc" $
      assertParse "++x" [tok T.OpInc, tok (T.Identifier "x")] (ExprUnary PrefixInc (ExprVar "x"))
  ]

postfixTests :: TestTree
postfixTests = testGroup "Postfix Tests"
  [ testCase "Field Access" $
      assertParse "x.y" 
        [tok (T.Identifier "x"), tok T.Dot, tok (T.Identifier "y")]
        (ExprAccess (ExprVar "x") "y")
  
  , testCase "Call" $
      assertParse "f()"
        [tok (T.Identifier "f"), tok T.LParen, tok T.RParen]
        (ExprCall "f" [])

  , testCase "Call with args" $
      assertParse "f(1, 2)"
        [tok (T.Identifier "f"), tok T.LParen, tok (T.LitInt 1), tok T.Comma, tok (T.LitInt 2), tok T.RParen]
        (ExprCall "f" [ExprLitInt 1, ExprLitInt 2])
  
  , testCase "Method Call (x.f())" $
      -- x.f() parses as (x.f)() -> ExprCall "f" [x]
      -- Wait, parseCallPostfix implementation:
      -- pure $ \e -> case e of
      --   ExprAccess target field -> ExprCall field (target : args)
      assertParse "x.f()"
        [tok (T.Identifier "x"), tok T.Dot, tok (T.Identifier "f"), tok T.LParen, tok T.RParen]
        (ExprCall "f" [ExprVar "x"])

  , testCase "Postfix Inc" $
      assertParse "x++" [tok (T.Identifier "x"), tok T.OpInc] (ExprUnary PostfixInc (ExprVar "x"))
  ]

structInitTests :: TestTree
structInitTests = testGroup "Struct Init Tests"
  [ testCase "Empty Struct" $
      assertParse "S {}"
        [tok (T.Identifier "S"), tok T.LBrace, tok T.RBrace]
        (ExprStructInit "S" [])
  
  , testCase "Struct with fields" $
      assertParse "S { x: 1 }"
        [tok (T.Identifier "S"), tok T.LBrace, tok (T.Identifier "x"), tok T.Colon, tok (T.LitInt 1), tok T.RBrace]
        (ExprStructInit "S" [("x", ExprLitInt 1)])
  ]

parenthesesTests :: TestTree
parenthesesTests = testGroup "Parentheses Tests"
  [ testCase "(1 + 2)" $
      assertParse "(1 + 2)"
        [tok T.LParen, tok (T.LitInt 1), tok T.OpPlus, tok (T.LitInt 2), tok T.RParen]
        (ExprBinary Add (ExprLitInt 1) (ExprLitInt 2))
  ]
