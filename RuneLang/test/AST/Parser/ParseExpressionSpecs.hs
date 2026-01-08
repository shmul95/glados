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
    , arrayLiteralTests
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
  [ testCase "Int" $ assertParse "Int" [tok (T.LitInt 1)] (ExprLitInt (SourcePos "test" 1 1) 1)
  , testCase "Float" $ assertParse "Float" [tok (T.LitFloat 1.5)] (ExprLitFloat (SourcePos "test" 1 1) 1.5)
  , testCase "String" $ assertParse "String" [tok (T.LitString "s")] (ExprLitString (SourcePos "test" 1 1) "s")
  , testCase "Char" $ assertParse "Char" [tok (T.LitChar 'c')] (ExprLitChar (SourcePos "test" 1 1) 'c')
  , testCase "Bool True" $ assertParse "Bool" [tok (T.LitBool True)] (ExprLitBool (SourcePos "test" 1 1) True)
  , testCase "Null" $ assertParse "Null" [tok T.LitNull] (ExprLitNull (SourcePos "test" 1 1))
  ]

variableTests :: TestTree
variableTests = testGroup "Variable Tests"
  [ testCase "Identifier" $ assertParse "Var" [tok (T.Identifier "x")] (ExprVar (SourcePos "test" 1 1) "x")
  ]

binaryOpTests :: TestTree
binaryOpTests = testGroup "Binary Op Tests"
  [ testCase "Add" $
      assertParse "1 + 2" 
        [tok (T.LitInt 1), tok T.OpPlus, tok (T.LitInt 2)]
        (ExprBinary (SourcePos "test" 1 1) Add (ExprLitInt (SourcePos "test" 1 1) 1) (ExprLitInt (SourcePos "test" 1 1) 2))
  
  , testCase "Precedence Mul > Add" $
      assertParse "1 + 2 * 3"
        [tok (T.LitInt 1), tok T.OpPlus, tok (T.LitInt 2), tok T.OpMul, tok (T.LitInt 3)]
        (ExprBinary (SourcePos "test" 1 1) Add (ExprLitInt (SourcePos "test" 1 1) 1) (ExprBinary (SourcePos "test" 1 1) Mul (ExprLitInt (SourcePos "test" 1 1) 2) (ExprLitInt (SourcePos "test" 1 1) 3)))

  , testCase "Precedence Mul > Add (reverse)" $
      assertParse "1 * 2 + 3"
        [tok (T.LitInt 1), tok T.OpMul, tok (T.LitInt 2), tok T.OpPlus, tok (T.LitInt 3)]
        (ExprBinary (SourcePos "test" 1 1) Add (ExprBinary (SourcePos "test" 1 1) Mul (ExprLitInt (SourcePos "test" 1 1) 1) (ExprLitInt (SourcePos "test" 1 1) 2)) (ExprLitInt (SourcePos "test" 1 1) 3))

  , testCase "Logical Or < And" $
      assertParse "true || false && true"
        [tok (T.LitBool True), tok T.OpOr, tok (T.LitBool False), tok T.OpAnd, tok (T.LitBool True)]
        (ExprBinary (SourcePos "test" 1 1) Or (ExprLitBool (SourcePos "test" 1 1) True) (ExprBinary (SourcePos "test" 1 1) And (ExprLitBool (SourcePos "test" 1 1) False) (ExprLitBool (SourcePos "test" 1 1) True)))
  ]

unaryOpTests :: TestTree
unaryOpTests = testGroup "Unary Op Tests"
  [ testCase "Negate" $
      assertParse "-1" [tok T.OpMinus, tok (T.LitInt 1)] (ExprUnary (SourcePos "test" 1 1) Negate (ExprLitInt (SourcePos "test" 1 1) 1))
  , testCase "Not" $
      assertParse "!x" [tok T.OpNot, tok (T.Identifier "x")] (ExprUnary (SourcePos "test" 1 1) Not (ExprVar (SourcePos "test" 1 1) "x"))
  , testCase "PrefixInc" $
      assertParse "++x" [tok T.OpInc, tok (T.Identifier "x")] (ExprUnary (SourcePos "test" 1 1) PrefixInc (ExprVar (SourcePos "test" 1 1) "x"))
  , testCase "ErrorPropagate" $
      assertParse "x?" [tok (T.Identifier "x"), tok T.OpErrorProp] (ExprUnary (SourcePos "test" 1 1) PropagateError (ExprVar (SourcePos "test" 1 1) "x"))
  ]

postfixTests :: TestTree
postfixTests = testGroup "Postfix Tests"
  [ testCase "Field Access" $
      assertParse "x.y" 
        [tok (T.Identifier "x"), tok T.Dot, tok (T.Identifier "y")]
        (ExprAccess (SourcePos "test" 1 1) (ExprVar (SourcePos "test" 1 1) "x") "y")
  
  , testCase "Call" $
      assertParse "f()"
        [tok (T.Identifier "f"), tok T.LParen, tok T.RParen]
        (ExprCall (SourcePos "test" 1 1) "f" [])

  , testCase "Call with args" $
      assertParse "f(1, 2)"
        [tok (T.Identifier "f"), tok T.LParen, tok (T.LitInt 1), tok T.Comma, tok (T.LitInt 2), tok T.RParen]
        (ExprCall (SourcePos "test" 1 1) "f" [ExprLitInt (SourcePos "test" 1 1) 1, ExprLitInt (SourcePos "test" 1 1) 2])
  
  , testCase "Method Call (x.f())" $
      assertParse "x.f()"
        [tok (T.Identifier "x"), tok T.Dot, tok (T.Identifier "f"), tok T.LParen, tok T.RParen]
        (ExprCall (SourcePos "test" 1 1) "f" [ExprVar (SourcePos "test" 1 1) "x"])

  , testCase "Postfix Inc" $
      assertParse "x++" [tok (T.Identifier "x"), tok T.OpInc] (ExprUnary (SourcePos "test" 1 1) PostfixInc (ExprVar (SourcePos "test" 1 1) "x"))

  , testCase "Array Index" $
      assertParse "arr[1]"
        [tok (T.Identifier "arr"), tok T.LBracket, tok (T.LitInt 1), tok T.RBracket]
        (ExprIndex (SourcePos "test" 1 1) (ExprVar (SourcePos "test" 1 1) "arr") (ExprLitInt (SourcePos "test" 1 1) 1))

  , testCase "Array Index Field Access" $
      assertParse "arr[1].x"
        [tok (T.Identifier "arr"), tok T.LBracket, tok (T.LitInt 1), tok T.RBracket, tok T.Dot, tok (T.Identifier "x")]
        (ExprAccess (SourcePos "test" 1 1) (ExprIndex (SourcePos "test" 1 1) (ExprVar (SourcePos "test" 1 1) "arr") (ExprLitInt (SourcePos "test" 1 1) 1)) "x")

  , testCase "Array Index Chained" $
      assertParse "arr[x][y]"
        [tok (T.Identifier "arr"), tok T.LBracket, tok (T.Identifier "x"), tok T.RBracket, tok T.LBracket, tok (T.Identifier "y"), tok T.RBracket]
        (ExprIndex (SourcePos "test" 1 1) (ExprIndex (SourcePos "test" 1 1) (ExprVar (SourcePos "test" 1 1) "arr") (ExprVar (SourcePos "test" 1 1) "x")) (ExprVar (SourcePos "test" 1 1) "y"))

  , testCase "Call with error propagation" $
      assertParse "f()?"
        [tok (T.Identifier "f"), tok T.LParen, tok T.RParen, tok T.OpErrorProp]
        (ExprUnary (SourcePos "test" 1 1) PropagateError (ExprCall (SourcePos "test" 1 1) "f" []))
  ]

structInitTests :: TestTree
structInitTests = testGroup "Struct Init Tests"
  [ testCase "Empty Struct" $
      assertParse "S {}"
        [tok (T.Identifier "S"), tok T.LBrace, tok T.RBrace]
        (ExprStructInit (SourcePos "test" 1 1) "S" [])
  
  , testCase "Struct with fields" $
      assertParse "S { x: 1 }"
        [tok (T.Identifier "S"), tok T.LBrace, tok (T.Identifier "x"), tok T.Colon, tok (T.LitInt 1), tok T.RBrace]
        (ExprStructInit (SourcePos "test" 1 1) "S" [("x", ExprLitInt (SourcePos "test" 1 1) 1)])
  ]

parenthesesTests :: TestTree
parenthesesTests = testGroup "Parentheses Tests"
  [ testCase "(1 + 2)" $
      assertParse "(1 + 2)"
        [tok T.LParen, tok (T.LitInt 1), tok T.OpPlus, tok (T.LitInt 2), tok T.RParen]
        (ExprBinary (SourcePos "test" 1 1) Add (ExprLitInt (SourcePos "test" 1 1) 1) (ExprLitInt (SourcePos "test" 1 1) 2))
  ]

arrayLiteralTests :: TestTree
arrayLiteralTests = testGroup "Array Literal Tests"
  [ testCase "Empty array []" $
      assertParse "[]"
        [tok T.LBracket, tok T.RBracket]
        (ExprLitArray (SourcePos "test" 1 1) [])
  
  , testCase "Array with single int [1]" $
      assertParse "[1]"
        [tok T.LBracket, tok (T.LitInt 1), tok T.RBracket]
        (ExprLitArray (SourcePos "test" 1 1) [ExprLitInt (SourcePos "test" 1 1) 1])
  
  , testCase "Array with multiple ints [1, 2, 3]" $
      assertParse "[1, 2, 3]"
        [tok T.LBracket, tok (T.LitInt 1), tok T.Comma, tok (T.LitInt 2), tok T.Comma, tok (T.LitInt 3), tok T.RBracket]
        (ExprLitArray (SourcePos "test" 1 1) [ExprLitInt (SourcePos "test" 1 1) 1, ExprLitInt (SourcePos "test" 1 1) 2, ExprLitInt (SourcePos "test" 1 1) 3])
  
  , testCase "Array with strings" $
      assertParse "[\"hello\", \"world\"]"
        [tok T.LBracket, tok (T.LitString "hello"), tok T.Comma, tok (T.LitString "world"), tok T.RBracket]
        (ExprLitArray (SourcePos "test" 1 1) [ExprLitString (SourcePos "test" 1 1) "hello", ExprLitString (SourcePos "test" 1 1) "world"])
  
  , testCase "Array with chars ['a', 'b', 'c']" $
      assertParse "['a', 'b', 'c']"
        [tok T.LBracket, tok (T.LitChar 'a'), tok T.Comma, tok (T.LitChar 'b'), tok T.Comma, tok (T.LitChar 'c'), tok T.RBracket]
        (ExprLitArray (SourcePos "test" 1 1) [ExprLitChar (SourcePos "test" 1 1) 'a', ExprLitChar (SourcePos "test" 1 1) 'b', ExprLitChar (SourcePos "test" 1 1) 'c'])
  
  , testCase "Array with expressions [1 + 2, 3 * 4]" $
      assertParse "[1 + 2, 3 * 4]"
        [tok T.LBracket, tok (T.LitInt 1), tok T.OpPlus, tok (T.LitInt 2), tok T.Comma, tok (T.LitInt 3), tok T.OpMul, tok (T.LitInt 4), tok T.RBracket]
        (ExprLitArray (SourcePos "test" 1 1) [ExprBinary (SourcePos "test" 1 1) Add (ExprLitInt (SourcePos "test" 1 1) 1) (ExprLitInt (SourcePos "test" 1 1) 2), ExprBinary (SourcePos "test" 1 1) Mul (ExprLitInt (SourcePos "test" 1 1) 3) (ExprLitInt (SourcePos "test" 1 1) 4)])
  ]
