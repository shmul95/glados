module AST.TypesSpecs (astTypesTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Rune.AST.Nodes
import Rune.AST.Types (Parser(..), ParserState(..))
import qualified Rune.Lexer.Tokens as T
import Control.Applicative (empty, (<|>))


--
-- public
--

astTypesTests :: TestTree
astTypesTests =
  testGroup
    "AST Types Tests"
    [ typeTests
    , binaryOpTests
    , unaryOpTests
    , parameterTests
    , fieldTests
    , programTests
    , topLevelDefTests
    , statementTests
    , expressionTests
    , parserStateTests
    , parserInstanceTests
    ]

--
-- private
--

typeTests :: TestTree
typeTests = testGroup "Type Tests"
  [ testCase "Show for all primitive types" $ do
      assertEqual "i8" "i8" (show TypeI8)
      assertEqual "i16" "i16" (show TypeI16)
      assertEqual "i32" "i32" (show TypeI32)
      assertEqual "i64" "i64" (show TypeI64)
      assertEqual "f32" "f32" (show TypeF32)
      assertEqual "f64" "f64" (show TypeF64)
      assertEqual "bool" "bool" (show TypeBool)
      assertEqual "u8" "u8" (show TypeU8)
      assertEqual "u16" "u16" (show TypeU16)
      assertEqual "u32" "u32" (show TypeU32)
      assertEqual "u64" "u64" (show TypeU64)
      assertEqual "char" "char" (show TypeChar)
      assertEqual "str" "str" (show TypeString)
      assertEqual "any" "any" (show TypeAny)
      assertEqual "null" "null" (show TypeNull)
      assertEqual "TypeCustom" "Vec2f" (show (TypeCustom "Vec2f"))
  , testCase "Eq for Type" $ do
      assertBool "Equal primitive types" (TypeI32 == TypeI32)
      assertBool "Unequal primitive types" (TypeI32 /= TypeF32)
      assertBool "Equal custom types" (TypeCustom "A" == TypeCustom "A")
      assertBool "Unequal custom types" (TypeCustom "A" /= TypeCustom "B")
      assertBool "Primitive vs custom" (TypeI32 /= TypeCustom "I32")
  ]

binaryOpTests :: TestTree
binaryOpTests = testGroup "BinaryOp Tests"
  [ testCase "Show for all BinaryOps" $ do
      assertEqual "Show Add" "Add" (show Add)
      assertEqual "Show Sub" "Sub" (show Sub)
      assertEqual "Show Mul" "Mul" (show Mul)
      assertEqual "Show Div" "Div" (show Div)
      assertEqual "Show Mod" "Mod" (show Mod)
      assertEqual "Show Eq" "Eq" (show Eq)
      assertEqual "Show Neq" "Neq" (show Neq)
      assertEqual "Show Lt" "Lt" (show Lt)
      assertEqual "Show Lte" "Lte" (show Lte)
      assertEqual "Show Gt" "Gt" (show Gt)
      assertEqual "Show Gte" "Gte" (show Gte)
      assertEqual "Show And" "And" (show And)
      assertEqual "Show Or" "Or" (show Or)
  , testCase "Eq for BinaryOp" $ do
      assertBool "Equal" (Add == Add)
      assertBool "Unequal" (Add /= Sub)
  ]

unaryOpTests :: TestTree
unaryOpTests = testGroup "UnaryOp Tests"
  [ testCase "Show for all UnaryOps" $ do
      assertEqual "Show Negate" "Negate" (show Negate)
      assertEqual "Show PropagateError" "PropagateError" (show PropagateError)
      assertEqual "Show PrefixInc" "PrefixInc" (show PrefixInc)
      assertEqual "Show PrefixDec" "PrefixDec" (show PrefixDec)
      assertEqual "Show PostfixInc" "PostfixInc" (show PostfixInc)
      assertEqual "Show PostfixDec" "PostfixDec" (show PostfixDec)
  , testCase "Eq for UnaryOp" $ do
      assertBool "Equal" (Negate == Negate)
      assertBool "Unequal" (Negate /= PrefixInc)
  ]

parameterTests :: TestTree
parameterTests = testGroup "Parameter Tests"
  [ testCase "Show Parameter" $
      assertEqual "Show" "Parameter {paramName = \"x\", paramType = i32}" (show (Parameter "x" TypeI32))
  , testCase "Eq Parameter" $ do
      let p1 = Parameter "x" TypeI32
      let p2 = Parameter "x" TypeI32
      let p3 = Parameter "y" TypeI32
      let p4 = Parameter "x" TypeF32
      assertBool "Equal" (p1 == p2)
      assertBool "Unequal name" (p1 /= p3)
      assertBool "Unequal type" (p1 /= p4)
  ]

fieldTests :: TestTree
fieldTests = testGroup "Field Tests"
  [ testCase "Show Field" $
      assertEqual "Show" "Field {fieldName = \"x\", fieldType = f32}" (show (Field "x" TypeF32))
  , testCase "Eq Field" $ do
      let f1 = Field "x" TypeF32
      let f2 = Field "x" TypeF32
      let f3 = Field "y" TypeF32
      let f4 = Field "x" TypeI32
      assertBool "Equal" (f1 == f2)
      assertBool "Unequal name" (f1 /= f3)
      assertBool "Unequal type" (f1 /= f4)
  ]

programTests :: TestTree
programTests = testGroup "Program Tests"
  [ testCase "Show Program" $
      assertEqual "Show" "Program {programName = \"main.rune\", programDefs = []}" (show (Program "main.rune" []))
  , testCase "Eq Program" $ do
      let p1 = Program "a" []
      let p2 = Program "a" []
      let p3 = Program "b" []
      assertBool "Equal" (p1 == p2)
      assertBool "Unequal" (p1 /= p3)
  ]

topLevelDefTests :: TestTree
topLevelDefTests = testGroup "TopLevelDef Tests"
  [ testCase "Show DefFunction" $
      assertBool "Show" (not (null (show defFunc)))
  , testCase "Show DefStruct" $
      assertBool "Show" (not (null (show defStruct)))
  , testCase "Show DefOverride" $
      assertBool "Show" (not (null (show defOverride)))
  , testCase "Eq DefFunction" $ do
      assertBool "Equal" (defFunc == defFunc)
      assertBool "Unequal name" (defFunc /= defFunc {funcName = "bar"})
  , testCase "Eq DefStruct" $ do
      assertBool "Equal" (defStruct == defStruct)
      assertBool "Unequal name" (defStruct /= defStruct {structName = "Vec3f"})
  , testCase "Eq DefOverride" $ do
      assertBool "Equal" (defOverride == defOverride)
      assertBool "Unequal name" (defOverride /= defOverride {overrideName = "display"})
      assertBool "Unequal body" (defOverride /= defOverride {overrideBody = blockB})
  , testCase "Eq Different Constructors" $ do
      assertBool "DefFunction /= DefStruct" (defFunc /= defStruct)
      assertBool "DefStruct /= DefOverride" (defStruct /= defOverride)
  ]
  where
    param = Parameter "x" TypeI32
    field = Field "x" TypeI32
    blockA = [StmtStop]
    blockB = [StmtNext]
    defFunc = DefFunction "foo" [param] TypeI32 blockA
    defStruct = DefStruct "Vec2f" [field] [defFunc]
    defOverride = DefOverride "show" [param] TypeNull blockA

statementTests :: TestTree
statementTests = testGroup "Statement Tests"
  [ testCase "Show StmtVarDecl" $
      assertBool "Show" (not (null (show (StmtVarDecl "x" (Just TypeI32) exprVar))))
  , testCase "Show StmtAssignment" $
      assertBool "Show" (not (null (show (StmtAssignment exprVar exprLitInt))))
  , testCase "Show StmtReturn (Just)" $
      assertBool "Show" (not (null (show (StmtReturn (Just exprLitInt)))))
  , testCase "Show StmtReturn (Nothing)" $
      assertBool "Show" (not (null (show (StmtReturn Nothing))))
  , testCase "Show StmtIf (Just else)" $
      assertBool "Show" (not (null (show (StmtIf exprLitBool block block'))))
  , testCase "Show StmtIf (Nothing else)" $
      assertBool "Show" (not (null (show (StmtIf exprLitBool block Nothing))))
  , testCase "Show StmtFor (Full)" $
      assertBool "Show" (not (null (show (StmtFor "i" (Just TypeI32) (Just exprLitInt) exprLitInt block))))
  , testCase "Show StmtFor (No type/start)" $
      assertBool "Show" (not (null (show (StmtFor "i" Nothing Nothing exprLitInt block))))
  , testCase "Show StmtForEach" $
      assertBool "Show" (not (null (show (StmtForEach "item" Nothing exprVar block))))
  , testCase "Show StmtLoop" $
      assertBool "Show" (not (null (show (StmtLoop block))))
  , testCase "Show StmtStop" $
      assertBool "Show" (not (null (show StmtStop)))
  , testCase "Show StmtNext" $
      assertBool "Show" (not (null (show StmtNext)))
  , testCase "Show StmtExpr" $
      assertBool "Show" (not (null (show (StmtExpr exprLitInt))))
  , testCase "Eq StmtVarDecl" $
      assertBool "Unequal name" (StmtVarDecl "x" Nothing exprLitInt /= StmtVarDecl "y" Nothing exprLitInt)
  , testCase "Eq StmtIf" $
      assertBool "Unequal else" (StmtIf exprLitBool block block' /= StmtIf exprLitBool block Nothing)
  , testCase "Eq StmtLoop" $
      assertBool "Equal" (StmtLoop block == StmtLoop block)
  , testCase "Eq StmtStop/StmtNext" $
      assertBool "Unequal constructors" (StmtStop /= StmtNext)
  ]
  where
    exprLitInt = ExprLitInt 1
    exprLitBool = ExprLitBool True
    exprVar = ExprVar "list"
    block = [StmtStop]
    block' = Just [StmtNext]

expressionTests :: TestTree
expressionTests = testGroup "Expression Tests"
  [ testCase "Show ExprBinary" $
      assertBool "Show" (not (null (show (ExprBinary Add exprLitInt exprLitInt))))
  , testCase "Show ExprUnary" $
      assertBool "Show" (not (null (show (ExprUnary Negate exprLitInt))))
  , testCase "Show ExprCall" $
      assertBool "Show" (not (null (show (ExprCall "foo" [exprLitInt]))))
  , testCase "Show ExprStructInit" $
      assertBool "Show" (not (null (show (ExprStructInit "Vec2f" [("x", exprLitInt)]))))
  , testCase "Show ExprAccess" $
      assertBool "Show" (not (null (show (ExprAccess exprVar "field"))))
  , testCase "Show ExprLitInt" $
      assertBool "Show" (not (null (show exprLitInt)))
  , testCase "Show ExprLitFloat" $
      assertBool "Show" (not (null (show (ExprLitFloat 3.14))))
  , testCase "Show ExprLitString" $
      assertBool "Show" (not (null (show (ExprLitString "hi"))))
  , testCase "Show ExprLitChar" $
      assertBool "Show" (not (null (show (ExprLitChar 'a'))))
  , testCase "Show ExprLitBool" $
      assertBool "Show" (not (null (show (ExprLitBool False))))
  , testCase "Show ExprLitNull" $
      assertBool "Show" (not (null (show ExprLitNull)))
  , testCase "Show ExprVar" $
      assertBool "Show" (not (null (show exprVar)))
  , testCase "Eq ExprBinary" $
      assertBool "Unequal op" (ExprBinary Add exprLitInt exprLitInt /= ExprBinary Sub exprLitInt exprLitInt)
  , testCase "Eq ExprUnary" $
      assertBool "Unequal op" (ExprUnary Negate exprLitInt /= ExprUnary PropagateError exprLitInt)
  , testCase "Eq ExprCall" $
      assertBool "Unequal name" (ExprCall "foo" [] /= ExprCall "bar" [])
  , testCase "Eq ExprStructInit" $
      assertBool "Unequal fields" (ExprStructInit "V" [("x", exprLitInt)] /= ExprStructInit "V" [("y", exprLitInt)])
  , testCase "Eq ExprAccess" $
      assertBool "Unequal target" (ExprAccess exprVar "f" /= ExprAccess (ExprLitInt 0) "f")
  , testCase "Eq ExprVar" $
      assertBool "Unequal var" (ExprVar "a" /= ExprVar "b")
  , testCase "Eq ExprLitNull" $
      assertBool "Equal" (ExprLitNull == ExprLitNull)
  , testCase "Eq Different Constructors" $
      assertBool "ExprLitInt /= ExprLitFloat" (exprLitInt /= ExprLitFloat 1.0)
  ]
  where
    exprLitInt = ExprLitInt 1
    exprVar = ExprVar "v"

parserStateTests :: TestTree
parserStateTests = testGroup "ParserState Tests"
  [ testCase "Show ParserState" $
      assertBool "Show" (not (null (show testParserState)))
  , testCase "ParserState Accessors" $ do
      let t = T.Token (T.Identifier "x") "x" 1 1
      assertEqual "psTokens" [t] (psTokens testParserState)
      assertEqual "psPosition" 0 (psPosition testParserState)
      assertEqual "psFilePath" "file.ru" (psFilePath testParserState)
      assertEqual "psLoopDepth" 42 (psLoopDepth testParserState)
  ]
  where
    testParserState = ParserState
      { psTokens = [T.Token (T.Identifier "x") "x" 1 1]
      , psPosition = 0
      , psFilePath = "file.ru"
      , psLoopDepth = 42
      }

assertRunResult :: (Eq a, Show a) => String -> Either String (a, ParserState) -> Either String a -> Int -> IO ()
assertRunResult msg actual expectedValue expectedPosition =
  case actual of
    Left actualErr -> case expectedValue of
      Left expectedErr -> assertEqual (msg ++ ": Error message content check (simple)") (show expectedErr) (show actualErr)
      Right _ -> assertFailure $ msg ++ ": Expected success but got error: " ++ actualErr
    Right (actualValue, actualState) -> case expectedValue of
      Left expectedErr -> assertFailure $ msg ++ ": Expected error: " ++ expectedErr ++ " but got success: " ++ show actualValue
      Right expectedV -> do
        assertEqual (msg ++ ": Value check") expectedV actualValue
        assertEqual (msg ++ ": Position check") expectedPosition (psPosition actualState)

parserInstanceTests :: TestTree
parserInstanceTests = testGroup "Parser Instance Tests"
  [ testCase "Functor fmap Left" $
      assertRunResult "Functor fmap Left" (runParser (fmap (+1) failParser) initialState) (Left "Error") 0
  , testCase "Functor fmap Right" $
      assertRunResult "Functor fmap Right" (runParser (fmap (+1) successParser) initialState) (Right (42 + (1 :: Int))) 0
  , testCase "Applicative pure" $
      assertRunResult "Applicative pure" (runParser (pure (42 :: Int)) initialState) (Right (42 :: Int)) 0
  , testCase "Applicative <*> Left (pf)" $
      assertRunResult "Applicative <*> Left (pf)" (runParser (failFuncParser <*> successParser) initialState) (Left "Error") 0
  , testCase "Applicative <*> Left (px)" $
      assertRunResult "Applicative <*> Left (px)" (runParser (successFuncParser <*> failParser) initialState) (Left "Error") 0
  , testCase "Applicative <*> Right" $
      assertRunResult "Applicative <*> Right" (runParser (successFuncParser <*> successParser) initialState) (Right (42 + (1 :: Int))) 0
  , testCase "Monad >>= Left" $
      assertRunResult "Monad >>= Left" (runParser (failParser >>= (\_ -> successParser)) initialState) (Left "Error") 0
  , testCase "Monad >>= Right (p success, f fail)" $
      assertRunResult "Monad >>= Right (p success, f fail)" (runParser (successParser >>= (\_ -> failParser')) initialState) (Left "Fail from f") 0
  , testCase "Monad >>= Right (p success, f success)" $
      assertRunResult "Monad >>= Right (p success, f success)" (runParser (successParser >>= (\v -> Parser (\s -> Right (v + 2, s {psPosition = 2})))) initialState) (Right (42 + (2 :: Int))) 2
  , testCase "Alternative empty" $
      assertRunResult "Alternative empty" (runParser (empty :: Parser ()) initialState) (Left "Parser empty\n") 0
  , testCase "Alternative <|> Right (p1)" $
      assertRunResult "Alternative <|> Right (p1)" (runParser (successParser <|> failParser) initialState) (Right (42 :: Int)) 0
  , testCase "Alternative <|> Right (p2)" $
      assertRunResult "Alternative <|> Right (p2)" (runParser (failParser <|> successParser') initialState) (Right (43 :: Int)) 1
  , testCase "Alternative <|> Left (p1 fail, p2 fail)" $
      assertRunResult "Alternative <|> Left (p1 fail, p2 fail)" (runParser (failParser <|> failParser) initialState) (Left "Error") 0
  ]
  where
    initialState = ParserState [] 0 "test" 0

    failParser :: Parser Int
    failParser = Parser (\_ -> Left "Error")

    failParser' :: Parser Int
    failParser' = Parser (\_ -> Left "Fail from f")

    successParser :: Parser Int
    successParser = Parser (\s -> Right (42, s))

    successParser' :: Parser Int
    successParser' = Parser (\s -> Right (43, s {psPosition = 1}))

    successFuncParser :: Parser (Int -> Int)
    successFuncParser = Parser (\s -> Right ((+1), s))

    failFuncParser :: Parser (Int -> Int)
    failFuncParser = Parser (\_ -> Left "Error")
