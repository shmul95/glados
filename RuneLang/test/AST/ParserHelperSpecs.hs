module AST.ParserHelperSpecs (parserHelperTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure, (@?))
import Rune.AST.ParserHelper
import Rune.AST.Types (Parser(..), ParserState(..))
import qualified Rune.Lexer.Tokens as T
import Control.Applicative (Alternative (..))
import qualified Data.List as List
import Data.Either (isLeft)

--
-- public
--

parserHelperTests :: TestTree
parserHelperTests =
    testGroup
        "Rune.AST.ParserHelper Tests"
        [ test_withContext
        , test_failParse
        , test_try
        , test_getParserState
        , test_incLoopDepth
        , test_checkLoopDepth
        , test_tokenMap
        , test_peek
        , test_advance
        , test_expect
        , test_expectIdent
        , test_check
        , test_match
        , test_choice
        , test_between
        , test_sepBy
        , test_sepBy1
        , test_sepEndBy
        , test_chainl1
        , test_chainUnary
        , test_chainPostfix
        ]

--
-- private
--

type Result a = Either String (a, ParserState)

tok :: T.TokenKind -> String -> Int -> Int -> T.Token
tok k v l c = T.Token k v l c

initialState :: [T.Token] -> Int -> ParserState
initialState ts depth =
    ParserState
        { psTokens = ts ++ [tok T.EOF "" 99 99],
          psPosition = 0,
          psFilePath = "test.rune",
          psLoopDepth = depth
        }

failP :: String -> Parser Int
failP msg = Parser $ \_ -> Left msg

successP :: Int -> Int -> Parser Int
successP val posChange = Parser $ \s -> Right (val, s {psPosition = psPosition s + posChange})

tId :: T.Token
tId = tok (T.Identifier "var") "var" 1 1
tInt :: T.Token
tInt = tok (T.LitInt 1) "1" 1 5
tComma :: T.Token
tComma = tok T.Comma "," 1 6
tLParen :: T.Token
tLParen = tok T.LParen "(" 1 7
tRParen :: T.Token
tRParen = tok T.RParen ")" 1 8
tPlus :: T.Token
tPlus = tok T.OpPlus "+" 1 9
tMinus :: T.Token
tMinus = tok T.OpMinus "-" 1 10

assertIsErrorWithMsg :: String -> Result a -> IO ()
assertIsErrorWithMsg expectedMsg result =
    case result of
        Left actualMsg -> assertEqual "Error message check" expectedMsg actualMsg
        Right _ -> assertFailure $ "Expected failure with: " ++ expectedMsg ++ ", but got success."

test_withContext :: TestTree
test_withContext = testGroup "withContext"
    [ testCase "Success: result passes through" $
        let s = initialState [] 0
            result = runParser (withContext "ctx" (successP 42 0)) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value" 42 v
                assertEqual "State position" 0 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "Failure: context is added to error message" $
        let s = initialState [] 0
            msg = "Expected token"
            p = failParse msg
            result = runParser (withContext "function call" p) s
        in case result of
            Left err' -> assertBool "Error should contain context" ("  ... in function call\n" `List.isInfixOf` err')
            Right _ -> assertFailure $ "Expected failure, got success"
    ]

test_failParse :: TestTree
test_failParse = testGroup "failParse"
    [ testCase "Normal token fails with full error message" $
        let tokens = [tId]
            s = initialState tokens 0
            expectedMsg = "Expected keyword"
            expectedTok = "Identifier \"var\""
            result = runParser (failParse expectedMsg) s
        in case result of
            Left err -> do
                assertBool "Error contains file/line/col" ("test.rune:1:1:" `List.isInfixOf` err)
                assertBool "Error contains custom message" (expectedMsg `List.isInfixOf` err)
                assertBool "Error contains actual token" (expectedTok `List.isInfixOf` err)
            Right _ -> assertFailure "Expected failure"

    , testCase "EOF token fails with '<EOF>' message" $
        let s = initialState [] 0
            expectedMsg = "End of file unexpectedly"
            result = runParser (failParse expectedMsg) (s {psPosition = length (psTokens s) - 1})
        in case result of
            Left err -> do
                assertBool "Error contains EOF line/col" ("99:99:" `List.isInfixOf` err)
                assertBool "Error contains custom message" (expectedMsg `List.isInfixOf` err)
                assertBool "Error contains <EOF>" ("Got: <EOF>" `List.isInfixOf` err)
            Right _ -> assertFailure "Expected failure"
    ]

test_try :: TestTree
test_try = testGroup "try"
    [ testCase "Success: result passes through" $
        let tokens = [tId, tInt]
            s = initialState tokens 0
            p = expect (T.Identifier "var")
            result = runParser (try p) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value" tId v
                assertEqual "Position" 1 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "Failure (failParse): returns Left \"\" (backtracks state)" $
        let tokens = [tInt, tId]
            s = initialState tokens 0
            p = expect (T.Identifier "var")
            result = runParser (try p) s
        in case result of
             Left "" -> pure ()
             Left err -> assertFailure $ "Expected Left \"\" for backtrack, got: Left \"" ++ err ++ "\""
             Right _ -> assertFailure "Expected failure, got success"
    ]

test_getParserState :: TestTree
test_getParserState = testCase "getParserState returns current state" $
    let s = initialState [tId] 5
        result = runParser getParserState s
    in case result of
        Right (s', s'') -> do
            assertEqual "Returned state" (psFilePath s) (psFilePath s')
            assertEqual "Final state" (psFilePath s) (psFilePath s'')
        Left err -> assertFailure $ "Expected success, got error: " ++ err

test_incLoopDepth :: TestTree
test_incLoopDepth = testGroup "incLoopDepth"
    [ testCase "Success: loop depth increments and restores" $
        let s = initialState [] 5
            p = Parser $ \s' -> Right (psLoopDepth s', s' {psPosition = 1})
            result = runParser (incLoopDepth p) s
        in case result of
            Right (innerDepth, s') -> do
                assertEqual "Inner value is incremented depth" 6 innerDepth
                assertEqual "Final loop depth should be restored" 5 (psLoopDepth s')
                assertEqual "Position change" 1 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "Failure: inner parser failure propagates error (check error string)" $
        let s = initialState [] 5
            p = failP "IncDepthError"
            result = runParser (incLoopDepth p) s
        in case result of
             Left err -> assertEqual "Error content check" "IncDepthError" err
             Right _ -> assertFailure "Expected failure"
    ]

test_checkLoopDepth :: TestTree
test_checkLoopDepth = testGroup "checkLoopDepth"
    [ testCase "Depth > 0 returns True" $
        let s = initialState [] 1
            result = runParser checkLoopDepth s
        in case result of
            Right (v, _) -> assertBool "Should be True" v
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "Depth == 0 returns False" $
        let s = initialState [] 0
            result = runParser checkLoopDepth s
        in case result of
            Right (v, _) -> assertBool "Should be False" (not v)
            Left err -> assertFailure $ "Expected success, got error: " ++ err
    ]

test_tokenMap :: TestTree
test_tokenMap = testGroup "tokenMap"
    [ testCase "Match: returns mapped value and advances" $
        let s = initialState [tInt] 0
            mapper (T.LitInt n) = Just (n + 1)
            mapper _ = Nothing
            result = runParser (tokenMap mapper) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value" 2 v
                assertEqual "Position" 1 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "No Match: calls empty and does not advance" $
        let s = initialState [tId] 0
            mapper (T.LitInt _) = Just (1 :: Int)
            mapper _ = Nothing
            result = runParser (tokenMap mapper) s
        in assertIsErrorWithMsg "Parser empty\n" result
    ]

test_peek :: TestTree
test_peek = testCase "peek returns current token (tId) and no state change" $
    let s = initialState [tId, tInt] 0
        result = runParser peek s
    in case result of
        Right (v, s') -> do
            assertEqual "Token value" tId v
            assertEqual "Position" 0 (psPosition s')
        Left err -> assertFailure $ "Expected success, got error: " ++ err

test_advance :: TestTree
test_advance = testCase "advance increments position by 1" $
    let s = initialState [tId] 0
        result = runParser advance s
    in case result of
        Right ((), s') -> assertEqual "Position" 1 (psPosition s')
        Left err -> assertFailure $ "Expected success, got error: " ++ err

test_expect :: TestTree
test_expect = testGroup "expect"
    [ testCase "Match: returns token and advances (Identifier)" $
        let s = initialState [tId] 0
            result = runParser (expect (T.Identifier "var")) s
        in case result of
            Right (v, s') -> do
                assertEqual "Token value" tId v
                assertEqual "Position" 1 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "No Match: calls failParse and does not advance (LitInt vs Identifier)" $
        let s = initialState [tInt] 0
            result = runParser (expect (T.Identifier "var")) s
        in isLeft result @? "Expected failure"
    ]

test_expectIdent :: TestTree
test_expectIdent = testGroup "expectIdent"
    [ testCase "Match: advances" $
        let s = initialState [tId] 0
            result = runParser (expectIdent "var") s
        in case result of
            Right ((), s') -> assertEqual "Position" 1 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "No Match (wrong name): calls failParse" $
        let s = initialState [tId] 0
            result = runParser (expectIdent "other") s
        in isLeft result @? "Expected failure"

    , testCase "No Match (wrong kind - LitInt): calls failParse" $
        let s = initialState [tInt] 0
            result = runParser (expectIdent "var") s
        in isLeft result @? "Expected failure"
    ]

test_check :: TestTree
test_check = testGroup "check"
    [ testCase "Match (Identifier): returns True, no advance" $
        let s = initialState [tId] 0
            result = runParser (check (T.Identifier "var")) s
        in case result of
            Right (v, s') -> do
                assertBool "Value" v
                assertEqual "Position" 0 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "No Match (LitInt): returns False, no advance" $
        let s = initialState [tId] 0
            result = runParser (check (T.LitInt 1)) s
        in case result of
            Right (v, s') -> do
                assertBool "Value" (not v)
                assertEqual "Position" 0 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err
    ]

test_match :: TestTree
test_match = testGroup "match"
    [ testCase "Match (Identifier): returns True and advances" $
        let s = initialState [tId] 0
            result = runParser (match (T.Identifier "var")) s
        in case result of
            Right (v, s') -> do
                assertBool "Value" v
                assertEqual "Position" 1 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "No Match (LitInt): returns False and does not advance" $
        let s = initialState [tId] 0
            result = runParser (match (T.LitInt 1)) s
        in case result of
            Right (v, s') -> do
                assertBool "Value" (not v)
                assertEqual "Position" 0 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err
    ]

test_choice :: TestTree
test_choice = testGroup "choice"
    [ testCase "First parser succeeds" $
        let s = initialState [] 0
            p1 = pure (1 :: Int)
            p2 = pure (2 :: Int)
            result = runParser (choice [p1, p2]) s
        in case result of
            Right (v, _) -> assertEqual "Expected 1" 1 v
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "First fails (via empty), second succeeds" $
        let s = initialState [] 0
            p1 = empty :: Parser Int
            p2 = pure 2
            result = runParser (choice [p1, p2]) s
        in case result of
            Right (v, _) -> assertEqual "Expected 2" 2 v
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "All fail (via empty)" $
        let s = initialState [] 0
            p1 = empty :: Parser Int
            p2 = empty :: Parser Int
            result = runParser (choice [p1, p2]) s
        in assertIsErrorWithMsg "Parser empty\n" result
    ]

test_between :: TestTree
test_between = testCase "between parses p enclosed by open and close" $
    let tokens = [tLParen, tInt, tRParen]
        s = initialState tokens 0
        pOpen = expect T.LParen
        pClose = expect T.RParen
        pMid = expect (T.LitInt 1)
        result = runParser (between pOpen pClose pMid) s
    in case result of
        Right (v, s') -> do
            assertEqual "Value" tInt v
            assertEqual "Position" 3 (psPosition s')
        Left err -> assertFailure $ "Expected success, got error: " ++ err

test_sepBy :: TestTree
test_sepBy = testGroup "sepBy"
    [ testCase "Zero elements (success - pure [])" $
        let s = initialState [tId] 0
            pVal = expect (T.LitInt 1)
            pSep = expect T.Comma
            result = runParser (sepBy pVal pSep) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value" [] v
                assertEqual "Position" 0 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "Multiple elements (success)" $
        let tokens = [tInt, tComma, tInt]
            s = initialState tokens 0
            pVal = expect (T.LitInt 1)
            pSep = expect T.Comma
            result = runParser (sepBy pVal pSep) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value: Two ints" [tInt, tInt] v
                assertEqual "Position" 3 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err
    ]

test_sepBy1 :: TestTree
test_sepBy1 = testGroup "sepBy1"
    [ testCase "One element (success)" $
        let tokens = [tInt]
            s = initialState tokens 0
            pVal = expect (T.LitInt 1)
            pSep = expect T.Comma
            result = runParser (sepBy1 pVal pSep) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value" [tInt] v
                assertEqual "Position" 1 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "Zero elements (failure)" $
        let s = initialState [tId] 0
            pVal = expect (T.LitInt 1)
            pSep = expect T.Comma
            result = runParser (sepBy1 pVal pSep) s
        in isLeft result @? "Expected failure"
    ]

test_sepEndBy :: TestTree
test_sepEndBy = testGroup "sepEndBy"
    [ testCase "Multiple elements ending without separator (success - pure [x])" $
        let tokens = [tInt, tComma, tInt, tId]
            s = initialState tokens 0
            pVal = expect (T.LitInt 1)
            pSep = expect T.Comma
            result = runParser (sepEndBy pVal pSep) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value: Two ints" [tInt, tInt] v
                assertEqual "Position" 3 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "Multiple elements ending with separator (success - sep *)" $
        let tokens = [tInt, tComma, tInt, tComma, tId]
            s = initialState tokens 0
            pVal = expect (T.LitInt 1)
            pSep = expect T.Comma
            result = runParser (sepEndBy pVal pSep) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value: Two ints" [tInt, tInt] v
                assertEqual "Position" 4 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err

    , testCase "Empty (success - pure [])" $
        let s = initialState [tId] 0
            pVal = expect (T.LitInt 1)
            pSep = expect T.Comma
            result = runParser (sepEndBy pVal pSep) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value" [] v
                assertEqual "Position" 0 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err
    ]

test_chainl1 :: TestTree
test_chainl1 = testGroup "chainl1"
    [ testCase "Chained operators (left-associative: (1+2)-1 = 2)" $
        let tokens = [tInt, tPlus, tInt, tMinus, tInt]
            s = initialState tokens 0
            pOpAdd = (\x y -> x + y) <$ expect T.OpPlus
            pOpSub = (\x y -> x - y) <$ expect T.OpMinus
            pVal = tokenMap (\t -> case t of T.LitInt n -> Just n; _ -> Nothing)
            result = runParser (chainl1 pVal (pOpAdd <|> pOpSub)) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value" 1 v
                assertEqual "Position" 5 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err
    ]

test_chainUnary :: TestTree
test_chainUnary = testGroup "chainUnary"
    [ testCase "Two unary operators (- - 1)" $
        let tokens = [tMinus, tMinus, tInt]
            s = initialState tokens 0
            pOp = negate <$ expect T.OpMinus
            pVal = tokenMap (\t -> case t of T.LitInt n -> Just n; _ -> Nothing)
            result = runParser (chainUnary pOp pVal) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value: -(-1) = 1" 1 v
                assertEqual "Position" 3 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err
    ]

test_chainPostfix :: TestTree
test_chainPostfix = testGroup "chainPostfix"
    [ testCase "Multiple postfix operators (1++)" $
        let tokens = [tInt, tPlus, tPlus]
            s = initialState tokens 0
            pOp = (\x -> x + 1) <$ expect T.OpPlus
            pVal = tokenMap (\t -> case t of T.LitInt n -> Just n; _ -> Nothing)
            result = runParser (chainPostfix pVal pOp) s
        in case result of
            Right (v, s') -> do
                assertEqual "Value: 1+1+1 = 3" 3 v
                assertEqual "Position" 3 (psPosition s')
            Left err -> assertFailure $ "Expected success, got error: " ++ err
    ]
