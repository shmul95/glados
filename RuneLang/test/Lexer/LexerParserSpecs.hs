module Lexer.LexerParserSpecs (lexerParserTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))
import Rune.Lexer.LexerParser (Parser, makeTokenParser, makeReservedTokenParser, applyPosition)
import Text.Megaparsec (runParser, choice, try)

import Lexer.Utils (tok)

--
-- public
--

lexerParserTests :: TestTree
lexerParserTests =
  testGroup
    "LexerParser Tests (Core Parser Builders)"
    [ test_makeTokenParser_success,
      test_makeTokenParser_prefix_ok,
      test_makeReservedTokenParser_success,
      test_makeReservedTokenParser_prefix_fail,
      test_makeReservedTokenParser_suffix_underscore_fail,
      test_applyPosition_success,
      test_applyPosition_failure
    ]

--
-- helpers
--

runConstructorParser_ :: String -> Parser (Int -> Int -> Token) -> IO (Either String Token)
runConstructorParser_ input parser =
  return $ case runParser parser "test.rune" input of
    Right constructor -> Right $ constructor 1 1
    Left _ -> Left "Parser failed"

runConstructorParserChoice :: String -> [Parser (Int -> Int -> Token)] -> IO (Either String Token)
runConstructorParserChoice input parsers =
  if null parsers
    then return $ Left "No parsers provided"
    else runConstructorParser_ input (choice $ map try parsers)

safeHeadParser :: [Parser (Int -> Int -> Token)] -> Parser (Int -> Int -> Token)
safeHeadParser [] = error "Mock map cannot be empty in LexerParserSpecs"
safeHeadParser (p:_) = p

--
-- private
--

mockMap :: [(TokenKind, String)]
mockMap = [(KwDef, "def"), (OpPlus, "+"), (LParen, "(")]

test_makeTokenParser_success :: TestTree
test_makeTokenParser_success = testCase "makeTokenParser success (OpPlus)" $ do
  res <- runConstructorParserChoice "+" (makeTokenParser mockMap)
  assertEqual "Should parse '+'" (Right $ tok OpPlus "+" 1 1) res

test_makeTokenParser_prefix_ok :: TestTree
test_makeTokenParser_prefix_ok = testCase "makeTokenParser (no prefix check)" $ do
  res <- runConstructorParser_ "defX" (safeHeadParser $ makeTokenParser mockMap)
  assertEqual "Should tokenize 'def' prefix" (Right $ tok KwDef "def" 1 1) res

test_makeReservedTokenParser_success :: TestTree
test_makeReservedTokenParser_success = testCase "makeReservedTokenParser success (KwDef)" $ do
  res <- runConstructorParser_ "def" (safeHeadParser $ makeReservedTokenParser mockMap)
  assertEqual "Should parse 'def'" (Right $ tok KwDef "def" 1 1) res

test_makeReservedTokenParser_prefix_fail :: TestTree
test_makeReservedTokenParser_prefix_fail = testCase "makeReservedTokenParser (prefix check failure)" $ do
  res <- runConstructorParser_ "defX" (safeHeadParser $ makeReservedTokenParser mockMap)
  case res of
    Left _ -> return ()
    Right actual -> assertFailure $ "Should have failed on prefix 'defX', but got: " ++ show actual

test_makeReservedTokenParser_suffix_underscore_fail :: TestTree
test_makeReservedTokenParser_suffix_underscore_fail = testCase "makeReservedTokenParser (suffix _ failure)" $ do
  res <- runConstructorParser_ "def_" (safeHeadParser $ makeReservedTokenParser mockMap)
  case res of
    Left _ -> return ()
    Right actual -> assertFailure $ "Should have failed on suffix 'def_', but got: " ++ show actual

test_applyPosition_success :: TestTree
test_applyPosition_success = testCase "applyPosition success" $ do
  let parser = applyPosition (makeTokenParser [(LParen, "(")]) 10 5
  case runParser parser "test.rune" "(" of
      Right actualToken -> assertEqual "Token should have correct position" (tok LParen "(" 10 5) actualToken
      Left err -> assertFailure $ "Parser failed: " ++ show err

test_applyPosition_failure :: TestTree
test_applyPosition_failure = testCase "applyPosition failure" $ do
  let parser = applyPosition (makeTokenParser [(LParen, "(")]) 1 1
  case runParser parser "test.rune" "[" of
      Left _ -> return ()
      Right actualToken -> assertFailure $ "Parser should have failed, but got: " ++ show actualToken
