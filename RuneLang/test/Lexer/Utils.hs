module Lexer.Utils (tok, lexTest, lexFailTest) where

import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token (..), TokenKind (..))
import Test.Tasty.HUnit (assertEqual, assertFailure)

--
-- public
--

tok :: TokenKind -> String -> Int -> Int -> Token
tok = Token

lexTest :: String -> [Token] -> IO ()
lexTest input expected =
  case lexer "test.rune" input of
    Right actual -> assertEqual ("Lexing: " ++ show input) expected actual
    Left err -> assertFailure $ "Lexing failed unexpectedly: " ++ show err

lexFailTest :: String -> IO ()
lexFailTest input =
  case lexer "test.rune" input of
    Left _ -> return ()
    Right tokens -> assertFailure $ "Expected lexer failure on input: " ++ show input ++ ", but got tokens: " ++ show tokens
