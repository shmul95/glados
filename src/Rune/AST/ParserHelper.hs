module Rune.AST.ParserHelper
  ( makeError,
    advance,
    expect,
    getCurrentToken,
    check,
    match,
  )
where

import Control.Monad (when)
import Rune.AST.ParserTypes (Parser (..), ParserState (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

makeError :: String -> Parser a
makeError msg = Parser $ \s ->
  let tok = currentToken s
   in Left $ makeErrorMsg (psFilePath s) tok msg

advance :: Parser ()
advance = Parser $ \s -> Right ((), s {psPosition = psPosition s + 1})

expect :: T.TokenKind -> Parser T.Token
expect kind = do
  tok <- getCurrentToken
  if T.tokenKind tok == kind
    then advance >> return tok
    else makeError $ "Expected " ++ show kind

getCurrentToken :: Parser T.Token
getCurrentToken = Parser $ \s -> Right (currentToken s, s)

check :: T.TokenKind -> Parser Bool
check kind = do
  tok <- getCurrentToken
  return $ T.tokenKind tok == kind

match :: T.TokenKind -> Parser Bool
match kind = do
  isMatch <- check kind
  when isMatch advance
  return isMatch

--
-- private
--

makeErrorMsg :: String -> T.Token -> String -> String
makeErrorMsg fp tok msg =
  fp
    ++ ":"
    ++ show (T.tokenLine tok)
    ++ ":"
    ++ show (T.tokenColumn tok)
    ++ ": Parse error: "
    ++ msg
    ++ "\n  Got: "
    ++ show (T.tokenKind tok)

currentToken :: ParserState -> T.Token
currentToken s
  | psPosition s >= length (psTokens s) = T.Token T.EOF "" 0 0
  | otherwise = psTokens s !! psPosition s
