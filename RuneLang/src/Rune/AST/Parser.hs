module Rune.AST.Parser (parseRune) where

import Rune.AST.Nodes (Program (..))
import Rune.AST.Parser.ParseTopLevel (parseTopLevels)
import Rune.AST.ParserHelper
import Rune.AST.Types (Parser (..), ParserState (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseRune :: FilePath -> [T.Token] -> Either String Program
parseRune filepath tokens =
  case runParser parseProgram (ParserState tokens 0 filepath 0) of
    Left err -> Left err
    Right (prog, _) -> Right prog

--
-- private
--

--
-- Program
--

parseProgram :: Parser Program
parseProgram =
  Program
    <$> (psFilePath <$> getParserState)
    <*> parseTopLevels
    <* expect T.EOF

--
-- Types & Identifiers
--
