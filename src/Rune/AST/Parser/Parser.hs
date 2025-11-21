module Rune.AST.Parser.Parser (parseRune) where

import Rune.AST.Nodes
import Rune.AST.Parser.Helpers (formatError)
import Rune.AST.Parser.TopLevel (programParser)
import qualified Rune.Lexer.Tokens as L
import Text.Megaparsec (runParser)

--
-- public 
--

parseRune :: FilePath -> [L.Token] -> Either String Program
parseRune filename inputTokens =
  case runParser programParser filename inputTokens of
    Left err -> Left $ formatError inputTokens err
    Right prog -> Right $ Program filename prog
