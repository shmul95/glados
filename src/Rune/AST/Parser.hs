module Rune.AST.Parser (parseRune) where

import Rune.AST.Nodes (Program(..))
import Rune.Lexer.Tokens (Token)

parseRune :: FilePath -> [Token] -> Either String Program
parseRune filepath _ = Right (Program filepath [])
