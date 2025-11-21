module Rune.AST.Parser.Types (Parser) where

import Data.Void (Void)
import Rune.Lexer.Tokens (Token)
import Text.Megaparsec hiding (Token)

--
-- public
--

type Parser = Parsec Void [Token]
