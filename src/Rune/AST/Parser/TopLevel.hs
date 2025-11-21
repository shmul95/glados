module Rune.AST.Parser.TopLevel (programParser)
where

import Control.Monad (void)
import Rune.AST.Nodes
import Rune.AST.Parser.Helpers
import qualified Rune.Lexer.Tokens as L
import Rune.AST.Parser.Impl
import Text.Megaparsec hiding (Token, match)

--
-- public
--

programParser :: Parser [TopLevelDef]
programParser = many topLevelDef <* match L.EOF

--
-- private
--

topLevelDef :: Parser TopLevelDef
topLevelDef =
  choice
    [ parseOverride,
      parseStruct,
      parseFunction
    ]

parseStruct :: Parser TopLevelDef
parseStruct = do
  void $ match L.KwStruct
  name <- identifier
  (fields, methods) <- braces parseStructBody
  return $ DefStruct name fields methods
