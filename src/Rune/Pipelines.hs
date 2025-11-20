module Rune.Pipelines (compilePipeline, interpretPipeline
) where

import Control.Monad ((>=>))
import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token)

--
-- public
--

compilePipeline :: FilePath -> FilePath -> IO [Token]
compilePipeline inFile _ =
  readFile inFile
    >>= either (error . show) pure . lexer

interpretPipeline :: FilePath -> IO [Token]
interpretPipeline =
  readFile >=> either (error . show) pure . lexer
