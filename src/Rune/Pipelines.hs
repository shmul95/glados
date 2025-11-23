module Rune.Pipelines (compilePipeline, interpretPipeline) where

import Rune.Lexer.Lexer (lexer)
import Rune.Lexer.Tokens (Token)

--
-- public
--

compilePipeline :: FilePath -> FilePath -> IO [Token]
compilePipeline inFile _ =
  readFile inFile
    >>= either (error . show) pure . lexer inFile

-- NOTE: maybe in the future, when we call interpretPipeline with an REPL, inFile -> "<repl>"
interpretPipeline :: FilePath -> IO [Token]
interpretPipeline inFile =
  readFile inFile
    >>= either (error . show) pure . lexer inFile

