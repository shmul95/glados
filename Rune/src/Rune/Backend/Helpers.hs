module Rune.Backend.Helpers
  ( emit,
    escapeString,
  )
where

import Data.List (intercalate)

--
-- public
--

emit :: Int -> String -> String
emit lvl s = replicate (lvl * 4) ' ' ++ s

escapeString :: String -> String
escapeString s = intercalate "," (map escapeChar s)

--
-- private
--

escapeChar :: Char -> String
escapeChar '\0' = "0"
escapeChar '\n' = "10"
escapeChar '\t' = "9"
escapeChar '\r' = "13"
escapeChar '\\' = "'\\\\'"
escapeChar '"' = "'\"'"
escapeChar '\'' = "'''"
escapeChar c
  | c >= ' ' && c <= '~' = "'" ++ [c] ++ "'"
  | otherwise = show (fromEnum c)
