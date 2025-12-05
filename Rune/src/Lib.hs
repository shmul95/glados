module Lib (escapeString, isPrintable) where

--
-- public
--

isPrintable :: Char -> Bool
isPrintable c = c >= ' ' && c <= '~'

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\0' = "\\0"
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar c = [c]
