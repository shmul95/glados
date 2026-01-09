module Lib (escapeString, isPrintable, fixpoint, (>*>)) where

--
-- public
--

(>*>) :: (a -> b -> c) -> b -> a -> c
(>*>) = flip
infixl 4 >*>

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

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
  | x == x'   = x
  | otherwise = fixpoint f x'
  where x' = f x
