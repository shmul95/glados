{-# LANGUAGE OverloadedStrings #-}

module Rune.AST.Parser.Helpers
  ( formatError,
    satisfyToken,
    match,
    identifier,
    parens,
    braces,
    commaSep,
    semicolon,
    chainl1,
    Parser,
  )
where

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)
import Rune.AST.Parser.Types (Parser)
import qualified Rune.Lexer.Tokens as L
import Text.Megaparsec hiding (Token, match)

--
-- syntax errors formatting
--

formatError :: [L.Token] -> ParseErrorBundle [L.Token] Void -> String
formatError inputTokens bundle =
  unlines $ map (formatParseError inputTokens) (NE.toList $ bundleErrors bundle)

formatParseError :: [L.Token] -> ParseError [L.Token] Void -> String
formatParseError inputTokens err =
  let offset = errorOffset err
      maybeTok =
        if offset < length inputTokens
          then Just (inputTokens !! offset)
          else Nothing

      (line, col) = case maybeTok of
        Just t -> (L.tokenLine t, L.tokenColumn t)
        Nothing -> case NE.last (L.Token L.EOF "" 0 0 NE.:| inputTokens) of
          lastTok -> (L.tokenLine lastTok, L.tokenColumn lastTok)

      unexpectedMsg = case err of
        TrivialError _ (Just (Tokens (t NE.:| _))) _ -> "Unexpected '" ++ L.tokenValue t ++ "'"
        TrivialError _ (Just (Label l)) _ -> "Unexpected label: " ++ toList l
        TrivialError _ (Just EndOfInput) _ -> "Unexpected end of input"
        TrivialError _ Nothing _ -> "Unexpected end of input"
        FancyError _ _ -> "Unknown error"

      expectedMsg = case err of
        TrivialError _ _ expectedSet ->
          let items = toList expectedSet
           in if null items
                then ""
                else ", expected " ++ showExpected items
        _ -> ""

      showExpected [] = ""
      showExpected items = unwords (map showItem items)

      showItem (Tokens (t NE.:| _)) = "'" ++ L.tokenValue t ++ "'"
      showItem (Label l) = toList l
      showItem EndOfInput = "end of input"
   in "Error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ unexpectedMsg ++ expectedMsg

--
-- parser combinators & helpers
--

satisfyToken :: (L.Token -> Bool) -> Parser L.Token
satisfyToken predicate = try $ do
  t <- anySingle
  if predicate t then return t else empty

match :: L.TokenKind -> Parser L.Token
match kind = satisfyToken (\t -> L.tokenKind t == kind)

identifier :: Parser String
identifier = try $ do
  t <- anySingle
  case L.tokenKind t of
    L.Identifier s -> return s
    _ -> empty

parens :: Parser a -> Parser a
parens = between (match L.LParen) (match L.RParen)

braces :: Parser a -> Parser a
braces = between (match L.LBrace) (match L.RBrace)

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (match L.Comma)

semicolon :: Parser L.Token
semicolon = match L.Semicolon

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> return x
