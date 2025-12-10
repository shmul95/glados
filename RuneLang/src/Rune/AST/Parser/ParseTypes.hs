{-# LANGUAGE LambdaCase #-}

module Rune.AST.Parser.ParseTypes
  ( parseType,
    parseIdentifier,
  )
where

import Control.Applicative ((<|>))
import Rune.AST.Nodes (Type (..))
import Rune.AST.ParserHelper (choice, failParse, tokenMap)
import Rune.AST.Types (Parser (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseType :: Parser Type
parseType =
  choice
    [ tokenMap tokenToType,
      TypeCustom <$> parseIdentifier
    ]
    <|> failParse "Expected type"

parseIdentifier :: Parser String
parseIdentifier =
  tokenMap $ \case
    T.Identifier s -> Just s
    _ -> Nothing

--
-- private helpers
--

tokenToType :: T.TokenKind -> Maybe Type
tokenToType T.TypeI8 = Just TypeI8
tokenToType T.TypeI16 = Just TypeI16
tokenToType T.TypeI32 = Just TypeI32
tokenToType T.TypeI64 = Just TypeI64
tokenToType T.TypeU8 = Just TypeU8
tokenToType T.TypeU16 = Just TypeU16
tokenToType T.TypeU32 = Just TypeU32
tokenToType T.TypeU64 = Just TypeU64
tokenToType T.TypeChar = Just TypeChar
tokenToType T.TypeF32 = Just TypeF32
tokenToType T.TypeF64 = Just TypeF64
tokenToType T.TypeBool = Just TypeBool
tokenToType T.TypeString = Just TypeString
tokenToType T.TypeAny = Just TypeAny
tokenToType T.TypeNull = Just TypeNull
tokenToType T.LitNull = Just TypeNull
tokenToType _ = Nothing
