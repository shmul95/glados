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
    [ tokenMap tokenTypeToType,
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

tokenTypeToType :: T.TokenKind -> Maybe Type
tokenTypeToType T.TypeI8 = Just TypeI8
tokenTypeToType T.TypeI16 = Just TypeI16
tokenTypeToType T.TypeI32 = Just TypeI32
tokenTypeToType T.TypeI64 = Just TypeI64
tokenTypeToType T.TypeF32 = Just TypeF32
tokenTypeToType T.TypeF64 = Just TypeF64
tokenTypeToType T.TypeBool = Just TypeBool
tokenTypeToType T.TypeU8 = Just TypeU8
tokenTypeToType T.TypeU16 = Just TypeU16
tokenTypeToType T.TypeU32 = Just TypeU32
tokenTypeToType T.TypeString = Just TypeString
tokenTypeToType T.TypeAny = Just TypeAny
tokenTypeToType T.TypeNull = Just TypeNull
tokenTypeToType _ = Nothing
