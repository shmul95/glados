{-# LANGUAGE CPP #-}

module Rune.AST.Parser.ParseTopLevel
#if defined(TESTING_EXPORT)
  ( parseTopLevels,
    parseTopLevelDef,
    parseExportedDef,
    parseFunction,
    parseStruct,
    parseStructBody,
    parseStructItem,
    parseOverride,
    parseParams,
    parseParameter,
    parseSelfParam,
    parseTypedParam,
    parseReturnType,
    parseField,
    parseSomewhere,
    parseFunctionSignatures,
    parseFunctionSignature,
    parseParamTypeInSignature
  )
#else
  ( parseTopLevels
  )
#endif
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Either (partitionEithers)
import Rune.AST.Nodes (Field (..), FunctionSignature (..), Parameter (..), TopLevelDef (..), Type (..))
import Rune.AST.Parser.ParseBlock (parseBlock)
import Rune.AST.Parser.ParseTypes (parseIdentifier, parseType)
import Rune.AST.ParserHelper (advance, between, check, expect, expectIdent, failParse, peek, sepBy, try, withContext)
import Rune.AST.Types (Parser (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseTopLevels :: Parser [TopLevelDef]
parseTopLevels = do
  isEof <- check T.EOF
  if isEof then pure []
  else do
    def  <- parseTopLevelDef
    defs <- parseTopLevels
    pure (def : defs)

--
-- private parsers
--

parseTopLevelDef :: Parser TopLevelDef
parseTopLevelDef = do
  t <- peek
  case T.tokenKind t of
    T.KwExport -> parseExportedDef
    T.KwDef -> parseFunction False
    T.KwStruct -> parseStruct
    T.KwOverride -> parseOverride False
    T.KwSomewhere -> parseSomewhere
    _ -> failParse "Expected top-level definition (def, struct, override, export, somewhere)"

--
-- functions
--

parseExportedDef :: Parser TopLevelDef
parseExportedDef = do
  _ <- expect T.KwExport
  t <- peek
  case T.tokenKind t of
    T.KwDef -> parseFunction True
    T.KwOverride -> parseOverride True
    _ -> failParse "Expected 'def' or 'override' after 'export'"

parseFunction :: Bool -> Parser TopLevelDef
parseFunction isExport = do
  _ <- expect T.KwDef
  name <- parseIdentifier
  params <- withContext ("parameters of function '" ++ name ++ "'") parseParams
  retType <- withContext ("return type of function '" ++ name ++ "'") parseReturnType
  body <- withContext ("body of function '" ++ name ++ "'") parseBlock
  pure $ DefFunction name params retType body isExport

--
-- structs
--

parseStruct :: Parser TopLevelDef
parseStruct = do
  name <- expect T.KwStruct *> parseIdentifier
  _ <- expect T.LBrace
  (fields, methods) <- withContext ("body of struct '" ++ name ++ "'") parseStructBody
  pure $ DefStruct name fields methods

parseStructBody :: Parser ([Field], [TopLevelDef])
parseStructBody = do
  partitionEithers <$> parseStructItemsLoop

parseStructItemsLoop :: Parser [Either Field TopLevelDef]
parseStructItemsLoop = do
  isEnd <- check T.RBrace
  if isEnd then
    advance >> pure []
  else do
    item <- parseStructItem
    rest <- parseStructItemsLoop
    pure (item : rest)

parseStructItem :: Parser (Either Field TopLevelDef)
parseStructItem = do
  t<- peek
  case T.tokenKind t of
    T.KwDef -> Right <$> parseFunction False
    T.Identifier _ -> Left <$> parseField <* expect T.Semicolon
    _ -> failParse "Expected struct field or method"

--
-- overrides
--

parseOverride :: Bool -> Parser TopLevelDef
parseOverride isExport = do
  _ <- expect T.KwOverride *> expect T.KwDef
  name <- parseIdentifier
  params <- withContext ("parameters of override '" ++ name ++ "'") parseParams
  retType <- withContext ("return type of override '" ++ name ++ "'") parseReturnType
  body <- withContext ("body of override '" ++ name ++ "'") parseBlock
  pure $ DefOverride name params retType body isExport

--
-- parameters
--

parseParams :: Parser [Parameter]
parseParams = between (expect T.LParen) (expect T.RParen) (sepBy parseParameter (expect T.Comma))

parseParameter :: Parser Parameter
parseParameter = parseSelfParam <|> parseTypedParam

parseSelfParam :: Parser Parameter
parseSelfParam =
  Parameter "self" TypeAny <$ expectIdent "self"

--
-- typed parameters
--

parseTypedParam :: Parser Parameter
parseTypedParam =
  Parameter
    <$> parseIdentifier
    <*> (expect T.Colon *> parseType)
    <|> failParse "Expected typed parameter (name: type)"

--
-- return type
--

parseReturnType :: Parser Type
parseReturnType =
  (expect T.OpArrow <|> expect T.OpSquigArrow) *> parseType

--
-- fields
--

parseField :: Parser Field
parseField = Field <$> parseIdentifier <*> (expect T.Colon *> parseType)

--
-- somewhere (forward declarations)
--

parseSomewhere :: Parser TopLevelDef
parseSomewhere =
  do _ <- expect T.KwSomewhere *> expect T.LBrace
     DefSomewhere <$> parseFunctionSignatures

parseFunctionSignatures :: Parser [FunctionSignature]
parseFunctionSignatures = do
  isEnd <- check T.RBrace
  if isEnd then
    advance >> pure []
  else do
    sig <- parseFunctionSignature
    rest <- parseFunctionSignatures
    pure (sig : rest)

parseFunctionSignature :: Parser FunctionSignature
parseFunctionSignature = do
  isOverride <- check T.KwOverride
  when isOverride advance
  _ <- expect T.KwDef
  name <- parseIdentifier
  paramTypes <- between (expect T.LParen) (expect T.RParen) (sepBy parseParamTypeInSignature (expect T.Comma))
  retType <- parseReturnType
  _ <- expect T.Semicolon
  pure $ FunctionSignature name paramTypes retType isOverride

parseParamTypeInSignature :: Parser Type
parseParamTypeInSignature = 
  (paramType <$> try parseTypedParam) <|> parseType
