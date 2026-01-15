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
import Data.Either (partitionEithers)
import Rune.AST.Nodes (Field (..), FunctionSignature (..), Parameter (..), TopLevelDef (..), Type (..), Visibility (..))
import Rune.AST.Parser.ParseBlock (parseBlock)
import Rune.AST.Parser.ParseExpression (parseExpression)
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
    T.KwDef -> parseFunction False Public False False
    T.KwStruct -> parseStruct False
    T.KwAbstract -> advance >> parseStruct True
    T.KwSomewhere -> parseSomewhere
    _ -> failParse "Expected top-level definition (def, struct, export, somewhere)"

--
-- functions
--

parseExportedDef :: Parser TopLevelDef
parseExportedDef = do
  _ <- expect T.KwExport
  t <- peek
  case T.tokenKind t of
    T.KwDef -> parseFunction True Public False False
    _ -> failParse "Expected 'def' after 'export'"

parseFunction :: Bool -> Visibility -> Bool -> Bool -> Parser TopLevelDef
parseFunction isExport visibility isStatic isAbstract = do
  _ <- expect T.KwDef
  name <- parseIdentifier
  params <- withContext ("parameters of function '" ++ name ++ "'") parseParams
  retType <- withContext ("return type of function '" ++ name ++ "'") parseReturnType
  body <- withContext ("body of function '" ++ name ++ "'") parseBlock
  pure $ DefFunction name params retType body isExport visibility isStatic isAbstract

--
-- structs²
--

parseStruct :: Bool -> Parser TopLevelDef
parseStruct isAbstract = do
  name <- expect T.KwStruct *> parseIdentifier
  extensions <- parseStructExtensions
  _ <- expect T.LBrace
  (fields, methods) <- withContext
                        ("body of struct '" ++ name ++ "'") parseStructBody
  pure $ DefStruct name fields methods isAbstract extensions

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
  visibility <- parseVisibility
  isAbstract <- parseAbstractModifier
  isStatic <- parseStaticModifier
  case isStatic && isAbstract of
    True -> failParse "Struct item cannot be both static and abstract"
    _ -> pure ()
  t <- peek
  case T.tokenKind t of
    T.KwDef -> Right <$> parseFunction False visibility isStatic isAbstract
    T.Identifier _ -> Left <$> parseField visibility isStatic <* expect T.Semicolon
    _ -> failParse "Expected struct field or method"

parseStructExtensions :: Parser (Maybe [String])
parseStructExtensions = do
  t <- peek
  case T.tokenKind t of
    T.KwExtends -> do
      _ <- advance
      names <- sepBy parseIdentifier (expect T.Comma)
      pure $ Just names
    _ -> pure Nothing

--
-- parameters
--

parseParams :: Parser [Parameter]
parseParams = between (expect T.LParen) (expect T.RParen) (sepBy parseParameter (expect T.Comma))

parseParameter :: Parser Parameter
parseParameter = parseSelfParam <|> parseTypedParam

parseSelfParam :: Parser Parameter
parseSelfParam =
  Parameter "self" TypeAny Nothing <$ expectIdent "self"

--
-- typed parameters
--

parseTypedParam :: Parser Parameter
parseTypedParam =
  parseTypedParamWithType <|> parseTypedParamWithDefault
  where
    -- Try to parse: name: type [= value]
    parseTypedParamWithType = do
      name <- parseIdentifier
      _ <- expect T.Colon
      pType <- parseType
      pDefault <- try (Just <$> (expect T.OpAssign *> parseExpression)) <|> pure Nothing
      pure $ Parameter name pType pDefault
    -- Try to parse: name = value (no type annotation)
    parseTypedParamWithDefault = do
      name <- parseIdentifier
      _ <- expect T.OpAssign
      Parameter name TypeAny . Just <$> parseExpression

--
-- return type
--

parseReturnType :: Parser Type
parseReturnType =
  (expect T.OpArrow <|> expect T.OpSquigArrow) *> parseType

--
-- static
--

parseStaticModifier :: Parser Bool
parseStaticModifier = do
  t <- peek
  case T.tokenKind t of
    T.KwStatic -> advance >> pure True
    _ -> pure False

--
-- abstract
--

parseAbstractModifier :: Parser Bool
parseAbstractModifier = do
  t <- peek
  case T.tokenKind t of
    T.KwAbstract -> advance >> pure True
    _ -> pure False

--
-- fields
--

parseField :: Visibility -> Bool -> Parser Field
parseField visibility isStatic = do
  name <- parseIdentifier
  _ <- expect T.Colon
  typ <- parseType
  fDefault <- try (Just <$> (expect T.OpAssign *> parseExpression)) <|> pure Nothing
  pure $ Field name typ visibility isStatic fDefault

--
-- visibility
--

parseVisibility :: Parser Visibility
parseVisibility = do
  t <- peek
  case T.tokenKind t of
    T.KwPublic -> advance >> pure Public
    T.KwProtected -> advance >> pure Protected
    T.KwPrivate -> advance >> pure Private
    _ -> pure Public

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
  isExtern <- (True <$ expect T.KwExtern) <|> pure False
  _ <- expect T.KwDef
  name <- parseIdentifier
  paramTypes <- between (expect T.LParen) (expect T.RParen) (sepBy parseParamTypeInSignature (expect T.Comma))
  retType <- parseReturnType
  _ <- expect T.Semicolon
  pure $ FunctionSignature name paramTypes retType isExtern

parseParamTypeInSignature :: Parser Type
parseParamTypeInSignature =
  (paramType <$> try parseTypedParam) <|> parseType
