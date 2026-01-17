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
    parseSomewhereDecls,
    parseSomewhereDecl,
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
import Rune.AST.Nodes (Field (..), FunctionSignature (..), Parameter (..), TopLevelDef (..), Type (..), SomewhereDecl (..))
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
    T.KwDef -> parseFunction False
    T.KwStruct -> parseStruct
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
    T.KwDef -> parseFunction True
    _ -> failParse "Expected 'def' after 'export'"

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
     DefSomewhere <$> parseSomewhereDecls

parseSomewhereDecls :: Parser [SomewhereDecl]
parseSomewhereDecls = do
  isEnd <- check T.RBrace
  if isEnd then
    advance >> pure []
  else do
    decl <- parseSomewhereDecl
    rest <- parseSomewhereDecls
    pure $ decl : rest

parseSomewhereDecl :: Parser SomewhereDecl
parseSomewhereDecl = do
  tok <- peek
  case T.tokenKind tok of
    T.KwUse -> parseUseStatement
    T.KwDef -> DeclFuncSig <$> parseFunctionSignature
    T.KwExtern -> DeclFuncSig <$> parseFunctionSignature
    T.KwStruct -> DeclDefs <$> parseStruct
    _ -> failParse $ "Expected use, function signature, struct definition, or extern function, got: " ++ show tok

parseUseStatement :: Parser SomewhereDecl
parseUseStatement = do
  _ <- expect T.KwUse
  fileName <- parseUseFileName
  _ <- expect T.Semicolon
  pure $ DeclUse fileName

parseUseFileName :: Parser String
parseUseFileName = do
  tok <- peek
  case T.tokenKind tok of
    T.Identifier name -> do
      advance
      -- Check for optional .sw or .somewhere extension
      dotTok <- peek
      case T.tokenKind dotTok of
        T.Dot -> do
          advance
          extTok <- peek
          case T.tokenKind extTok of
            T.Identifier "sw" -> do
              advance
              pure $ name ++ ".sw"
            T.Identifier "somewhere" -> do
              advance
              pure $ name ++ ".somewhere"
            _ -> failParse "Expected 'sw' or 'somewhere' file extension"
        _ -> pure name  -- Allow extension-less filename
    _ -> failParse "Expected filename"

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
  pure $ FunctionSignature { sigFuncName = name, sigParams = paramTypes, sigReturnType = retType, sigIsExtern = isExtern }

parseParamTypeInSignature :: Parser Type
parseParamTypeInSignature =
  (paramType <$> try parseTypedParam) <|> parseType
