{-# LANGUAGE LambdaCase #-}

module Rune.AST.Parser (parseRune) where

import Control.Applicative (Alternative (..), optional)
import Data.Either (partitionEithers)
import Rune.AST.Nodes
import Rune.AST.ParserHelper
import Rune.AST.ParserTypes (Parser (..), ParserState (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseRune :: FilePath -> [T.Token] -> Either String Program
parseRune filepath tokens =
  case runParser parseProgram (ParserState tokens 0 filepath) of
    Left err -> Left err
    Right (prog, _) -> Right prog

--
-- private
--

--
-- Program
--

parseProgram :: Parser Program
parseProgram =
  Program
    <$> (psFilePath <$> getParserState)
    <*> many parseTopLevelDef
    <* expect T.EOF

--
-- Top Level
--

parseTopLevelDef :: Parser TopLevelDef
parseTopLevelDef =
  choice
    [ parseFunction,
      parseStruct,
      parseOverride
    ]
    <|> failParse "Expected top-level definition (def, struct, override)"

parseFunction :: Parser TopLevelDef
parseFunction =
  DefFunction
    <$> (expect T.KwDef *> parseIdentifier)
    <*> parseParams
    <*> parseReturnType
    <*> parseBlock

parseStruct :: Parser TopLevelDef
parseStruct = do
  name <- expect T.KwStruct *> parseIdentifier
  _ <- expect T.LBrace
  (fields, methods) <- parseStructBody
  pure $ DefStruct name fields methods

parseStructBody :: Parser ([Field], [TopLevelDef])
parseStructBody = do
  items <- many parseStructItem <* expect T.RBrace
  pure $ partitionEithers items

parseStructItem :: Parser (Either Field TopLevelDef)
parseStructItem =
  choice
    [ Right <$> parseFunction,
      Left <$> parseField <* expect T.Semicolon
    ]

parseOverride :: Parser TopLevelDef
parseOverride =
  DefOverride
    <$> (expect T.KwOverride *> expect T.KwDef *> parseIdentifier)
    <*> parseParams
    <*> parseReturnType
    <*> parseBlock

parseParams :: Parser [Parameter]
parseParams = between (expect T.LParen) (expect T.RParen) (sepBy parseParameter (expect T.Comma))

parseParameter :: Parser Parameter
parseParameter = parseSelfParam <|> parseTypedParam

parseSelfParam :: Parser Parameter
parseSelfParam =
  Parameter "self" TypeAny <$ expectIdent "self"

parseTypedParam :: Parser Parameter
parseTypedParam =
  Parameter
    <$> parseIdentifier
    <*> (expect T.Colon *> parseType)
      <|> failParse "Expected typed parameter"

parseReturnType :: Parser Type
parseReturnType =
  (expect T.OpArrow <|> expect T.OpSquigArrow) *> parseType

parseField :: Parser Field
parseField = Field <$> parseIdentifier <*> (expect T.Colon *> parseType)

--
-- Statements
--

parseBlock :: Parser Block
parseBlock = between (expect T.LBrace) (expect T.RBrace) (many parseStatement)

parseStatement :: Parser Statement
parseStatement =
  choice
    [ parseReturn,
      parseIf,
      parseFor,
      parseForEach,
      parseVarDeclOrExpr
    ]

parseReturn :: Parser Statement
parseReturn =
  expect T.KwReturn
    *> (StmtReturn <$> optional parseExpression)
    <* expect T.Semicolon

parseIf :: Parser Statement
parseIf =
  StmtIf
    <$> (expect T.KwIf *> parseExpression)
    <*> parseBlock
    <*> optional (expect T.KwElse *> (parseBlock <|> (pure <$> parseIf)))

parseFor :: Parser Statement
parseFor = do
  _ <- expect T.KwFor
  try (parseForRange) <|> parseForEachBody

parseForRange :: Parser Statement
parseForRange =
  StmtFor
    <$> parseIdentifier
    <*> (expect T.OpAssign *> parseExpression)
    <*> (expect T.KwTo *> parseExpression)
    <*> parseBlock

parseForEach :: Parser Statement
parseForEach = expect T.KwFor *> parseForEachBody

parseForEachBody :: Parser Statement
parseForEachBody =
  StmtForEach
    <$> parseIdentifier
    <*> (expect T.KwIn *> parseExpression)
    <*> parseBlock

parseVarDeclOrExpr :: Parser Statement
parseVarDeclOrExpr = try parseVarDecl <|> parseExprStmt

parseVarDecl :: Parser Statement
parseVarDecl =
  StmtVarDecl
    <$> parseIdentifier
    <*> optional (expect T.Colon *> parseType)
    <* expect T.OpAssign
    <*> parseExpression
    <* expect T.Semicolon

parseExprStmt :: Parser Statement
parseExprStmt = do
  expr <- parseExpression
  choice
    [ StmtExpr expr <$ expect T.Semicolon,
      do
        isEnd <- check T.RBrace
        if isEnd
          then pure (StmtReturn (Just expr))
          else empty,
      failParse "Expected ';' after expression or implicit return at block end"
    ]

--
-- Expressions
--

parseExpression :: Parser Expression
parseExpression = parseLogicalOr

parseLogicalOr :: Parser Expression
parseLogicalOr = chainl1 parseLogicalAnd (ExprBinary Or <$ expect T.OpOr)

parseLogicalAnd :: Parser Expression
parseLogicalAnd = chainl1 parseEquality (ExprBinary And <$ expect T.OpAnd)

parseEquality :: Parser Expression
parseEquality = chainl1 parseComparison op
  where
    op = (ExprBinary Eq <$ expect T.OpEq) <|> (ExprBinary Neq <$ expect T.OpNeq)

parseComparison :: Parser Expression
parseComparison = chainl1 parseTerm op
  where
    op =
      choice
        [ ExprBinary Lt <$ expect T.OpLt,
          ExprBinary Lte <$ expect T.OpLte,
          ExprBinary Gt <$ expect T.OpGt,
          ExprBinary Gte <$ expect T.OpGte
        ]

parseTerm :: Parser Expression
parseTerm = chainl1 parseFactor op
  where
    op = (ExprBinary Add <$ expect T.OpPlus) <|> (ExprBinary Sub <$ expect T.OpMinus)

parseFactor :: Parser Expression
parseFactor = chainl1 parseUnary op
  where
    op =
      choice
        [ ExprBinary Mul <$ expect T.OpMul,
          ExprBinary Div <$ expect T.OpDiv,
          ExprBinary Mod <$ expect T.OpMod
        ]

parseUnary :: Parser Expression
parseUnary =
  (ExprUnary Negate <$ expect T.OpMinus <*> parseUnary)
    <|> parsePostfix

parsePostfix :: Parser Expression
parsePostfix = chainPostfix parsePrimary op
  where
    op =
      choice
        [ do
            args <- between (expect T.LParen) (expect T.RParen) (sepBy parseExpression (expect T.Comma))
            pure $ \e -> ExprCall (getExprName e) args,
          do
            f <- expect T.Dot *> parseIdentifier
            pure $ \e -> ExprAccess e f,
          do
            _ <- expect T.OpErrorProp
            pure $ \e -> ExprUnary PropagateError e
        ]

getExprName :: Expression -> String
getExprName (ExprVar name) = name
getExprName (ExprAccess _ field) = field
getExprName _ = ""

parsePrimary :: Parser Expression
parsePrimary =
  choice
    [ parseLitInt,
      parseLitFloat,
      parseLitString,
      parseLitBool,
      ExprLitNull <$ expect T.LitNull,
      parseStructInitOrVar,
      between (expect T.LParen) (expect T.RParen) parseExpression
    ]

parseLitInt :: Parser Expression
parseLitInt =
  tokenMap $ \case
    T.LitInt n -> Just (ExprLitInt n)
    _ -> Nothing

parseLitFloat :: Parser Expression
parseLitFloat =
  tokenMap $ \case
    T.LitFloat f -> Just (ExprLitFloat f)
    _ -> Nothing

parseLitString :: Parser Expression
parseLitString =
  tokenMap $ \case
    T.LitString s -> Just (ExprLitString s)
    _ -> Nothing

parseLitBool :: Parser Expression
parseLitBool =
  tokenMap $ \case
    T.LitBool b -> Just (ExprLitBool b)
    _ -> Nothing

tokenMap :: (T.TokenKind -> Maybe a) -> Parser a
tokenMap f = do
  t <- peek
  case f (T.tokenKind t) of
    Just val -> advance >> pure val
    Nothing -> empty

parseStructInitOrVar :: Parser Expression
parseStructInitOrVar = try parseStructInit <|> (ExprVar <$> parseIdentifier)

parseStructInit :: Parser Expression
parseStructInit =
  ExprStructInit
    <$> parseIdentifier
    <*> between (expect T.LBrace) (expect T.RBrace) parseStructFields

parseStructFields :: Parser [(String, Expression)]
parseStructFields = sepEndBy parseStructField (expect T.Comma)

parseStructField :: Parser (String, Expression)
parseStructField = (,) <$> parseIdentifier <*> (expect T.Colon *> parseExpression)

--
-- Types & Identifiers
--

parseType :: Parser Type
parseType =
  choice
    [ TypeI8 <$ expect T.TypeI8,
      TypeI16 <$ expect T.TypeI16,
      TypeI32 <$ expect T.TypeI32,
      TypeI64 <$ expect T.TypeI64,
      TypeF32 <$ expect T.TypeF32,
      TypeF64 <$ expect T.TypeF64,
      TypeBool <$ expect T.TypeBool,
      TypeU8 <$ expect T.TypeU8,
      TypeU16 <$ expect T.TypeU16,
      TypeU32 <$ expect T.TypeU32,
      TypeString <$ expect T.TypeString,
      TypeAny <$ expect T.TypeAny,
      TypeNull <$ expect T.TypeNull,
      TypeCustom <$> parseIdentifier
    ]

parseIdentifier :: Parser String
parseIdentifier =
  tokenMap $ \case
    T.Identifier s -> Just s
    _ -> Nothing
