{-# LANGUAGE LambdaCase #-}

module Rune.AST.Parser (parseRune) where

import Control.Applicative (Alternative (..), optional)
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
parseStruct =
  DefStruct
    <$> (expect T.KwStruct *> parseIdentifier)
    <*> (expect T.LBrace *> (fst <$> parseStructBody))
    <*> (snd <$> parseStructBody <* expect T.RBrace)

parseStructBody :: Parser ([Field], [TopLevelDef])
parseStructBody = go [] []
  where
    go fs ms =
      choice
        [ expect T.RBrace *> pure (reverse fs, reverse ms),
          do
            m <- parseFunction
            go fs (m : ms),
          do
            f <- parseField <* expect T.Semicolon
            go (f : fs) ms
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
parseParameter = Parameter <$> parseIdentifier <*> (expect T.Colon *> parseType)

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
parseVarDeclOrExpr = do
  try parseVarDecl <|> parseExprStmt

parseVarDecl :: Parser Statement
parseVarDecl = do
  name <- parseIdentifier
  typeAnn <- optional (expect T.Colon *> parseType)
  _ <- expect T.OpAssign
  val <- parseExpression
  _ <- expect T.Semicolon
  pure $ StmtVarDecl name typeAnn val

parseExprStmt :: Parser Statement
parseExprStmt = do
  expr <- parseExpression
  isSemi <- match T.Semicolon
  if isSemi
    then pure $ StmtExpr expr
    else do
      isEnd <- check T.RBrace
      if isEnd
        then pure $ StmtReturn (Just expr)
        else failParse "Expected ';' after expression"

--
-- Expressions
--

parseExpression :: Parser Expression
parseExpression = parseLogicalOr

-- | |
parseLogicalOr :: Parser Expression
parseLogicalOr = chainl1 parseLogicalAnd (ExprBinary Or <$ expect T.OpOr)

-- &&
parseLogicalAnd :: Parser Expression
parseLogicalAnd = chainl1 parseEquality (ExprBinary And <$ expect T.OpAnd)

-- == !=
parseEquality :: Parser Expression
parseEquality = chainl1 parseComparison op
  where
    op = (ExprBinary Eq <$ expect T.OpEq) <|> (ExprBinary Neq <$ expect T.OpNeq)

-- < <= > >=
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

-- + -
parseTerm :: Parser Expression
parseTerm = chainl1 parseFactor op
  where
    op = (ExprBinary Add <$ expect T.OpPlus) <|> (ExprBinary Sub <$ expect T.OpMinus)

-- * / %

parseFactor :: Parser Expression
parseFactor = chainl1 parseUnary op
  where
    op =
      choice
        [ ExprBinary Mul <$ expect T.OpMul,
          ExprBinary Div <$ expect T.OpDiv,
          ExprBinary Mod <$ expect T.OpMod
        ]

-- -negate
parseUnary :: Parser Expression
parseUnary =
  (ExprUnary Negate <$ expect T.OpMinus <*> parseUnary)
    <|> parsePostfix

-- func(), struct.field, error?
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
parseLitInt = do
  t <- peek
  case T.tokenKind t of
    T.LitInt n -> advance >> pure (ExprLitInt n)
    _ -> empty

parseLitFloat :: Parser Expression
parseLitFloat = do
  t <- peek
  case T.tokenKind t of
    T.LitFloat f -> advance >> pure (ExprLitFloat f)
    _ -> empty

parseLitString :: Parser Expression
parseLitString = do
  t <- peek
  case T.tokenKind t of
    T.LitString s -> advance >> pure (ExprLitString s)
    _ -> empty

parseLitBool :: Parser Expression
parseLitBool = do
  t <- peek
  case T.tokenKind t of
    T.LitBool b -> advance >> pure (ExprLitBool b)
    _ -> empty

parseStructInitOrVar :: Parser Expression
parseStructInitOrVar = try parseStructInit <|> (ExprVar <$> parseIdentifier)

parseStructInit :: Parser Expression
parseStructInit =
  ExprStructInit
    <$> parseIdentifier
    <*> between (expect T.LBrace) (expect T.RBrace) (sepBy parseStructField (expect T.Comma))

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
parseIdentifier = do
  t <- peek
  case T.tokenKind t of
    T.Identifier s -> advance >> pure s
    _ -> empty
