{-# LANGUAGE LambdaCase #-}

module Rune.AST.Parser.ParseExpression
  ( parseExpression,
  )
where

import Control.Applicative ((<|>))
import Rune.AST.Nodes (BinaryOp (..), Expression (..), UnaryOp (..))
import Rune.AST.Parser.ParseTypes (parseIdentifier, parseType)
import Rune.AST.ParserHelper (between, chainPostfix, chainl1, choice, expect, failParse, getCurrentPos, sepBy, sepEndBy, tokenMap, try, withContext)
import Rune.AST.Types (Parser (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseExpression :: Parser Expression
parseExpression = parseLogicalOr

--
-- private parsers
--

parseLogicalOr :: Parser Expression
parseLogicalOr = chainl1 parseLogicalAnd op
  where
    op = do
      pos <- getCurrentPos
      _ <- expect T.OpOr
      return $ ExprBinary pos Or

parseLogicalAnd :: Parser Expression
parseLogicalAnd = chainl1 parseBitAnd op
  where
    op = do
      pos <- getCurrentPos
      _ <- expect T.OpAnd
      return $ ExprBinary pos And

parseBitAnd :: Parser Expression
parseBitAnd = chainl1 parseEquality op
  where
    op = do
      pos <- getCurrentPos
      _ <- expect T.OpBitAnd
      return $ ExprBinary pos BitAnd

parseEquality :: Parser Expression
parseEquality = chainl1 parseComparison op
  where
    op = do
      pos <- getCurrentPos
      opType <- (Eq <$ expect T.OpEq) <|> (Neq <$ expect T.OpNeq)
      return $ ExprBinary pos opType

parseComparison :: Parser Expression
parseComparison = chainl1 parseTerm op
  where
    op = do
      pos <- getCurrentPos
      opType <- choice
        [ Lt <$ expect T.OpLt,
          Lte <$ expect T.OpLte,
          Gt <$ expect T.OpGt,
          Gte <$ expect T.OpGte
        ]
      return $ ExprBinary pos opType

parseTerm :: Parser Expression
parseTerm = chainl1 parseFactor op
  where
    op = do
      pos <- getCurrentPos
      opType <- (Add <$ expect T.OpPlus) <|> (Sub <$ expect T.OpMinus)
      return $ ExprBinary pos opType

parseFactor :: Parser Expression
parseFactor = chainl1 parseUnary op
  where
    op = do
      pos <- getCurrentPos
      opType <- choice
        [ Mul <$ expect T.OpMul,
          Div <$ expect T.OpDiv,
          Mod <$ expect T.OpMod
        ]
      return $ ExprBinary pos opType

parseUnary :: Parser Expression
parseUnary =
  choice
    [ do
        pos <- getCurrentPos
        _ <- expect T.OpMinus
        ExprUnary pos Negate <$> parseUnary,
      do
        pos <- getCurrentPos
        _ <- expect T.OpNot
        ExprUnary pos Not <$> parseUnary,
      do
        pos <- getCurrentPos
        _ <- expect T.OpBitNot
        ExprUnary pos BitNot <$> parseUnary,
      do
        pos <- getCurrentPos
        _ <- expect T.OpInc
        ExprUnary pos PrefixInc <$> parseUnary,
      do
        pos <- getCurrentPos
        _ <- expect T.OpDec
        ExprUnary pos PrefixDec <$> parseUnary,
      do
        pos <- getCurrentPos
        _ <- expect T.OpMul
        ExprUnary pos Deref <$> parseUnary,
      do
        pos <- getCurrentPos
        _ <- expect T.OpBitAnd
        ExprUnary pos Reference <$> parseUnary,
      parseSizeof,
      parsePostfix
    ]

parseSizeof :: Parser Expression
parseSizeof = do
  pos <- getCurrentPos
  _   <- expect T.KwSizeof
  val <- choice
    [ Left <$> try (between (expect T.LParen) (expect T.RParen) parseType),
      Right <$> try parseUnary,
      Left <$> parseType
    ]
  pure $ ExprSizeof pos val

parsePostfix :: Parser Expression
parsePostfix = chainPostfix parsePrimary op
  where
    op =
      choice
        [ parseCallPostfix,
          parseFieldAccessPostfix,
          parseIndexPostfix,
          parseCastPostfix,
          parseErrorPropPostfix,
          parseIncPostfix,
          parseDecPostfix
        ]

parseCallPostfix :: Parser (Expression -> Expression)
parseCallPostfix = do
  pos <- getCurrentPos
  args <- between (expect T.LParen) (expect T.RParen) (sepBy (withContext "argument" parseExpression) (expect T.Comma))
  pure $ \e -> ExprCall pos e args

parseFieldAccessPostfix :: Parser (Expression -> Expression)
parseFieldAccessPostfix = do
  pos <- getCurrentPos
  f <- expect T.Dot *> parseIdentifier
  pure $ \e -> ExprAccess pos e f

parseIndexPostfix :: Parser (Expression -> Expression)
parseIndexPostfix = do
  pos <- getCurrentPos
  index <- between (expect T.LBracket) (expect T.RBracket) (withContext "array index" parseExpression)
  pure $ \e -> ExprIndex pos e index

parseCastPostfix :: Parser (Expression -> Expression)
parseCastPostfix = do
  pos <- getCurrentPos
  _ <- expect T.Colon <|> expect T.KwAs
  t <- parseType
  pure $ \e -> ExprCast pos e t

parseErrorPropPostfix :: Parser (Expression -> Expression)
parseErrorPropPostfix = do
  pos <- getCurrentPos
  _ <- expect T.OpErrorProp
  pure $ \e -> ExprUnary pos PropagateError e

parseIncPostfix :: Parser (Expression -> Expression)
parseIncPostfix = do
  pos <- getCurrentPos
  _ <- expect T.OpInc
  pure $ \e -> ExprUnary pos PostfixInc e

parseDecPostfix :: Parser (Expression -> Expression)
parseDecPostfix = do
  pos <- getCurrentPos
  _ <- expect T.OpDec
  pure $ \e -> ExprUnary pos PostfixDec e
--
-- literals & primary expressions
--

parsePrimary :: Parser Expression
parsePrimary =
  choice
    [ parseLitInt,
      parseLitFloat,
      parseLitString,
      parseChar,
      parseLitBool,
      parseLitArray,
      do
        pos <- getCurrentPos
        _ <- expect T.LitNull <|> expect T.TypeNull
        pure $ ExprLitNull pos,
      parseStructInitOrVar,
      between (expect T.LParen) (expect T.RParen) (withContext "parenthesized expression" parseExpression)
    ]
    <|> failParse "Expected expression (literal, variable, or '('...)"

parseLitInt :: Parser Expression
parseLitInt = do
  pos <- getCurrentPos
  tokenMap $ \case
    T.LitInt n -> Just (ExprLitInt pos n)
    _ -> Nothing

parseLitFloat :: Parser Expression
parseLitFloat = do
  pos <- getCurrentPos
  tokenMap $ \case
    T.LitFloat f -> Just (ExprLitFloat pos f)
    _ -> Nothing

parseChar :: Parser Expression
parseChar = do
  pos <- getCurrentPos
  tokenMap $ \case
    T.LitChar c -> Just (ExprLitChar pos c)
    _ -> Nothing

parseLitString :: Parser Expression
parseLitString = do
  pos <- getCurrentPos
  tokenMap $ \case
    T.LitString s -> Just (ExprLitString pos s)
    _ -> Nothing

parseLitBool :: Parser Expression
parseLitBool = do
  pos <- getCurrentPos
  tokenMap $ \case
    T.LitBool b -> Just (ExprLitBool pos b)
    _ -> Nothing

parseLitArray :: Parser Expression
parseLitArray = do
  pos <- getCurrentPos
  exprs <- between
    (expect T.LBracket) (expect T.RBracket)
    (sepEndBy (withContext "array element" parseExpression) (expect T.Comma))
  pure $ ExprLitArray pos exprs

--
-- struct
--

parseStructInitOrVar :: Parser Expression
parseStructInitOrVar = try parseStructInit <|> do
  pos <- getCurrentPos
  ExprVar pos <$> parseIdentifier

parseStructInit :: Parser Expression
parseStructInit = do
  pos <- getCurrentPos
  name <- parseIdentifier
  fields <- between (expect T.LBrace) (expect T.RBrace) parseStructFields
  pure $ ExprStructInit pos name fields

parseStructFields :: Parser [(String, Expression)]
parseStructFields = sepEndBy parseStructField (expect T.Comma)

parseStructField :: Parser (String, Expression)
parseStructField = do
  name <- parseIdentifier
  _ <- expect T.Colon
  val <- withContext ("value of field '" <> name <> "'") parseExpression
  pure (name, val)
