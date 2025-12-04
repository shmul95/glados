{-# LANGUAGE LambdaCase #-}

module Rune.AST.Parser.ParseExpression
  ( parseExpression,
  )
where

import Control.Applicative ((<|>))
import Rune.AST.Nodes (BinaryOp (..), Expression (..), UnaryOp (..))
import Rune.AST.Parser.ParseTypes (parseIdentifier, parseType)
import Rune.AST.ParserHelper (between, chainPostfix, chainl1, choice, expect, failParse, sepBy, sepEndBy, tokenMap, try, withContext)
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
parseLogicalOr = chainl1 parseLogicalAnd (ExprBinary Or <$ expect T.OpOr)

parseLogicalAnd :: Parser Expression
parseLogicalAnd = chainl1 parseEquality (ExprBinary And <$ expect T.OpAnd)

parseEquality :: Parser Expression
parseEquality = chainl1 parseComparison op
  where
    op = (ExprBinary Eq <$ expect T.OpEq) <|> (ExprBinary Neq <$ expect T.OpNeq)

parseComparison :: Parser Expression
parseComparison = chainl1 parseCast op
  where
    op =
      choice
        [ ExprBinary Lt <$ expect T.OpLt,
          ExprBinary Lte <$ expect T.OpLte,
          ExprBinary Gt <$ expect T.OpGt,
          ExprBinary Gte <$ expect T.OpGte
        ]

parseCast :: Parser Expression
parseCast = do
  expr <- parseTerm
  parseCastSuffix expr <|> pure expr

parseCastSuffix :: Expression -> Parser Expression
parseCastSuffix expr = do
  _ <- expect T.KwAs
  targetType <- parseType
  pure $ ExprCast expr targetType

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
  choice
    [ ExprUnary Negate <$ expect T.OpMinus <*> parseUnary,
      ExprUnary PrefixInc <$ expect T.OpInc <*> parseUnary,
      ExprUnary PrefixDec <$ expect T.OpDec <*> parseUnary,
      parsePostfix
    ]

parsePostfix :: Parser Expression
parsePostfix = chainPostfix parsePrimary op
  where
    op =
      choice
        [ parseCallPostfix,
          parseFieldAccessPostfix,
          parseErrorPropPostfix,
          parseIncPostfix,
          parseDecPostfix
        ]

parseCallPostfix :: Parser (Expression -> Expression)
parseCallPostfix = do
  args <- between (expect T.LParen) (expect T.RParen) (sepBy (withContext "argument" parseExpression) (expect T.Comma))
  pure $ \e -> case e of
    ExprAccess target field -> ExprCall field (target : args)
    _ -> ExprCall (getExprName e) args

parseFieldAccessPostfix :: Parser (Expression -> Expression)
parseFieldAccessPostfix = do
  f <- expect T.Dot *> parseIdentifier
  pure $ \e -> ExprAccess e f

parseErrorPropPostfix :: Parser (Expression -> Expression)
parseErrorPropPostfix = do
  _ <- expect T.OpErrorProp
  pure $ \e -> ExprUnary PropagateError e

parseIncPostfix :: Parser (Expression -> Expression)
parseIncPostfix = do
  _ <- expect T.OpInc
  pure $ \e -> ExprUnary PostfixInc e

parseDecPostfix :: Parser (Expression -> Expression)
parseDecPostfix = do
  _ <- expect T.OpDec
  pure $ \e -> ExprUnary PostfixDec e

getExprName :: Expression -> String
getExprName (ExprVar name) = name
getExprName (ExprAccess _ field) = field
getExprName _ = ""

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
      ExprLitNull <$ expect T.LitNull,
      parseStructInitOrVar,
      between (expect T.LParen) (expect T.RParen) (withContext "parenthesized expression" parseExpression)
    ]
    <|> failParse "Expected expression (literal, variable, or '('...)"

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

parseChar :: Parser Expression
parseChar =
  tokenMap $ \case
    T.LitChar c -> Just (ExprLitChar c)
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

--
-- struct
--

parseStructInitOrVar :: Parser Expression
parseStructInitOrVar = try parseStructInit <|> (ExprVar <$> parseIdentifier)

parseStructInit :: Parser Expression
parseStructInit = do
  name <- parseIdentifier
  fields <- between (expect T.LBrace) (expect T.RBrace) parseStructFields
  pure $ ExprStructInit name fields

parseStructFields :: Parser [(String, Expression)]
parseStructFields = sepEndBy parseStructField (expect T.Comma)

parseStructField :: Parser (String, Expression)
parseStructField = do
  name <- parseIdentifier
  _ <- expect T.Colon
  val <- withContext ("value of field '" ++ name ++ "'") parseExpression
  pure (name, val)
