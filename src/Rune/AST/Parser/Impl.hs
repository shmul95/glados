{-# LANGUAGE OverloadedStrings #-}

module Rune.AST.Parser.Impl
  ( parseType,
    parseExpression,
    parseStatement,
    parseBlock,
    parseParameters,
    parseRetArrow,
    parseField,
    parseStructBody,
    parseFunction,
    parseOverride,
  )
where

import Control.Monad (void)
import Data.Either (lefts, rights)
import Rune.AST.Nodes
import Rune.AST.Parser.Helpers
import qualified Rune.Lexer.Tokens as L
import Text.Megaparsec hiding (Token, match)

--
-- Types
--

parseType :: Parser Type
parseType =
  choice
    [ TypeI8 <$ match L.TypeI8,
      TypeI16 <$ match L.TypeI16,
      TypeI32 <$ match L.TypeI32,
      TypeI64 <$ match L.TypeI64,
      TypeF32 <$ match L.TypeF32,
      TypeF64 <$ match L.TypeF64,
      TypeBool <$ match L.TypeBool,
      TypeU8 <$ match L.TypeU8,
      TypeU16 <$ match L.TypeU16,
      TypeU32 <$ match L.TypeU32,
      TypeString <$ match L.TypeString,
      TypeAny <$ match L.TypeAny,
      TypeNull <$ match L.TypeNull,
      TypeCustom <$> identifier
    ]

--
-- Expressions
--

parseExpression :: Parser Expression
parseExpression = parseOr

parseOr :: Parser Expression
parseOr = parseAnd `chainl1` (ExprBinary Or <$ match L.OpOr)

parseAnd :: Parser Expression
parseAnd = parseEquality `chainl1` (ExprBinary And <$ match L.OpAnd)

parseEquality :: Parser Expression
parseEquality = parseComparison `chainl1` ops
  where
    ops = (ExprBinary Eq <$ match L.OpEq) <|> (ExprBinary Neq <$ match L.OpNeq)

parseComparison :: Parser Expression
parseComparison = parseTerm `chainl1` ops
  where
    ops =
      choice
        [ ExprBinary Lt <$ match L.OpLt,
          ExprBinary Lte <$ match L.OpLte,
          ExprBinary Gt <$ match L.OpGt,
          ExprBinary Gte <$ match L.OpGte
        ]

parseTerm :: Parser Expression
parseTerm = parseFactor `chainl1` ops
  where
    ops = (ExprBinary Add <$ match L.OpPlus) <|> (ExprBinary Sub <$ match L.OpMinus)

parseFactor :: Parser Expression
parseFactor = parseUnary `chainl1` ops
  where
    ops =
      choice
        [ ExprBinary Mul <$ match L.OpMul,
          ExprBinary Div <$ match L.OpDiv,
          ExprBinary Mod <$ match L.OpMod
        ]

parseUnary :: Parser Expression
parseUnary =
  choice
    [ ExprUnary Negate <$> (match L.OpMinus *> parseUnary),
      parsePostfix
    ]

parsePostfix :: Parser Expression
parsePostfix = parsePrimary >>= postfixLoop
  where
    postfixLoop base =
      choice
        [ do
            args <- parens (commaSep parseExpression)
            let call = case base of
                  ExprVar name -> ExprCall name args
                  _ -> error "Semantic limitation: Can only call named identifiers"
            postfixLoop call,
          do
            void $ match L.Dot
            field <- identifier
            postfixLoop (ExprAccess base field),
          do
            void $ match L.OpErrorProp
            postfixLoop (ExprUnary PropagateError base),
          return base
        ]

parsePrimary :: Parser Expression
parsePrimary =
  choice
    [ parseLiteral,
      try parseStructInit,
      parseIdentifier,
      parens parseExpression
    ]

parseLiteral :: Parser Expression
parseLiteral =
  choice
    [ ExprLitInt . read . L.tokenValue <$> satisfyToken isInt,
      ExprLitFloat . read . L.tokenValue <$> satisfyToken isFloat,
      ExprLitString . read . L.tokenValue <$> satisfyToken isString,
      ExprLitBool . parseBool <$> satisfyToken isBool,
      ExprLitNull <$ satisfyToken isNull
    ]
  where
    isInt t = case L.tokenKind t of L.LitInt _ -> True; _ -> False
    isFloat t = case L.tokenKind t of L.LitFloat _ -> True; _ -> False
    isString t = case L.tokenKind t of L.LitString _ -> True; _ -> False
    isBool t = case L.tokenKind t of L.LitBool _ -> True; _ -> False
    isNull t = case L.tokenKind t of L.LitNull -> True; _ -> False

    parseBool :: L.Token -> Bool
    parseBool t = L.tokenValue t == "true"

parseStructInit :: Parser Expression
parseStructInit = do
  name <- identifier
  fields <- braces (commaSep parseInitField)
  return $ ExprStructInit name fields
  where
    parseInitField = do
      fName <- identifier
      void $ match L.Colon
      expr <- parseExpression
      return (fName, expr)

parseIdentifier :: Parser Expression
parseIdentifier = ExprVar <$> identifier

--
-- Statements
--

parseStatement :: Parser Statement
parseStatement =
  choice
    [ parseReturn,
      parseIf,
      parseFor,
      parseVarDeclOrExpr
    ]

parseReturn :: Parser Statement
parseReturn = do
  void $ match L.KwReturn
  expr <- optional parseExpression
  void semicolon
  return $ StmtReturn expr

parseIf :: Parser Statement
parseIf = do
  void $ match L.KwIf
  cond <- parseExpression
  thenB <- parseBlock
  elseB <- optional (match L.KwElse *> parseBlock)
  return $ StmtIf cond thenB elseB

parseFor :: Parser Statement
parseFor = do
  void $ match L.KwFor
  var <- identifier
  choice
    [ do
        void $ match L.OpAssign
        start <- parseExpression
        void $ match L.KwTo
        end <- parseExpression
        StmtFor var start end <$> parseBlock,
      do
        void $ match L.KwIn
        iterable <- parseExpression
        StmtForEach var iterable <$> parseBlock
    ]

parseVarDeclOrExpr :: Parser Statement
parseVarDeclOrExpr = try parseExplicitDecl <|> try parseInferredDecl <|> parseExprStmt

parseExplicitDecl :: Parser Statement
parseExplicitDecl = do
  name <- identifier
  void $ match L.Colon
  t <- parseType
  void $ match L.OpAssign
  val <- parseExpression
  void semicolon
  return $ StmtVarDecl name (Just t) val

parseInferredDecl :: Parser Statement
parseInferredDecl = do
  name <- identifier
  void $ match L.OpAssign
  val <- parseExpression
  void semicolon
  return $ StmtVarDecl name Nothing val

parseExprStmt :: Parser Statement
parseExprStmt = do
  expr <- parseExpression
  void $ optional semicolon
  return $ StmtExpr expr

--
-- Blocks & Parameters
--

parseBlock :: Parser Block
parseBlock = braces (many parseStatement)

parseParameters :: Parser [Parameter]
parseParameters = commaSep parseParameter

parseParameter :: Parser Parameter
parseParameter = do
  name <- identifier
  t <-
    if name == "self"
      then pure TypeAny
      else match L.Colon *> parseType
  return $ Parameter name t

parseRetArrow :: Parser L.TokenKind
parseRetArrow = L.tokenKind <$> satisfyToken (\t -> L.tokenKind t == L.OpArrow || L.tokenKind t == L.OpSquigArrow)

parseField :: Parser Field
parseField = do
  name <- identifier
  void $ match L.Colon
  t <- parseType
  void semicolon
  return $ Field name t

--
-- Top-Level (needed here to avoid circular imports bc haskell doesnt support forward declarations)
--

parseFunction :: Parser TopLevelDef
parseFunction = do
  void $ match L.KwDef
  name <- identifier
  params <- parens parseParameters
  _arrow <- parseRetArrow
  retType <- parseType
  DefFunction name params retType <$> parseBlock

parseOverride :: Parser TopLevelDef
parseOverride = do
  void $ match L.KwOverride
  void $ match L.KwDef
  name <- identifier
  params <- parens parseParameters
  void parseRetArrow
  retType <- parseType
  DefOverride name params retType <$> parseBlock

parseStructBody :: Parser ([Field], [TopLevelDef])
parseStructBody = do
  items <-
    many $
      choice
        [ Left <$> try parseField,
          Right <$> parseFunction
        ]
  let fields = lefts items
  let methods = rights items
  return (fields, methods)
