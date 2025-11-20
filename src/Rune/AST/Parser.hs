{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Rune.AST.Parser (parseRune) where

import Data.Void (Void)
import Text.Megaparsec hiding (Token, match)
import Rune.AST.Nodes
import qualified Rune.Lexer.Tokens as L
import Control.Monad (void)
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)

--
-- Type Definitions
--

type Parser = Parsec Void [L.Token]

--
-- Public API
--

parseRune :: FilePath -> [L.Token] -> Either String Program
parseRune filename inputTokens =
  case runParser programParser filename inputTokens of
    Left err -> Left $ formatError inputTokens err
    Right prog -> Right prog

--
-- Error Formatting
--

formatError :: [L.Token] -> ParseErrorBundle [L.Token] Void -> String
formatError inputTokens bundle =
    unlines $ map (formatParseError inputTokens) (NE.toList $ bundleErrors bundle)

formatParseError :: [L.Token] -> ParseError [L.Token] Void -> String
formatParseError inputTokens err =
    let offset = errorOffset err
        maybeTok = if offset < length inputTokens
                   then Just (inputTokens !! offset)
                   else Nothing
        
        (line, col) = case maybeTok of
            Just t -> (L.tokenLine t, L.tokenColumn t)
            Nothing -> case NE.last (L.Token L.EOF "" 0 0 NE.:| inputTokens) of
                lastTok -> (L.tokenLine lastTok, L.tokenColumn lastTok)

        unexpectedMsg = case err of
            TrivialError _ (Just (Tokens (t NE.:| _))) _ -> "Unexpected '" ++ L.tokenValue t ++ "'"
            TrivialError _ (Just (Label l)) _ -> "Unexpected label: " ++ toList l
            TrivialError _ (Just EndOfInput) _ -> "Unexpected end of input"
            TrivialError _ Nothing _ -> "Unexpected end of input"
            FancyError _ _ -> "Unknown error"

        expectedMsg = case err of
            TrivialError _ _ expectedSet -> 
                let items = toList expectedSet
                in if null items 
                   then "" 
                   else ", expected " ++ showExpected items
            _ -> ""

        showExpected [] = ""
        showExpected items = unwords (map showItem items)
        
        showItem (Tokens (t NE.:| _)) = "'" ++ L.tokenValue t ++ "'"
        showItem (Label l) = toList l
        showItem EndOfInput = "end of input"

    in "Error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ unexpectedMsg ++ expectedMsg

--
-- Parser Combinators & Helpers
--

satisfyToken :: (L.Token -> Bool) -> Parser L.Token
satisfyToken predicate = try $ do
  t <- anySingle
  if predicate t then return t else empty

match :: L.TokenKind -> Parser L.Token
match kind = satisfyToken (\t -> L.tokenKind t == kind)

identifier :: Parser String
identifier = try $ do
  t <- anySingle
  case L.tokenKind t of
    L.Identifier s -> return s
    _ -> empty

parens :: Parser a -> Parser a
parens = between (match L.LParen) (match L.RParen)

braces :: Parser a -> Parser a
braces = between (match L.LBrace) (match L.RBrace)

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (match L.Comma)

semicolon :: Parser L.Token
semicolon = match L.Semicolon

--
-- Top Level Definitions
--

programParser :: Parser Program
programParser = Program <$> many topLevelDef <* match L.EOF

topLevelDef :: Parser TopLevelDef
topLevelDef = choice
  [ parseOverride
  , parseStruct
  , parseFunction
  ]

parseFunction :: Parser TopLevelDef
parseFunction = do
   void $ match L.KwDef
   name <- identifier
   params <- parens parseParameters
   _arrow <- parseRetArrow
   retType <- parseType
   DefFunction name params retType <$> parseBlock

parseStruct :: Parser TopLevelDef
parseStruct = do
  void $ match L.KwStruct
  name <- identifier
  (fields, methods) <- braces parseStructBody
  return $ DefStruct name fields methods

parseStructBody :: Parser ([Field], [TopLevelDef])
parseStructBody = do
  items <- many $ choice
    [ Left <$> try parseField
    , Right <$> parseFunction
    ]
  let fields = [f | Left f <- items]
  let methods = [m | Right m <- items]
  return (fields, methods)

parseField :: Parser Field
parseField = do
  name <- identifier
  void $ match L.Colon
  t <- parseType
  void $ semicolon
  return $ Field name t

parseOverride :: Parser TopLevelDef
parseOverride = do
  void $ match L.KwOverride
  void $ match L.KwDef
  name <- identifier
  params <- parens parseParameters
  void $ parseRetArrow
  retType <- parseType
  body <- parseBlock
  return $ DefOverride name params retType body

parseParameters :: Parser [Parameter]
parseParameters = commaSep parseParameter

parseParameter :: Parser Parameter
parseParameter = do
  name <- identifier
  t <- if name == "self"
    then pure TypeAny
    else match L.Colon *> parseType
  return $ Parameter name t

parseRetArrow :: Parser L.TokenKind
parseRetArrow = L.tokenKind <$> satisfyToken (\t -> L.tokenKind t == L.OpArrow || L.tokenKind t == L.OpSquigArrow)

parseBlock :: Parser Block
parseBlock = braces (many parseStatement)

--
-- Statements
--

parseStatement :: Parser Statement
parseStatement = choice
  [ parseReturn
  , parseIf
  , parseFor
  , parseVarDeclOrExpr
  ]

parseReturn :: Parser Statement
parseReturn = do
  void $ match L.KwReturn
  expr <- optional parseExpression
  void $ semicolon
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
        body <- parseBlock
        return $ StmtFor var start end body
    , do
        void $ match L.KwIn
        iterable <- parseExpression
        body <- parseBlock
        return $ StmtForEach var iterable body
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
  void $ semicolon
  return $ StmtVarDecl name (Just t) val

parseInferredDecl :: Parser Statement
parseInferredDecl = do
  name <- identifier
  void $ match L.OpAssign
  val <- parseExpression
  void $ semicolon
  return $ StmtVarDecl name Nothing val

parseExprStmt :: Parser Statement
parseExprStmt = do
  expr <- parseExpression
  void $ optional semicolon
  return $ StmtExpr expr

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
    ops = choice
      [ ExprBinary Lt  <$ match L.OpLt
      , ExprBinary Lte <$ match L.OpLte
      , ExprBinary Gt  <$ match L.OpGt
      , ExprBinary Gte <$ match L.OpGte
      ]

parseTerm :: Parser Expression
parseTerm = parseFactor `chainl1` ops
  where
    ops = (ExprBinary Add <$ match L.OpPlus) <|> (ExprBinary Sub <$ match L.OpMinus)

parseFactor :: Parser Expression
parseFactor = parseUnary `chainl1` ops
  where
    ops = choice
      [ ExprBinary Mul <$ match L.OpMul
      , ExprBinary Div <$ match L.OpDiv
      , ExprBinary Mod <$ match L.OpMod
      ]

parseUnary :: Parser Expression
parseUnary = choice
  [ ExprUnary Negate <$> (match L.OpMinus *> parseUnary)
  , parsePostfix
  ]

parsePostfix :: Parser Expression
parsePostfix = parsePrimary >>= postfixLoop
  where
    postfixLoop base = choice
      [ do
          args <- parens (commaSep parseExpression)
          let call = case base of
                       ExprVar name -> ExprCall name args
                       _ -> error "Semantic limitation: Can only call named identifiers"
          postfixLoop call
      , do
          void $ match L.Dot
          field <- identifier
          postfixLoop (ExprAccess base field)
      , do
          void $ match L.OpErrorProp
          postfixLoop (ExprUnary PropagateError base)
      , return base
      ]

parsePrimary :: Parser Expression
parsePrimary = choice
  [ parseLiteral
  , try parseStructInit
  , parseIdentifier
  , parens parseExpression
  ]

parseLiteral :: Parser Expression
parseLiteral = choice
  [ ExprLitInt . read . L.tokenValue    <$> satisfyToken isInt
  , ExprLitFloat . read . L.tokenValue  <$> satisfyToken isFloat
  , ExprLitString . read . L.tokenValue <$> satisfyToken isString
  , ExprLitBool . parseBool             <$> satisfyToken isBool
  , ExprLitNull                         <$  satisfyToken isNull
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
-- Types
--

parseType :: Parser Type
parseType = choice
  [ TypeI8     <$ match L.TypeI8
  , TypeI16    <$ match L.TypeI16
  , TypeI32    <$ match L.TypeI32
  , TypeI64    <$ match L.TypeI64
  , TypeF32    <$ match L.TypeF32
  , TypeF64    <$ match L.TypeF64
  , TypeBool   <$ match L.TypeBool
  , TypeU8     <$ match L.TypeU8
  , TypeU16    <$ match L.TypeU16
  , TypeU32    <$ match L.TypeU32
  , TypeString <$ match L.TypeString
  , TypeAny    <$ match L.TypeAny
  , TypeNull   <$ match L.TypeNull
  , TypeCustom <$> identifier
  ]

--
-- Helpers
--

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y))
      <|> return x
