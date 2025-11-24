{-# LANGUAGE LambdaCase #-}

module Rune.AST.Parser (parseRune) where

import Control.Monad (void)
import Rune.AST.Nodes
import Rune.AST.ParserHelper (advance, check, expect, getCurrentToken, makeError, match)
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

parseProgram :: Parser Program
parseProgram = do
  defs <- many parseTopLevelDef
  void $ expect T.EOF
  fp <- Parser $ \s -> Right (psFilePath s, s)
  return $ Program fp defs

--
-- Top-Level Definitions
--

parseTopLevelDef :: Parser TopLevelDef
parseTopLevelDef = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.KwDef -> parseFunction
    T.KwStruct -> parseStruct
    T.KwOverride -> parseOverride
    T.EOF -> makeError "Unexpected end of file"
    _ -> makeError $ "Expected top-level definition (def, struct, override)"

--
-- Function Definition
--

parseFunction :: Parser TopLevelDef
parseFunction = do
  void $ expect T.KwDef
  name <- parseIdentifier
  void $ expect T.LParen
  params <- parseParameters
  void $ expect T.RParen
  retType <- parseReturnType
  body <- parseBlock
  return $ DefFunction name params retType body

parseParameters :: Parser [Parameter]
parseParameters = do
  isRParen <- check T.RParen
  if isRParen
    then return []
    else do
      first <- parseParameter
      rest <- many (expect T.Comma >> parseParameter)
      return (first : rest)

parseParameter :: Parser Parameter
parseParameter = do
  name <- parseIdentifier
  void $ expect T.Colon
  typ <- parseType
  return $ Parameter name typ

parseReturnType :: Parser Type
parseReturnType = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.OpArrow -> advance >> parseType
    T.OpSquigArrow -> advance >> parseType
    _ -> makeError "Expected -> or ~> for return type"

--
-- Struct Definition
--

parseStruct :: Parser TopLevelDef
parseStruct = do
  void $ expect T.KwStruct
  name <- parseIdentifier
  void $ expect T.LBrace
  (fields, methods) <- parseStructBody
  void $ expect T.RBrace
  return $ DefStruct name fields methods

parseStructBody :: Parser ([Field], [TopLevelDef])
parseStructBody = do
  isRBrace <- check T.RBrace
  if isRBrace
    then return ([], [])
    else parseStructMembers [] []

parseStructMembers :: [Field] -> [TopLevelDef] -> Parser ([Field], [TopLevelDef])
parseStructMembers fields methods = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.RBrace -> return (reverse fields, reverse methods)
    T.KwDef -> do
      method <- parseFunction
      parseStructMembers fields (method : methods)
    T.Identifier _ -> do
      field <- parseField
      void $ expect T.Semicolon
      parseStructMembers (field : fields) methods
    _ -> makeError "Expected field or method definition in struct"

parseField :: Parser Field
parseField = do
  name <- parseIdentifier
  void $ expect T.Colon
  typ <- parseType
  return $ Field name typ

--
-- Override Definition
--

parseOverride :: Parser TopLevelDef
parseOverride = do
  void $ expect T.KwOverride
  void $ expect T.KwDef
  name <- parseIdentifier
  void $ expect T.LParen
  params <- parseParameters
  void $ expect T.RParen
  retType <- parseReturnType
  body <- parseBlock
  return $ DefOverride name params retType body

--
-- Block and Statements
--

parseBlock :: Parser Block
parseBlock = do
  void $ expect T.LBrace
  stmts <- many parseStatement
  void $ expect T.RBrace
  return stmts

parseStatement :: Parser Statement
parseStatement = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.KwReturn -> parseReturn
    T.KwIf -> parseIf
    T.KwFor -> parseFor
    T.Identifier _ -> parseVarDeclOrExprStmt
    _ -> parseExprStatement

parseReturn :: Parser Statement
parseReturn = do
  void $ expect T.KwReturn
  hasSemicolon <- check T.Semicolon
  if hasSemicolon
    then do
      void advance
      return $ StmtReturn Nothing
    else do
      expr <- parseExpression
      void $ expect T.Semicolon
      return $ StmtReturn (Just expr)

parseIf :: Parser Statement
parseIf = do
  void $ expect T.KwIf
  cond <- parseExpression
  thenBlock <- parseBlock
  hasElse <- match T.KwElse
  elseBlock <-
    if hasElse
      then do
        hasIf <- check T.KwIf
        if hasIf
          then do
            ifStmt <- parseIf
            return $ Just [ifStmt]
          else Just <$> parseBlock
      else return Nothing
  return $ StmtIf cond thenBlock elseBlock

parseFor :: Parser Statement
parseFor = do
  void $ expect T.KwFor
  var <- parseIdentifier
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.OpAssign -> parseForRange var
    T.KwIn -> parseForEach var
    _ -> makeError "Expected '=' or 'in' after for variable"

parseForRange :: String -> Parser Statement
parseForRange var = do
  void $ expect T.OpAssign
  start <- parseExpression
  void $ expect T.KwTo
  end <- parseExpression
  body <- parseBlock
  return $ StmtFor var start end body

parseForEach :: String -> Parser Statement
parseForEach var = do
  void $ expect T.KwIn
  iterable <- parseExpression
  body <- parseBlock
  return $ StmtForEach var iterable body

parseVarDeclOrExprStmt :: Parser Statement
parseVarDeclOrExprStmt = do
  name <- parseIdentifier
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.Colon -> parseVarDeclWithType name
    T.OpAssign -> parseVarDeclInferred name
    _ -> do
      expr <- parseExpressionFromVar (ExprVar name)
      expectSemicolonOrImplicitReturn expr

parseExpressionFromVar :: Expression -> Parser Expression
parseExpressionFromVar initial = do
  e1 <- parseExpressionContinue initial
  e2 <- parseFactorContinue e1
  e3 <- parseTermContinue e2
  e4 <- parseComparisonContinue e3
  e5 <- parseEqualityContinue e4
  e6 <- parseLogicalAndContinue e5
  parseLogicalOrContinue e6

parseVarDeclWithType :: String -> Parser Statement
parseVarDeclWithType name = do
  void $ expect T.Colon
  typ <- parseType
  void $ expect T.OpAssign
  expr <- parseExpression
  void $ expect T.Semicolon
  return $ StmtVarDecl name (Just typ) expr

parseVarDeclInferred :: String -> Parser Statement
parseVarDeclInferred name = do
  void $ expect T.OpAssign
  expr <- parseExpression
  void $ expect T.Semicolon
  return $ StmtVarDecl name Nothing expr

parseExprStatement :: Parser Statement
parseExprStatement = do
  expr <- parseExpression
  expectSemicolonOrImplicitReturn expr

expectSemicolonOrImplicitReturn :: Expression -> Parser Statement
expectSemicolonOrImplicitReturn expr = do
  isSemi <- match T.Semicolon
  if isSemi
    then return $ StmtExpr expr
    else do
      isBlockEnd <- check T.RBrace
      if isBlockEnd
        then return $ StmtReturn (Just expr)
        else makeError "Expected ';' after expression"

--
-- Expressions
--

parseExpression :: Parser Expression
parseExpression = parseLogicalOr

parseLogicalOr :: Parser Expression
parseLogicalOr = do
  left <- parseLogicalAnd
  parseLogicalOrContinue left

parseLogicalOrContinue :: Expression -> Parser Expression
parseLogicalOrContinue left = do
  isOr <- match T.OpOr
  if isOr
    then do
      right <- parseLogicalAnd
      parseLogicalOrContinue (ExprBinary Or left right)
    else return left

parseLogicalAnd :: Parser Expression
parseLogicalAnd = do
  left <- parseEquality
  parseLogicalAndContinue left

parseLogicalAndContinue :: Expression -> Parser Expression
parseLogicalAndContinue left = do
  isAnd <- match T.OpAnd
  if isAnd
    then do
      right <- parseEquality
      parseLogicalAndContinue (ExprBinary And left right)
    else return left

parseEquality :: Parser Expression
parseEquality = do
  left <- parseComparison
  parseEqualityContinue left

parseEqualityContinue :: Expression -> Parser Expression
parseEqualityContinue left = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.OpEq -> do
      void advance
      right <- parseComparison
      parseEqualityContinue (ExprBinary Eq left right)
    T.OpNeq -> do
      void advance
      right <- parseComparison
      parseEqualityContinue (ExprBinary Neq left right)
    _ -> return left

parseComparison :: Parser Expression
parseComparison = do
  left <- parseTerm
  parseComparisonContinue left

parseComparisonContinue :: Expression -> Parser Expression
parseComparisonContinue left = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.OpLt -> do
      void advance
      right <- parseTerm
      parseComparisonContinue (ExprBinary Lt left right)
    T.OpLte -> do
      void advance
      right <- parseTerm
      parseComparisonContinue (ExprBinary Lte left right)
    T.OpGt -> do
      void advance
      right <- parseTerm
      parseComparisonContinue (ExprBinary Gt left right)
    T.OpGte -> do
      void advance
      right <- parseTerm
      parseComparisonContinue (ExprBinary Gte left right)
    _ -> return left

parseTerm :: Parser Expression
parseTerm = do
  left <- parseFactor
  parseTermContinue left

parseTermContinue :: Expression -> Parser Expression
parseTermContinue left = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.OpPlus -> do
      void advance
      right <- parseFactor
      parseTermContinue (ExprBinary Add left right)
    T.OpMinus -> do
      void advance
      right <- parseFactor
      parseTermContinue (ExprBinary Sub left right)
    _ -> return left

parseFactor :: Parser Expression
parseFactor = do
  left <- parseUnary
  parseFactorContinue left

parseFactorContinue :: Expression -> Parser Expression
parseFactorContinue left = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.OpMul -> do
      void advance
      right <- parseUnary
      parseFactorContinue (ExprBinary Mul left right)
    T.OpDiv -> do
      void advance
      right <- parseUnary
      parseFactorContinue (ExprBinary Div left right)
    T.OpMod -> do
      void advance
      right <- parseUnary
      parseFactorContinue (ExprBinary Mod left right)
    _ -> return left

parseUnary :: Parser Expression
parseUnary = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.OpMinus -> do
      void advance
      expr <- parseUnary
      return $ ExprUnary Negate expr
    _ -> parsePostfix

parsePostfix :: Parser Expression
parsePostfix = do
  expr <- parsePrimary
  parseExpressionContinue expr

parseExpressionContinue :: Expression -> Parser Expression
parseExpressionContinue expr = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.LParen -> do
      args <- parseCallArgs
      parseExpressionContinue (ExprCall (getExprName expr) args)
    T.Dot -> do
      void advance
      field <- parseIdentifier
      parseExpressionContinue (ExprAccess expr field)
    T.OpErrorProp -> do
      void advance
      parseExpressionContinue (ExprUnary PropagateError expr)
    _ -> return expr

getExprName :: Expression -> String
getExprName (ExprVar name) = name
getExprName (ExprAccess _ field) = field
getExprName _ = ""

parseCallArgs :: Parser [Expression]
parseCallArgs = do
  void $ expect T.LParen
  isRParen <- check T.RParen
  if isRParen
    then do
      void advance
      return []
    else do
      first <- parseExpression
      rest <- many (expect T.Comma >> parseExpression)
      void $ expect T.RParen
      return (first : rest)

parsePrimary :: Parser Expression
parsePrimary = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.LitInt n -> advance >> return (ExprLitInt n)
    T.LitFloat f -> advance >> return (ExprLitFloat f)
    T.LitString s -> advance >> return (ExprLitString s)
    T.LitBool b -> advance >> return (ExprLitBool b)
    T.LitNull -> advance >> return ExprLitNull
    T.Identifier name -> do
      void advance
      next <- getCurrentToken
      case T.tokenKind next of
        T.LBrace -> do
          res <- tryParse (parseStructInit name)
          case res of
            Just expr -> return expr
            Nothing -> return $ ExprVar name
        _ -> return $ ExprVar name
    T.LParen -> do
      void advance
      expr <- parseExpression
      void $ expect T.RParen
      return expr
    _ -> makeError $ "Unexpected token in expression: " ++ show (T.tokenKind tok)

parseStructInit :: String -> Parser Expression
parseStructInit name = do
  void $ expect T.LBrace
  fields <- parseStructInitFields
  void $ expect T.RBrace
  return $ ExprStructInit name fields

parseStructInitFields :: Parser [(String, Expression)]
parseStructInitFields = do
  isRBrace <- check T.RBrace
  if isRBrace
    then return []
    else do
      first <- parseStructInitField
      hasComma <- match T.Comma
      if hasComma
        then do
          rest <- parseStructInitFields
          return (first : rest)
        else return [first]

parseStructInitField :: Parser (String, Expression)
parseStructInitField = do
  name <- parseIdentifier
  void $ expect T.Colon
  expr <- parseExpression
  return (name, expr)

--
-- Types
--

parseType :: Parser Type
parseType = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.TypeI8 -> advance >> return TypeI8
    T.TypeI16 -> advance >> return TypeI16
    T.TypeI32 -> advance >> return TypeI32
    T.TypeI64 -> advance >> return TypeI64
    T.TypeF32 -> advance >> return TypeF32
    T.TypeF64 -> advance >> return TypeF64
    T.TypeBool -> advance >> return TypeBool
    T.TypeU8 -> advance >> return TypeU8
    T.TypeU16 -> advance >> return TypeU16
    T.TypeU32 -> advance >> return TypeU32
    T.TypeString -> advance >> return TypeString
    T.TypeAny -> advance >> return TypeAny
    T.TypeNull -> advance >> return TypeNull
    T.Identifier name -> advance >> return (TypeCustom name)
    _ -> makeError $ "Expected type, got " ++ show (T.tokenKind tok)

--
-- Identifiers
--

parseIdentifier :: Parser String
parseIdentifier = do
  tok <- getCurrentToken
  case T.tokenKind tok of
    T.Identifier name -> advance >> return name
    _ -> makeError "Expected identifier"

--
-- Utility Combinators
--

many :: Parser a -> Parser [a]
many p = do
  tok <- getCurrentToken
  if T.tokenKind tok == T.EOF || T.tokenKind tok == T.RBrace
    then return []
    else do
      result <- tryParse p
      case result of
        Just x -> do
          xs <- many p
          return (x : xs)
        Nothing -> return []

tryParse :: Parser a -> Parser (Maybe a)
tryParse p = Parser $ \s -> case runParser p s of
  Left _ -> Right (Nothing, s)
  Right (x, s') -> Right (Just x, s')
