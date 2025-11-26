module Rune.AST.Parser.ParseVariable
  ( parseVariable,
  )
where

import Control.Applicative (optional, (<|>))
import Rune.AST.Nodes (BinaryOp (..), Expression (..), Statement (..))
import Rune.AST.Parser.ParseExpression (parseExpression)
import Rune.AST.Parser.ParseTypes (parseIdentifier, parseType)
import Rune.AST.ParserHelper (chainPostfix, check, choice, expect, failParse, match, peek, try, withContext)
import Rune.AST.Types (Parser (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseVariable :: Parser Statement
parseVariable = do
  isDecl <- lookAheadIsVarDecl
  isAssignAttempt <- lookAheadAssignmentOp
  if isDecl
    then withContext "Variable declaration" parseVarDecl
    else
      if isAssignAttempt
        then withContext "Assignment statement" parseAssignment
        else withContext "Expression statement" parseExprStmt

--
-- private helpers
--

lookAheadAssignmentOp :: Parser Bool
lookAheadAssignmentOp = Parser $ \s ->
  let p = do
        _ <- parseExpression
        t <- peek
        pure $
          T.tokenKind t
            `elem` [T.OpAssign, T.OpAddAssign, T.OpSubAssign, T.OpMulAssign, T.OpDivAssign, T.OpModAssign]
   in case runParser (try p) s of
        Right (result, _) -> Right (result, s)
        Left _ -> Right (False, s)

lookAheadIsVarDecl :: Parser Bool
lookAheadIsVarDecl = Parser $ \s ->
  let p = do
        _ <- parseIdentifier
        t <- peek
        pure $ T.tokenKind t == T.Colon || T.tokenKind t == T.OpAssign || T.tokenKind t == T.Semicolon
   in case runParser (try p) s of
        Right (result, _) -> Right (result, s)
        Left _ -> Right (False, s)

--
-- private parsers
--

parseLValue :: Parser Expression
parseLValue = parseAccessOrVar

parseAccessOrVar :: Parser Expression
parseAccessOrVar = chainPostfix parseBase op
  where
    parseBase =
      (ExprVar <$> parseIdentifier)
        <|> failParse "Expected L-Value (variable or field access)"
    op = do
      f <- expect T.Dot *> parseIdentifier
      pure $ \e -> ExprAccess e f

parseAssignment :: Parser Statement
parseAssignment = do
  lvalue <- parseLValue
  opTok <- choice (map expect [T.OpAssign, T.OpAddAssign, T.OpSubAssign, T.OpMulAssign, T.OpDivAssign, T.OpModAssign])
  val <- withContext "RHS of assignment" parseExpression
  _ <- expect T.Semicolon
  let finalExpr = case T.tokenKind opTok of
        T.OpAssign -> val
        T.OpAddAssign -> ExprBinary Add lvalue val
        T.OpSubAssign -> ExprBinary Sub lvalue val
        T.OpMulAssign -> ExprBinary Mul lvalue val
        T.OpDivAssign -> ExprBinary Div lvalue val
        T.OpModAssign -> ExprBinary Mod lvalue val
        _ -> error "Impossible assignment operator"
  pure $ StmtAssignment lvalue finalExpr

parseVarDecl :: Parser Statement
parseVarDecl = do
  name <- parseIdentifier
  typeAnnot <- optional (expect T.Colon *> parseType)
  isAssign <- match T.OpAssign
  val <-
    if isAssign
      then withContext "assigned value" parseExpression
      else case typeAnnot of
        Just _ -> pure ExprLitNull
        Nothing -> failParse "Expected type annotation or initial value for variable declaration"

  _ <- expect T.Semicolon
  pure $ StmtVarDecl name typeAnnot val

parseExprStmt :: Parser Statement
parseExprStmt = do
  expr <- parseExpression
  isSemicolon <- match T.Semicolon
  if isSemicolon
    then pure (StmtExpr expr)
    else do
      isEnd <- check T.RBrace
      if isEnd
        then pure (StmtReturn (Just expr))
        else failParse "Expected ';' after expression or block-ending '}' for implicit return"
