module Rune.AST.Parser.ParseBlock
  ( parseBlock,
  )
where

import Control.Applicative (optional, (<|>))
import Control.Monad (unless)
import Rune.AST.Nodes (Block, Statement (..), Type (..))
import Rune.AST.Parser.ParseExpression (parseExpression)
import Rune.AST.Parser.ParseTypes (parseIdentifier, parseType)
import Rune.AST.Parser.ParseVariable (parseVariable)
import Rune.AST.ParserHelper (advance, check, checkLoopDepth, expect, failParse, incLoopDepth, peek, withContext)
import Rune.AST.Types (Parser (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseBlock :: Parser Block
parseBlock = expect T.LBrace *> parseBlockLoop

--
-- private parsers
--

--
-- blocks & statements
--

parseBlockLoop :: Parser Block
parseBlockLoop = do
  isEnd <- check T.RBrace
  if isEnd
    then advance >> pure []
    else do
      stmt <- parseStatement
      stmts <- parseBlockLoop
      pure (stmt : stmts)

parseStatement :: Parser Statement
parseStatement = do
  t <- peek
  case T.tokenKind t of
    T.KwReturn -> withContext "Return statement" parseReturn
    T.KwIf -> withContext "If statement" parseIf
    T.KwFor -> withContext "For loop" parseFor
    T.KwLoop -> withContext "Loop statement" parseLoop
    T.KwStop -> withContext "Stop statement" parseStop
    T.KwNext -> withContext "Next statement" parseNext
    _ -> parseVariable

--
-- return
--

parseReturn :: Parser Statement
parseReturn =
  expect T.KwReturn
    *> (StmtReturn <$> optional (withContext "return value" parseExpression))
    <* expect T.Semicolon

--
-- conditionals
--

parseIf :: Parser Statement
parseIf =
  StmtIf
    <$> (expect T.KwIf *> withContext "if condition" parseExpression)
    <*> withContext "if block" parseBlock
    <*> optional (expect T.KwElse *> (withContext "else block" (parseBlock <|> (pure <$> parseIf))))

--
-- loops
--

parseFor :: Parser Statement
parseFor = do
  _ <- expect T.KwFor
  name <- parseIdentifier
  typeAnnot <- optional (expect T.Colon *> parseType)

  t <- peek
  case T.tokenKind t of
    T.OpAssign -> parseForRangeRest name typeAnnot
    T.KwTo -> parseForRangeRestNoInit name typeAnnot
    T.KwIn -> parseForEachRest name typeAnnot
    _ -> failParse "Expected '=' or 'to' or 'in' after for loop variable"

parseForRangeRest :: String -> Maybe Type -> Parser Statement
parseForRangeRest var typeAnnot = do
  _ <- expect T.OpAssign
  start <- withContext "start index" parseExpression
  _ <- expect T.KwTo
  end <- withContext "end index" parseExpression
  body <- incLoopDepth (withContext "for block" parseBlock)
  pure $ StmtFor var typeAnnot (Just start) end body

parseForRangeRestNoInit :: String -> Maybe Type -> Parser Statement
parseForRangeRestNoInit var typeAnnot = do
  _ <- expect T.KwTo
  end <- withContext "end index" parseExpression
  body <- incLoopDepth (withContext "for block" parseBlock)
  pure $ StmtFor var typeAnnot Nothing end body

parseForEachRest :: String -> Maybe Type -> Parser Statement
parseForEachRest var typeAnnot = do
  _ <- expect T.KwIn
  iterable <- withContext "iterable expression" parseExpression
  body <- incLoopDepth (withContext "for-each block" parseBlock)
  pure $ StmtForEach var typeAnnot iterable body

parseLoop :: Parser Statement
parseLoop =
  StmtLoop <$> (expect T.KwLoop *> incLoopDepth (withContext "loop block" parseBlock))

--
-- control flow
--

parseStop :: Parser Statement
parseStop = do
  inLoop <- checkLoopDepth
  unless inLoop $ failParse "The 'stop' statement can only be used inside a loop or for block"
  expect T.KwStop *> expect T.Semicolon *> pure StmtStop

parseNext :: Parser Statement
parseNext = do
  inLoop <- checkLoopDepth
  unless inLoop $ failParse "The 'next' statement can only be used inside a loop or for block"
  expect T.KwNext *> expect T.Semicolon *> pure StmtNext
