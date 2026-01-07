{-# LANGUAGE CPP #-}

module Rune.AST.Parser.ParseBlock
#if defined(TESTING_EXPORT)
  ( parseBlock,
    parseBlockLoop,
    parseStatement,
    parseReturn,
    parseIf,
    parseFor,
    parseForRangeRest,
    parseForRangeRestNoInit,
    parseForEachRest,
    parseLoop,
    parseStop,
    parseNext
  )
#else
  ( parseBlock,
  )
#endif
where

import Control.Applicative (optional, (<|>))
import Control.Monad (unless)
import Rune.AST.Nodes (Block, Statement (..), Type (..))
import Rune.AST.Parser.ParseExpression (parseExpression)
import Rune.AST.Parser.ParseTypes (parseIdentifier, parseType)
import Rune.AST.Parser.ParseVariable (parseVariable)
import Rune.AST.ParserHelper (advance, check, checkLoopDepth, expect, failParse, getCurrentPos, incLoopDepth, peek, withContext)
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
  if isEnd then
    advance >> pure []
  else do
    stmt <- parseStatement
    stmts <- parseBlockLoop
    pure $ stmt : stmts

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
parseReturn = do
  pos <- getCurrentPos
  _ <- expect T.KwReturn
  expr <- optional (withContext "return value" parseExpression)
  _ <- expect T.Semicolon
  pure $ StmtReturn pos expr

--
-- conditionals
--

parseIf :: Parser Statement
parseIf = do
  pos <- getCurrentPos
  _ <- expect T.KwIf
  cond <- withContext "if condition" parseExpression
  thenBlock <- withContext "if block" parseBlock
  elseBlock <- optional (expect T.KwElse *> withContext "else block" (parseBlock <|> (pure <$> parseIf)))
  pure $ StmtIf pos cond thenBlock elseBlock

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
  pos <- getCurrentPos
  _ <- expect T.OpAssign
  start <- withContext "start index" parseExpression
  _ <- expect T.KwTo
  end <- withContext "end index" parseExpression
  body <- incLoopDepth (withContext "for block" parseBlock)
  pure $ StmtFor pos var typeAnnot (Just start) end body

parseForRangeRestNoInit :: String -> Maybe Type -> Parser Statement
parseForRangeRestNoInit var typeAnnot = do
  pos <- getCurrentPos
  _ <- expect T.KwTo
  end <- withContext "end index" parseExpression
  body <- incLoopDepth (withContext "for block" parseBlock)
  pure $ StmtFor pos var typeAnnot Nothing end body

parseForEachRest :: String -> Maybe Type -> Parser Statement
parseForEachRest var typeAnnot = do
  pos <- getCurrentPos
  _ <- expect T.KwIn
  iterable <- withContext "iterable expression" parseExpression
  body <- incLoopDepth (withContext "for-each block" parseBlock)
  pure $ StmtForEach pos var typeAnnot iterable body

parseLoop :: Parser Statement
parseLoop = do
  pos <- getCurrentPos
  _ <- expect T.KwLoop
  body <- incLoopDepth (withContext "loop block" parseBlock)
  pure $ StmtLoop pos body

--
-- control flow
--

parseStop :: Parser Statement
parseStop = do
  pos <- getCurrentPos
  inLoop <- checkLoopDepth
  unless inLoop $ failParse "The 'stop' statement can only be used inside a loop or for block"
  _ <- expect T.KwStop
  _ <- expect T.Semicolon
  pure $ StmtStop pos

parseNext :: Parser Statement
parseNext = do
  pos <- getCurrentPos
  inLoop <- checkLoopDepth
  unless inLoop $ failParse "The 'next' statement can only be used inside a loop or for block"
  _ <- expect T.KwNext
  _ <- expect T.Semicolon
  pure $ StmtNext pos
