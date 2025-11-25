module Rune.AST.ParserHelper
  ( failParse,
    withContext,
    getParserState,
    peek,
    advance,
    expect,
    expectIdent,
    check,
    match,
    try,
    choice,
    between,
    sepBy,
    sepBy1,
    sepEndBy,
    chainl1,
    chainUnary,
    chainPostfix,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (when)
import Data.Bifunctor (Bifunctor (first))
import Rune.AST.ParserTypes (Parser (..), ParserState (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

--
-- error handling
--

-- | add descriptive context to parser errors
-- | wraps a parser and adds a breadcrumb trail when it fails
withContext :: String -> Parser a -> Parser a
withContext ctx (Parser p) =
  Parser $ \s ->
    first (\err -> err <> "  ... in " <> ctx <> "\n") (p s)

-- | fail the parser with an error message well formatted with:
-- path: line:column: msg
-- got: actual_token
-- ... in <context>
failParse :: String -> Parser a
failParse msg = Parser $ \s ->
  let tok = currentToken s
      path = psFilePath s
      line = show $ T.tokenLine tok
      col = show $ T.tokenColumn tok

      actual = case T.tokenKind tok of
        T.EOF -> "<EOF>"
        k -> show k <> " " <> show (T.tokenValue tok)

      err =
        unlines
          [ path <> ":" <> line <> ":" <> col <> ": error:",
            "  " <> msg,
            "  Got: " <> actual
          ]
   in Left err

-- | try a parser; if it fails, backtrack to original state
-- used for optional alternatives in choice | <?> operators
try :: Parser a -> Parser a
try (Parser p) = Parser $ \s -> case p s of
  Left _ -> Left ""
  Right success -> Right success

getParserState :: Parser ParserState
getParserState = Parser $ \s -> Right (s, s)

--
-- tokens helpers
--

-- | get current token
peek :: Parser T.Token
peek = currentToken <$> getParserState

-- | consume current token
advance :: Parser ()
advance = Parser $ \s -> Right ((), s {psPosition = psPosition s + 1})

-- | expect current token to match kind, consume and return it else fail
expect :: T.TokenKind -> Parser T.Token
expect kind = do
  t <- peek
  when (T.tokenKind t /= kind) $
    failParse $
      "Expected " ++ show kind
  advance
  pure t

-- | expect current token to be identifier with given name
expectIdent :: String -> Parser ()
expectIdent name = do
  t <- peek
  case T.tokenKind t of
    T.Identifier s | s == name -> advance
    _ -> failParse $ "Expected identifier: " ++ name

-- | check if current token matches kind
check :: T.TokenKind -> Parser Bool
check kind = (== kind) . T.tokenKind <$> peek

-- | check if current token matches kind + consume if true
match :: T.TokenKind -> Parser Bool
match kind = do
  isMatch <- check kind
  when isMatch advance
  pure isMatch

--
-- combinators
--

-- | one or the other
-- example: choice [parseString, parseInt, parseBool]
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

-- | parse `p` enclosed by `open` and `close`
-- example: between (expect LParen) (expect RParen) parseExpression
-- parses: ( expression )
between :: Parser o -> Parser c -> Parser a -> Parser a
between open close p = open *> p <* close

-- | parse list of `p` separated by `sep`
-- example: sepBy parseExpr (expect Comma)
-- parses: expr1, expr2, expr3 OR <empty>
sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | same as sepBy but requires at least one element
-- example: sepBy1 parseDigit (expect Comma)
-- parses: 1, 2, 3 (but fails on empty)
sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | parse list of `p` separated and optionally ended by `sep`
-- example: sepEndBy parseExpr (expect Comma)
-- parses: expr1, expr2, expr3, OR expr1, expr2, expr3,
sepEndBy :: Parser a -> Parser s -> Parser [a]
sepEndBy p sep =
  choice
    [ do
        x <- p
        choice
          [ sep *> ((x :) <$> sepEndBy p sep),
            pure [x]
          ],
      pure []
    ]

-- | parse left-associative binary operators
-- used for arithmetic: 1 - 2 - 3 becomes ((1 - 2) - 3)
-- example: chainl1 parseTerm (ExprBinary Add <$ expect OpPlus)
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op; y <- p; rest (f x y)) <|> pure x

-- | like chainl1 but allows unary operators before the term
-- example: chainUnary (ExprUnary Negate <$ expect OpMinus) parseAtom
-- parses: - - 5
chainUnary :: Parser (a -> a) -> Parser a -> Parser a
chainUnary op p = (op <*> chainUnary op p) <|> p

-- | parse stacked left-associative postfix operators
-- used for function calls or member access: a.b().c
-- example: chainPostfix parseAtom (parseCall <|> parseFieldAccess)
chainPostfix :: Parser a -> Parser (a -> a) -> Parser a
chainPostfix p op = p >>= rest
  where
    rest x = (do f <- op; rest (f x)) <|> pure x

--
-- private
--

currentToken :: ParserState -> T.Token
currentToken s
  | psPosition s >= length (psTokens s) = T.Token T.EOF "" 0 0
  | otherwise = psTokens s !! psPosition s
