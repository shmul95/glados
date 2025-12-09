module Rune.Lexer.Tokens
  ( TokenKind (..),
    Token (..),
  )
where

data TokenKind
  -- | keywords
  = KwDef
  | KwReturn
  | KwStruct
  | KwIf
  | KwElse
  | KwFor
  | KwTo
  | KwOverride
  | KwIn
  | KwLoop
  | KwStop
  | KwNext
  -- | primitive types
  | TypeI8
  | TypeI16
  | TypeI32
  | TypeI64
  | TypeF32
  | TypeF64
  | TypeBool
  | TypeU8
  | TypeU16
  | TypeU32
  | TypeU64
  | TypeChar
  | TypeString
  | TypeAny
  | TypeNull
  -- | literals
  | LitInt Int
  | LitFloat Double
  | LitString String
  | LitChar Char
  | LitBool Bool
  | LitNull
  -- | identifiers
  | Identifier String
  -- | operators
  | OpPlus -- +
  | OpMinus -- -
  | OpMul -- *
  | OpDiv -- /
  | OpMod -- %
  | OpAssign -- =
  | OpAddAssign -- +=
  | OpSubAssign -- -=
  | OpMulAssign -- *=
  | OpDivAssign -- /=
  | OpModAssign -- %=
  | OpInc -- ++
  | OpDec -- --
  | OpEq -- ==
  | OpNeq -- !=
  | OpLt -- <
  | OpLte -- <=
  | OpGt -- >
  | OpGte -- >=
  | OpAnd -- &&
  | OpOr -- ||
  | OpErrorProp -- ?
  | OpArrow -- ->
  | OpSquigArrow -- ~>
  -- | delimiters
  | LParen
  | RParen -- ( )
  | LBrace
  | RBrace -- { }
  | Comma -- ,
  | Semicolon -- ;
  | Colon -- :
  | Dot -- .
  | EOF
  deriving (Show, Eq, Ord)

data Token = Token
  { tokenKind :: TokenKind,
    tokenValue :: String,
    tokenLine :: Int,
    tokenColumn :: Int
  }
  deriving (Show, Eq, Ord)
