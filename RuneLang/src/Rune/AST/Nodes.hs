{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Rune.AST.Nodes
  ( Program (..),
    TopLevelDef (..),
    Statement (..),
    Expression (..),
    Type (..),
    BinaryOp (..),
    UnaryOp (..),
    Parameter (..),
    Field (..),
    Block,
    SourcePos (..),
    getExprPos,
    getStmtPos,
  )
where

-- | Source position information
data SourcePos = SourcePos
  { posFile :: String,
    posLine :: Int,
    posCol :: Int
  }
  deriving (Eq, Ord)

instance Show SourcePos where
  show (SourcePos file line col) = file <> ":" <> show line <> ":" <> show col

data Type
  = TypeI8
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
  | TypeCustom String
  | TypeArray Type
  deriving (Eq, Ord)

instance Show Type where
  show  TypeI8        = "i8"
  show  TypeI16       = "i16"
  show  TypeI32       = "i32"
  show  TypeI64       = "i64"
  show  TypeF32       = "f32"
  show  TypeF64       = "f64"
  show  TypeBool      = "bool"
  show  TypeU8        = "u8"
  show  TypeU16       = "u16"
  show  TypeU32       = "u32"
  show  TypeU64       = "u64"
  show  TypeChar      = "char"
  show  TypeString    = "str"
  show  TypeAny       = "any"
  show  TypeNull      = "null"
  show (TypeArray t)  = "arr" <> show t
  show (TypeCustom s) = s

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or
  deriving (Show, Eq)

data UnaryOp
  = Negate -- -x
  | Not -- !x
  | PropagateError -- x?
  | PrefixInc -- ++x
  | PrefixDec -- --x
  | PostfixInc -- x++
  | PostfixDec -- x--
  deriving (Show, Eq)

data Program = Program
  { programName :: String,
    programDefs :: [TopLevelDef]
  }
  deriving (Show, Eq)

data TopLevelDef
  = -- | function definition
    -- def foo(x: i32, y: f64) -> i32
    -- {
    --    ...
    -- }
    DefFunction
      { funcName :: String,
        funcParams :: [Parameter],
        funcReturnType :: Type,
        funcBody :: Block
      }
  | -- | struct definition
    -- struct Vec2f
    -- {
    --    x: f32,
    --    y: f32,
    --
    --    def add(self, other: Vec2f) -> Vec2f
    --    {
    --        ...
    --    }
    --
    -- }
    DefStruct
      { structName :: String,
        structFields :: [Field],
        structMethods :: [TopLevelDef]
      }
  | -- | method override definition
    -- override def show(value: Vec2f) -> null
    -- {
    --    ...
    -- }
    DefOverride
      { overrideName :: String,
        overrideParams :: [Parameter],
        overrideReturnType :: Type,
        overrideBody :: Block
      }
  deriving (Show, Eq)

-- | a block is a list of statements
-- {
--    statement1;
--    statement2;
-- }
type Block = [Statement]

-- | function parameter
-- (x: i32, y: f64)
data Parameter = Parameter {paramName :: String, paramType :: Type}
  deriving (Show, Eq)

-- | struct field
-- {
--     x: f32;
--     y: f32;
-- }
data Field = Field {fieldName :: String, fieldType :: Type}
  deriving (Show, Eq)

-- | statements
-- variable declaration, return, if, for, expression statement
data Statement
  = -- | variable declaration
    -- x: i32 = 10; //<< explicit type annotation
    -- y = 20.5; //<< infer type from value
    StmtVarDecl
      { stmtPos :: SourcePos,
        varName :: String,
        varType :: Maybe Type, -- << optional type annotation, may infer from value
        varValue :: Expression
      }
  | -- | variable assignment (including compound assignments like +=)
    -- x = 10;
    -- y += 5;
    StmtAssignment
      { stmtPos :: SourcePos, -- << position of the assignment statement
        assignLValue :: Expression, -- << LValue (variable, field access, etc.)
        assignRValue :: Expression -- << RValue (result of operation, e.g., x + 5 for x += 5)
      }
  | -- | return statement
    -- {
    --     return expression;
    -- }
    --
    -- or
    --
    -- {
    --     expression //<< implicit return of the last expression if no ';' and no explicit return
    -- }
    StmtReturn SourcePos (Maybe Expression)
  | -- | if else statement
    -- {
    --     if condition {
    --         ...
    --     } else {
    --         ...
    --     }
    -- }
    StmtIf
      { stmtPos :: SourcePos,
        ifCond :: Expression,
        ifThen :: Block,
        ifElse :: Maybe Block
      }
  | -- | for loop
    -- {
    --     for i = 0 to 10 {
    --         ...
    --     }
    -- }
    -- or
    -- {
    --     for i to 10 { // start implicite
    --         ...
    --     }
    -- }
    StmtFor
      { stmtPos :: SourcePos,
        forVar :: String,
        forVarType :: Maybe Type,
        forStart :: Maybe Expression, -- << optional start expression
        forEnd :: Expression,
        forBody :: Block
      }
  | -- | for-each loop
    -- {
    --     for item in iterable {
    --         ...
    --     }
    -- }
    StmtForEach
      { stmtPos :: SourcePos,
        forEachVar :: String,
        forEachVarType :: Maybe Type,
        forEachIterable :: Expression,
        forEachBody :: Block
      }
  | -- | infinite loop
    -- loop {
    --    ...
    -- }
    StmtLoop SourcePos Block
  | StmtStop SourcePos
  | StmtNext SourcePos
  | -- | expression statement
    -- {
    --    expression;
    --    ...
    -- }
    StmtExpr SourcePos Expression
  deriving (Show, Eq)

-- | expressions
-- binary operations, unary operations, function calls, struct initializations, field accesses, literals, variables
data Expression
  = -- | binary operation
    -- left <op> right
    ExprBinary SourcePos BinaryOp Expression Expression
  | -- | unary operation
    -- <op> expr
    ExprUnary SourcePos UnaryOp Expression
  | -- | function call
    -- foo(arg1, arg2, ...)
    ExprCall
      { exprPos :: SourcePos,
        callName :: String,
        callArgs :: [Expression]
      }
  | -- | struct initialization
    -- Vec2f { x: 1.0, y: 2.0 }
    ExprStructInit
      { exprPos :: SourcePos,
        initStructName :: String,
        initFields :: [(String, Expression)]
      }
  | -- | field access
    -- vec.x;
    -- vec.y;
    ExprAccess
      { exprPos :: SourcePos,
        accessTarget :: Expression,
        accessField :: String
      }
  | -- | array index
    -- arr[index]
    ExprIndex
      { exprPos :: SourcePos,
        indexTarget :: Expression,
        indexValue :: Expression
      }
  | -- | literals and variables
    -- 42
    ExprLitInt SourcePos Int
  | -- 3.14
    ExprLitFloat SourcePos Double
  | -- "hello"
    ExprLitString SourcePos String
  | -- 'c'
    ExprLitChar SourcePos Char
  | -- true | false
    ExprLitBool SourcePos Bool
  | -- null
    ExprLitNull SourcePos
  | -- variable
    ExprVar SourcePos String
  | -- array literal
    -- [1, 2, 3, 4]
    ExprLitArray SourcePos [Expression]
  deriving (Show, Eq)

-- | Extract source position from an expression
getExprPos :: Expression -> SourcePos
getExprPos (ExprBinary pos _ _ _) = pos
getExprPos (ExprUnary pos _ _) = pos
getExprPos (ExprCall pos _ _) = pos
getExprPos (ExprStructInit pos _ _) = pos
getExprPos (ExprAccess pos _ _) = pos
getExprPos (ExprIndex pos _ _) = pos
getExprPos (ExprLitInt pos _) = pos
getExprPos (ExprLitFloat pos _) = pos
getExprPos (ExprLitString pos _) = pos
getExprPos (ExprLitChar pos _) = pos
getExprPos (ExprLitBool pos _) = pos
getExprPos (ExprLitNull pos) = pos
getExprPos (ExprVar pos _) = pos
getExprPos (ExprLitArray pos _) = pos

-- | Extract source position from a statement
getStmtPos :: Statement -> SourcePos
getStmtPos (StmtVarDecl pos _ _ _) = pos
getStmtPos (StmtAssignment pos _ _) = pos
getStmtPos (StmtReturn pos _) = pos
getStmtPos (StmtIf pos _ _ _) = pos
getStmtPos (StmtFor pos _ _ _ _ _) = pos
getStmtPos (StmtForEach pos _ _ _ _) = pos
getStmtPos (StmtLoop pos _) = pos
getStmtPos (StmtStop pos) = pos
getStmtPos (StmtNext pos) = pos
getStmtPos (StmtExpr pos _) = pos
