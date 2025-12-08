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
  )
where

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
  deriving (Eq, Ord)

instance Show Type where
  show (TypeI8      ) = "i8"
  show (TypeI16     ) = "i16"
  show (TypeI32     ) = "i32"
  show (TypeI64     ) = "i64"
  show (TypeF32     ) = "f32"
  show (TypeF64     ) = "f64"
  show (TypeBool    ) = "bool"
  show (TypeU8      ) = "u8"
  show (TypeU16     ) = "u16"
  show (TypeU32     ) = "u32"
  show (TypeU64     ) = "u64"
  show (TypeChar    ) = "char"
  show (TypeString  ) = "str"
  show (TypeAny     ) = "any"
  show (TypeNull    ) = "null"
  show (TypeCustom s) = "type " ++ s

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
      { varName :: String,
        varType :: Maybe Type, -- << optional type annotation, may infer from value
        varValue :: Expression
      }
  | -- | variable assignment (including compound assignments like +=)
    -- x = 10;
    -- y += 5;
    StmtAssignment
      { assignLValue :: Expression, -- << LValue (variable, field access, etc.)
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
    StmtReturn (Maybe Expression)
  | -- | if else statement
    -- {
    --     if condition {
    --         ...
    --     } else {
    --         ...
    --     }
    -- }
    StmtIf
      { ifCond :: Expression,
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
      { forVar :: String,
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
      { forEachVar :: String,
        forEachVarType :: Maybe Type,
        forEachIterable :: Expression,
        forEachBody :: Block
      }
  | -- | infinite loop
    -- loop {
    --    ...
    -- }
    StmtLoop Block
  | StmtStop
  | StmtNext
  | -- | expression statement
    -- {
    --    expression;
    --    ...
    -- }
    StmtExpr Expression
  deriving (Show, Eq)

-- | expressions
-- binary operations, unary operations, function calls, struct initializations, field accesses, literals, variables
data Expression
  = -- | binary operation
    -- left <op> right
    ExprBinary BinaryOp Expression Expression
  | -- | unary operation
    -- <op> expr
    ExprUnary UnaryOp Expression
  | -- | function call
    -- foo(arg1, arg2, ...)
    ExprCall
      { callName :: String,
        callArgs :: [Expression]
      }
  | -- | struct initialization
    -- Vec2f { x: 1.0, y: 2.0 }
    ExprStructInit
      { initStructName :: String,
        initFields :: [(String, Expression)]
      }
  | -- | field access
    -- vec.x;
    -- vec.y;
    ExprAccess
      { accessTarget :: Expression,
        accessField :: String
      }
  | -- | literals and variables
    -- 42
    ExprLitInt Int
  | -- 3.14
    ExprLitFloat Double
  | -- "hello"
    ExprLitString String
  | -- 'c'
    ExprLitChar Char
  | -- true | false
    ExprLitBool Bool
  | -- null
    ExprLitNull
  | -- variable
    ExprVar String
  deriving (Show, Eq)
