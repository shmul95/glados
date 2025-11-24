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
  | TypeString
  | TypeAny
  | TypeNull
  | TypeCustom String
  deriving (Show, Eq)

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
  = Negate
  | PropagateError
  deriving (Show, Eq)

data Program = Program 
  { programName :: String,
    programDefs :: [TopLevelDef]
  }
  deriving (Show, Eq)

data TopLevelDef
    -- | function definition
    -- def foo(x: i32, y: f64) -> i32
    -- {
    --    ...
    -- }
    = DefFunction
      { funcName :: String,
        funcParams :: [Parameter],
        funcReturnType :: Type,
        funcBody :: Block
      }
    -- | struct definition
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
    | DefStruct
      { structName :: String,
        structFields :: [Field],
        structMethods :: [TopLevelDef]
      }
    -- | method override definition
    -- override def show(value: Vec2f) -> null
    -- {
    --    ...
    -- }
    | DefOverride
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
    -- | variable declaration
    -- x: i32 = 10; //<< explicit type annotation
    -- y = 20.5; //<< infer type from value
    = StmtVarDecl
      { varName :: String,
        varType :: Maybe Type, -- << optional type annotation, may infer from value
        varValue :: Expression
      }
    -- | return statement
    -- {
    --     return expression;
    -- }
    --
    -- or
    --
    -- {
    --     expression //<< implicit return of the last expression if no ';' and no explicit return
    -- }
    | StmtReturn (Maybe Expression)
    -- | if else statement
    -- {
    --     if condition {
    --         ...
    --     } else {
    --         ...
    --     }
    -- }
    | StmtIf
      { ifCond :: Expression,
        ifThen :: Block,
        ifElse :: Maybe Block
      }
    -- | for loop
    -- {
    --     for i = 0 to 10 {
    --         ...
    --     }
    -- }
    | StmtFor
      { forVar :: String,
        forStart :: Expression,
        forEnd :: Expression,
        forBody :: Block
      }
    -- | for-each loop
    -- {
    --     for item in iterable {
    --         ...
    --     }
    -- }
    | StmtForEach
      { forEachVar :: String,
        forEachIterable :: Expression,
        forEachBody :: Block
      }
    -- | expression statement
    -- {
    --    expression;
    --    ...
    -- }
    | StmtExpr Expression
  deriving (Show, Eq)

-- | expressions
-- binary operations, unary operations, function calls, struct initializations, field accesses, literals, variables
data Expression
    -- | binary operation
    -- left <op> right
    = ExprBinary BinaryOp Expression Expression
    -- | unary operation
    -- <op> expr
    | ExprUnary UnaryOp Expression
    -- | function call
    -- foo(arg1, arg2, ...)
    | ExprCall
      { callName :: String,
        callArgs :: [Expression]
      }
    -- | struct initialization
    -- Vec2f { x: 1.0, y: 2.0 }
    | ExprStructInit
      { initStructName :: String,
        initFields :: [(String, Expression)]
      }
    -- | field access
    -- vec.x;
    -- vec.y;
    | ExprAccess
      { accessTarget :: Expression,
        accessField :: String
      }
    -- | literals and variables
    -- 42
    | ExprLitInt Int
    -- 3.14
    | ExprLitFloat Double
    -- "hello"
    | ExprLitString String
    -- true | false
    | ExprLitBool Bool
    -- null
    | ExprLitNull
    -- variable
    | ExprVar String
  deriving (Show, Eq)
