module Rune.IR.Nodes
  ( IRProgram (..),
    IRTopLevel (..),
    IRFunction (..),
    IRInstruction (..),
    IROperand (..),
    IRType (..),
    IRLabel (..),
    IRBinaryOp (..),
    IRCompareOp (..),
    IRGen,
    GenState (..),
  )
where

import Control.Monad.State (State)
import Data.Map (Map)

--
-- state
--

data GenState = GenState
  { gsTempCounter :: Int,
    gsLabelCounter :: Int,
    gsStringCounter :: Int,
    gsGlobals :: [IRTopLevel],
    gsCurrentFunc :: Maybe String,
    gsSymTable :: Map String (IROperand, IRType)
  }
  deriving (Show, Eq)

type IRGen = State GenState

--
-- public
--

data IRType
  = IRI32
  | IRI64
  | IRF32
  | IRF64
  | IRU8
  | IRPtr IRType
  | IRVoid
  deriving (Show, Eq)

data IRBinaryOp
  = IRADD
  | IRSUB
  | IRMUL
  | IRDIV
  | IRMOD
  deriving (Show, Eq)

data IRCompareOp
  = IREQ -- ==
  | IRNEQ -- !=
  | IRLT -- <
  | IRLTE -- <=
  | IRGT -- >
  | IRGTE -- >=
  deriving (Show, Eq)

data IROperand
  = IRConstInt Int
  | IRConstFloat Double
  | IRConstChar Char
  | IRTemp String IRType -- t0, t1, ...
  | IRParam String IRType -- p_<var_name>, p_<other_var_name>, ...
  | IRGlobal String IRType -- GLOBAL <type>_<name> = ...
  deriving (Show, Eq)

newtype IRLabel = IRLabel String
  deriving (Show, Eq)

data IRInstruction
  = -- memory operations
    IRALLOC String IRType -- allocate local variable
  | IRSTORE IROperand IROperand -- store value to address
  | IRLOAD String IROperand IRType -- load from address to temp
  | IRDEREF String IROperand IRType -- dereference pointer
  | -- arithmetic operations
    IRADD_OP String IROperand IROperand IRType -- t = a + b
  | IRSUB_OP String IROperand IROperand IRType -- t = a - b
  | IRMUL_OP String IROperand IROperand IRType -- t = a * b
  | IRDIV_OP String IROperand IROperand IRType -- t = a / b
  | IRMOD_OP String IROperand IROperand IRType -- t = a % b
  | -- comparison operations
    IRCMP_EQ String IROperand IROperand
  | IRCMP_NEQ String IROperand IROperand
  | IRCMP_LT String IROperand IROperand
  | IRCMP_LTE String IROperand IROperand
  | IRCMP_GT String IROperand IROperand
  | IRCMP_GTE String IROperand IROperand
  | -- logical operations
    IRAND_OP String IROperand IROperand IRType -- t = a && b
  | IROR_OP String IROperand IROperand IRType -- t = a || b
  | -- control flow
    IRLABEL IRLabel -- label definition
  | IRJUMP IRLabel -- unconditional jump
  | IRJUMP_TRUE IROperand IRLabel -- jump if true
  | IRJUMP_FALSE IROperand IRLabel -- jump if false
  | IRJUMP_EQ0 IROperand IRLabel -- jump if operand == 0
  | -- function operations
    IRCALL String String [IROperand] (Maybe IRType) -- result = call func(args)
  | IRRET (Maybe IROperand) -- return value
  | -- utility operations
    IRADDR String String IRType -- get address of global
  | IRINC IROperand -- increment pointer/value
  | IRDEC IROperand -- decrement pointer/value
  | IRASSIGN String IROperand IRType -- simple assignment
  deriving (Show, Eq)

data IRFunction = IRFunction
  { irFuncName :: String,
    irFuncParams :: [(String, IRType)],
    irFuncRetType :: Maybe IRType,
    irFuncBody :: [IRInstruction]
  }
  deriving (Show, Eq)

data IRTopLevel
  = IRGlobalString String String
  | IRFunctionDef IRFunction
  deriving (Show, Eq)

data IRProgram = IRProgram
  { irProgramName :: String,
    irProgramDefs :: [IRTopLevel]
  }
  deriving (Show, Eq)
