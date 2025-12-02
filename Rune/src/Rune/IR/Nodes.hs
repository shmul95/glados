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
    gsSymTable :: Map String (IROperand, IRType),
    gsStructs :: Map String [(String, IRType)],
    gsLoopStack :: [(IRLabel, IRLabel)] -- (header, end)
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
  | IRStruct String
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
  | IRTemp String IRType
  | IRParam String IRType
  | IRGlobal String IRType
  deriving (Show, Eq)

newtype IRLabel = IRLabel String
  deriving (Show, Eq)

data IRInstruction
  = -- memory
    IRALLOC String IRType
  | IRSTORE IROperand IROperand
  | IRLOAD String IROperand IRType
  | IRDEREF String IROperand IRType
  | -- struct
    IRGET_FIELD String IROperand String String IRType
  | IRSET_FIELD IROperand String String IROperand
  | -- arithmetic
    IRADD_OP String IROperand IROperand IRType
  | IRSUB_OP String IROperand IROperand IRType
  | IRMUL_OP String IROperand IROperand IRType
  | IRDIV_OP String IROperand IROperand IRType
  | IRMOD_OP String IROperand IROperand IRType
  | -- comparison
    IRCMP_EQ String IROperand IROperand
  | IRCMP_NEQ String IROperand IROperand
  | IRCMP_LT String IROperand IROperand
  | IRCMP_LTE String IROperand IROperand
  | IRCMP_GT String IROperand IROperand
  | IRCMP_GTE String IROperand IROperand
  | -- logical
    IRAND_OP String IROperand IROperand IRType
  | IROR_OP String IROperand IROperand IRType
  | -- control flow
    IRLABEL IRLabel
  | IRJUMP IRLabel
  | IRJUMP_TRUE IROperand IRLabel
  | IRJUMP_FALSE IROperand IRLabel
  | IRJUMP_EQ0 IROperand IRLabel
  | -- function
    IRCALL String String [IROperand] (Maybe IRType)
  | IRRET (Maybe IROperand)
  | -- utility
    IRADDR String String IRType
  | IRINC IROperand
  | IRDEC IROperand
  | IRASSIGN String IROperand IRType
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
  | IRStructDef String [(String, IRType)]
  deriving (Show, Eq)

data IRProgram = IRProgram
  { irProgramName :: String,
    irProgramDefs :: [IRTopLevel]
  }
  deriving (Show, Eq)
