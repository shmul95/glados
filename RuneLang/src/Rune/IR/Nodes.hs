module Rune.IR.Nodes
  ( IRProgram (..),
    IRTopLevel (..),
    IRGlobalValue (..),
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
import Control.Monad.Except (ExceptT)
import Data.Map (Map)
import Data.Set (Set)
import Rune.Semantics.Type (FuncStack)

--
-- state
--

data GenState = GenState
  { gsTempCounter :: Int,
    gsLabelCounter :: Int,
    gsStringCounter :: Int,
    gsFloatCounter :: Int,
    gsGlobals :: [IRTopLevel],
    gsCurrentFunc :: Maybe String,
    gsSymTable :: Map String (IROperand, IRType),
    gsStructs :: Map String [(String, IRType)],
    gsLoopStack :: [(IRLabel, IRLabel)],
    gsCalledFuncs :: Set String,
    gsStringMap :: Map String String,
    gsFloatMap :: Map (Double, IRType) String,
    gsFuncStack :: FuncStack
  }
  deriving (Show, Eq)

type IRGen = ExceptT String (State GenState)

--
-- public
--

data IRType
  = IRI8
  | IRI16
  | IRI32
  | IRI64
  | IRU8
  | IRU16
  | IRU32
  | IRU64
  | IRChar
  | IRF32
  | IRF64
  | IRBool
  | IRNull
  | IRPtr IRType
  | IRStruct String
  | IRArray IRType Int
  deriving (Show, Eq, Ord)

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
  | IRConstBool Bool
  | IRConstNull
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
  | -- array
    IRALLOC_ARRAY String IRType [IROperand]
  | IRGET_ELEM String IROperand IROperand IRType
  | IRSET_ELEM IROperand IROperand IROperand
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
    irFuncBody :: [IRInstruction],
    irFuncIsExport :: Bool
  }
  deriving (Show, Eq)

data IRGlobalValue
  = IRGlobalStringVal String
  | IRGlobalFloatVal Double IRType
  deriving (Show, Eq)

data IRTopLevel
  = IRGlobalDef String IRGlobalValue
  | IRFunctionDef IRFunction
  | IRStructDef String [(String, IRType)]
  | IRExtern String
  deriving (Show, Eq)

data IRProgram = IRProgram
  { irProgramName :: String,
    irProgramDefs :: [IRTopLevel]
  }
  deriving (Show, Eq)
