{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Backend.X86_64.Struct
  ( emitAllocStruct,
    emitGetField,
    emitSetField,
    sizeOfStruct,
    getFieldOffset,
  )
where
#else
module Rune.Backend.X86_64.Struct
  ( emitAllocStruct,
    emitGetField,
    emitSetField,
    sizeOfStruct,
    getFieldOffset,
  )
where
#endif

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Rune.Backend.Helpers (emit)
import Rune.Backend.X86_64.LoadStore (loadReg, loadRegWithExt, stackAddr, storeReg)
import Rune.Backend.X86_64.Registers (getRegisterName, getSizeSpecifier)
import Rune.Backend.X86_64.Compare (loadFloatOperand, isFloatType)
import Rune.IR.Nodes (IROperand (..), IRType (..))
import Rune.IR.IRHelpers (sizeOfIRType)

import Lib (alignTo, alignSize)

-- | Type alias for struct field definitions: (fieldName, fieldType)
type StructDef = [(String, IRType)]

-- | Type alias for struct definitions map
type StructMap = Map String StructDef

-- | Calculate the total size of a struct with proper alignment.
sizeOfStruct :: StructMap -> String -> Int
sizeOfStruct structMap name = sizeOfIRType structMap (IRStruct name)

-- | Get the byte offset of a specific field within a struct.
getFieldOffset :: StructMap -> String -> String -> Int
getFieldOffset structMap structName fieldName =
  case Map.lookup structName structMap of
    Nothing -> error $ "Struct not found: " <> structName
    Just fields -> findFieldOffset fields 0
  where
    findFieldOffset :: StructDef -> Int -> Int
    findFieldOffset [] _ = error $ "Field '" <> fieldName <> "' not found in struct '" <> structName <> "'"
    findFieldOffset ((name, typ):rest) offset
      | name == fieldName = alignedOffset
      | otherwise = findFieldOffset rest (alignedOffset + size)
      where
        size = sizeOfIRType structMap typ
        align = alignSize size
        alignedOffset = alignTo align offset

-- | Emit assembly for allocating a struct on the stack.
-- This is essentially a no-op since stack space is pre-allocated,
-- but we ensure the struct slot is properly initialized if needed.
emitAllocStruct :: Map String Int -> String -> String
emitAllocStruct _ _ = ""  -- Stack space is already allocated in calculateStackMap

-- | Emit assembly for getting a field from a struct.
-- IRGET_FIELD dest basePtr structName fieldName fieldType
--
-- Steps:
-- 1. Load the struct pointer into a register
-- 2. Calculate the field offset
-- 3. Load the field value at [ptr + offset] into the destination
emitGetField :: Map String Int -> StructMap -> String -> IROperand -> String -> String -> IRType -> [String]
emitGetField stackMap structMap dest basePtr structName fieldName fieldType =
  let offset = getFieldOffset structMap structName fieldName
      sizeSpec = getSizeSpecifier fieldType
      reg = getRegisterName "rax" fieldType
   in if isFloatType fieldType
      then emitGetFieldFloat stackMap dest basePtr offset fieldType
      else loadReg stackMap "rdi" basePtr
           <> [ emit 1 $ "mov " <> reg <> ", " <> sizeSpec <> " [rdi + " <> show offset <> "]"
              , storeReg stackMap dest "rax" fieldType
              ]

-- | Emit assembly for getting a float field from a struct.
emitGetFieldFloat :: Map String Int -> String -> IROperand -> Int -> IRType -> [String]
emitGetFieldFloat stackMap dest basePtr offset fieldType =
  let (movInstr, sizeSpec) = case fieldType of
        IRF32 -> ("movss", "dword")
        IRF64 -> ("movsd", "qword")
        _     -> ("movsd", "qword")  -- fallback
   in loadReg stackMap "rdi" basePtr
      <> [ emit 1 $ movInstr <> " xmm0, " <> sizeSpec <> " [rdi + " <> show offset <> "]"
         , emit 1 $ movInstr <> " " <> sizeSpec <> " " <> stackAddr stackMap dest <> ", xmm0"
         ]

-- | Emit assembly for setting a field in a struct.
-- IRSET_FIELD basePtr structName fieldName value
--
-- Steps:
-- 1. Load the struct pointer into a register
-- 2. Calculate the field offset
-- 3. Load the value into a register
-- 4. Store the value at [ptr + offset]
emitSetField :: Map String Int -> StructMap -> IROperand -> String -> String -> IROperand -> [String]
emitSetField stackMap structMap basePtr structName fieldName valueOp =
  let offset = getFieldOffset structMap structName fieldName
      fieldType = getFieldType structMap structName fieldName
   in if isFloatType fieldType
      then emitSetFieldFloat stackMap basePtr offset valueOp fieldType
      else emitSetFieldInt stackMap basePtr offset valueOp fieldType

-- | Get the type of a specific field in a struct.
getFieldType :: StructMap -> String -> String -> IRType
getFieldType structMap structName fieldName =
  case Map.lookup structName structMap of
    Nothing -> IRI64 -- should never happens
    Just fields -> fromMaybe IRI64 $ lookup fieldName fields

-- | Emit assembly for setting an integer/pointer field.
emitSetFieldInt :: Map String Int -> IROperand -> Int -> IROperand -> IRType -> [String]
emitSetFieldInt stackMap basePtr offset valueOp fieldType =
  let sizeSpec = getSizeSpecifier fieldType
      reg = getRegisterName "rax" fieldType
   in loadReg stackMap "rdi" basePtr
      <> loadRegWithExt stackMap ("rax", valueOp)
      <> [ emit 1 $ "mov " <> sizeSpec <> " [rdi + " <> show offset <> "], " <> reg ]

-- | Emit assembly for setting a float field.
emitSetFieldFloat :: Map String Int -> IROperand -> Int -> IROperand -> IRType -> [String]
emitSetFieldFloat stackMap basePtr offset valueOp fieldType =
  let (movInstr, sizeSpec) = case fieldType of
        IRF32 -> ("movss", "dword")
        IRF64 -> ("movsd", "qword")
        _     -> ("movsd", "qword")  -- fallback
   in loadReg stackMap "rdi" basePtr
      <> loadFloatOperand stackMap "xmm0" valueOp fieldType
      <> [ emit 1 $ movInstr <> " " <> sizeSpec <> " [rdi + " <> show offset <> "], xmm0" ]
