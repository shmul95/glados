{-# LANGUAGE CPP #-}

module Rune.Backend.X86_64.Operations
  ( emitBinaryOp,
    emitDivOp,
    emitModOp,
    emitShiftOp,
    emitBitNot
#if defined(TESTING_EXPORT)
  ,
    emitFloatBinaryOp,
    emitFloatDivOp,
    emitIntDivOp,
    emitIntModOp,
    emitSmallMul
#endif
  )
where

import Data.Map (Map)
import Rune.Backend.Helpers (emit)
import Rune.Backend.X86_64.Compare (isFloatType, loadFloatOperand)
import Rune.Backend.X86_64.LoadStore (loadReg, stackAddr, storeReg)
import Rune.Backend.X86_64.Registers (getRegisterName, x86_64FloatArgsRegisters)
import Rune.IR.Nodes (IROperand(..), IRType (..))

--
-- public
--

-- | emit dest = left <asmOp> right
emitBinaryOp :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitBinaryOp sm dest asmOp leftOp rightOp t
  | isFloatType t = emitFloatBinaryOp sm dest asmOp leftOp rightOp t
  | asmOp == "imul" && t `elem` [IRI8, IRU8, IRI16, IRU16] = emitSmallMul sm dest leftOp rightOp t

  -- use immediate value for small constants on right side
  | isSmallImmediate rightOp =
      let regL = getRegisterName "rax" t
       in loadReg sm "rax" leftOp
       <> [emit 1 $ asmOp <> " " <> regL <> ", " <> showImmediate rightOp]
       <> [storeReg sm dest "rax" t]

  | otherwise =
      let regL = getRegisterName "rax" t
          regR = getRegisterName "rbx" t
       in loadReg sm "rax" leftOp
       <> loadReg sm "rbx" rightOp
       <> [emit 1 $ asmOp <> " " <> regL <> ", " <> regR]
       <> [storeReg sm dest "rax" t]


-- | check if operand is a small immediate that can be used directly
isSmallImmediate :: IROperand -> Bool
isSmallImmediate (IRConstInt n) = n >= -2147483648 && n <= 2147483647
isSmallImmediate _ = False

-- | show immediate value
showImmediate :: IROperand -> String
showImmediate (IRConstInt n) = show n
showImmediate _ = "0"

-- | emit dest = left / right
emitDivOp :: Map String Int -> String -> IROperand -> IROperand -> IRType -> [String]
emitDivOp sm dest leftOp rightOp t
  | isFloatType t = emitFloatDivOp sm dest leftOp rightOp t
  | otherwise = emitIntDivOp sm dest leftOp rightOp t

-- | emit dest = left % right
emitModOp :: Map String Int -> String -> IROperand -> IROperand -> IRType -> [String]
emitModOp sm dest leftOp rightOp t
  | isFloatType t = [emit 1   "; TODO: floating point modulo not supported"]
  | otherwise = emitIntModOp sm dest leftOp rightOp t

-- | emit dest = left >> right  or  dest = left << right (shift operations)
emitShiftOp :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitShiftOp sm dest shiftInstr leftOp rightOp t

  -- use immediate shift amount for constants
  | isSmallImmediate rightOp =
      let regL = getRegisterName "rax" t
       in loadReg sm "rax" leftOp
       <> [emit 1 $ shiftInstr <> " " <> regL <> ", " <> showImmediate rightOp]
       <> [storeReg sm dest "rax" t]

  | otherwise =
      let regL = getRegisterName "rax" t
       in loadReg sm "rax" leftOp
       <> loadReg sm "rcx" rightOp
       <> [emit 1 $ shiftInstr <> " " <> regL <> ", cl"]
       <> [storeReg sm dest "rax" t]

--
-- private
--

-- | floating point division uses SSE instructions
emitFloatDivOp :: Map String Int -> String -> IROperand -> IROperand -> IRType -> [String]
emitFloatDivOp sm dest leftOp rightOp t =
  load leftOp xmmL t
    <> load rightOp xmmR t
    <> [emit 1 (divInstr <> " " <> xmmL <> ", " <> xmmR)]
    <> [store t]
  where
    (xmmL, xmmR) =
      case x86_64FloatArgsRegisters of
        r0 : r1 : _ -> (r0, r1)
        _           -> ("xmm0", "xmm1")
    
    load op reg = loadFloatOperand sm reg op
    
    divInstr = case t of
      IRF32 -> "divss"
      IRF64 -> "divsd"
      _     -> "divss"
    
    store IRF32 = emit 1 $ "movss dword " <> stackAddr sm dest <> ", " <> xmmL
    store IRF64 = emit 1 $ "movsd qword " <> stackAddr sm dest <> ", " <> xmmL
    store other = emit 1 $ "; TODO: unsupported float div result type: " <> show other

emitIntDivOp :: Map String Int -> String -> IROperand -> IROperand -> IRType -> [String]
emitIntDivOp sm dest leftOp rightOp t =
  case t of
    IRI8  -> emitSmallDiv sm dest leftOp rightOp "movsx" "eax" "ecx" "al" "cl" "cdq" "idiv" t
    IRU8  -> emitSmallDiv sm dest leftOp rightOp "movzx" "eax" "ecx" "al" "cl" "xor edx, edx" "div" t
    IRI16 -> emitSmallDiv sm dest leftOp rightOp "movsx" "eax" "ecx" "ax" "cx" "cdq" "idiv" t
    IRU16 -> emitSmallDiv sm dest leftOp rightOp "movzx" "eax" "ecx" "ax" "cx" "xor edx, edx" "div" t
    IRI32 -> emit32Div sm dest leftOp rightOp "cdq" "idiv"
    IRU32 -> emit32Div sm dest leftOp rightOp "xor edx, edx" "div"
    IRI64 -> emit64Div sm dest leftOp rightOp "cqo" "idiv"
    IRU64 -> emit64Div sm dest leftOp rightOp "xor rdx, rdx" "div"
    _     -> [emit 1 $ "; TODO: unsupported integer division type: " <> show t]
  where
    emitSmallDiv sm' dest' leftOp' rightOp' extInstr regA regB srcA srcB signExt divInstr typ =
      loadReg sm' "rax" leftOp'
        <> [emit 1 $ extInstr <> " " <> regA <> ", " <> srcA]
        <> loadReg sm' "rcx" rightOp'
        <> [emit 1 $ extInstr <> " " <> regB <> ", " <> srcB]
        <> [emit 1 signExt]
        <> [emit 1 $ divInstr <> " " <> regB]
        <> [storeReg sm' dest' "rax" typ]
    
    emit32Div sm' dest' leftOp' rightOp' signExt divInstr =
      loadReg sm' "rax" leftOp'
        <> [emit 1 signExt]
        <> loadReg sm' "rbx" rightOp'
        <> [emit 1 $ divInstr <> " ebx"]
        <> [storeReg sm' dest' "rax" t]
    
    emit64Div sm' dest' leftOp' rightOp' signExt divInstr =
      loadReg sm' "rax" leftOp'
        <> [emit 1 signExt]
        <> loadReg sm' "rbx" rightOp'
        <> [emit 1 $ divInstr <> " rbx"]
        <> [storeReg sm' dest' "rax" t]


emitSmallMul :: Map String Int -> String -> IROperand -> IROperand -> IRType -> [String]
emitSmallMul sm dest leftOp rightOp t =
  case t of
    IRI8  -> emitExtMul "movsx" "eax" "edx" "al" "dl"
    IRU8  -> emitExtMul "movzx" "eax" "edx" "al" "dl"
    IRI16 -> emitExtMul "movsx" "eax" "edx" "ax" "dx"
    IRU16 -> emitExtMul "movzx" "eax" "edx" "ax" "dx"
    _     -> []
  where
    emitExtMul extInstr regA regB srcA srcB =
      loadReg sm "rax" leftOp
        <> [emit 1 $ extInstr <> " " <> regA <> ", " <> srcA]
        <> loadReg sm "rdx" rightOp
        <> [emit 1 $ extInstr <> " " <> regB <> ", " <> srcB]
        <> [emit 1 $ "imul " <> regA <> ", " <> regB]
        <> [storeReg sm dest "rax" t]

emitIntModOp :: Map String Int -> String -> IROperand -> IROperand -> IRType -> [String]
emitIntModOp sm dest leftOp rightOp t =
  case t of
    IRI8  -> emitSmallMod sm dest leftOp rightOp "movsx" "eax" "ecx" "al" "cl" "cdq" "idiv" t
    IRU8  -> emitSmallMod sm dest leftOp rightOp "movzx" "eax" "ecx" "al" "cl" "xor edx, edx" "div" t
    IRI16 -> emitSmallMod sm dest leftOp rightOp "movsx" "eax" "ecx" "ax" "cx" "cdq" "idiv" t
    IRU16 -> emitSmallMod sm dest leftOp rightOp "movzx" "eax" "ecx" "ax" "cx" "xor edx, edx" "div" t
    IRI32 -> emit32Mod sm dest leftOp rightOp "cdq" "idiv"
    IRU32 -> emit32Mod sm dest leftOp rightOp "xor edx, edx" "div"
    IRI64 -> emit64Mod sm dest leftOp rightOp "cqo" "idiv"
    IRU64 -> emit64Mod sm dest leftOp rightOp "xor rdx, rdx" "div"
    _     -> [emit 1 $ "; TODO: unsupported integer modulo type: " <> show t]
  where
    emitSmallMod sm' dest' leftOp' rightOp' extInstr regA regB srcA srcB signExt divInstr typ =
      loadReg sm' "rax" leftOp'
        <> [emit 1 $ extInstr <> " " <> regA <> ", " <> srcA]
        <> loadReg sm' "rcx" rightOp'
        <> [emit 1 $ extInstr <> " " <> regB <> ", " <> srcB]
        <> [emit 1 signExt]
        <> [emit 1 $ divInstr <> " " <> regB]
        <> [storeReg sm' dest' "rdx" typ]
    
    emit32Mod sm' dest' leftOp' rightOp' signExt divInstr =
      loadReg sm' "rax" leftOp'
        <> [emit 1 signExt]
        <> loadReg sm' "rbx" rightOp'
        <> [emit 1 $ divInstr <> " ebx"]
        <> [storeReg sm' dest' "rdx" t]
    
    emit64Mod sm' dest' leftOp' rightOp' signExt divInstr =
      loadReg sm' "rax" leftOp'
        <> [emit 1 signExt]
        <> loadReg sm' "rbx" rightOp'
        <> [emit 1 $ divInstr <> " rbx"]
        <> [storeReg sm' dest' "rdx" t]

emitFloatBinaryOp
  :: Map String Int
  -> String        -- destination stack slot
  -> String        -- IR op name ("add","sub","imul")
  -> IROperand
  -> IROperand
  -> IRType
  -> [String]
emitFloatBinaryOp sm dest asmOp leftOp rightOp t =
  load left xmmL t
    <> load right xmmR t
    <> [emit 1 (mnemonic t <> " " <> xmmL <> ", " <> xmmR)]
    <> [store t]
  where

    (xmmL, xmmR) =
      case x86_64FloatArgsRegisters of
        r0 : r1 : _ -> (r0, r1)
        _           -> ("xmm0", "xmm1")   -- safe fallback; never used

    load op reg = loadFloatOperand sm reg op

    left  = leftOp
    right = rightOp

    -- The SSE instruction names
    mnemonic IRF32 = prefix "ss"
    mnemonic IRF64 = prefix "sd"
    mnemonic _     = "movss"

    prefix suf =
      case asmOp of
        "add"  -> "add" <> suf
        "sub"  -> "sub" <> suf
        "imul" -> "mul" <> suf
        _      -> "movss"

    -- Store result
    store IRF32 = emit 1 $ "movss dword " <> stackAddr sm dest <> ", " <> xmmL
    store IRF64 = emit 1 $ "movsd qword " <> stackAddr sm dest <> ", " <> xmmL
    store other = emit 1 $ "; TODO: unsupported float binary result type: " <> show other

-- | emit dest = ~operand (bitwise NOT)
emitBitNot :: Map String Int -> String -> IROperand -> IRType -> [String]
emitBitNot sm dest op t =
  let reg = getRegisterName "rax" t
   in loadReg sm "rax" op
   <> [emit 1 $ "not " <> reg]
   <> [storeReg sm dest "rax" t]
