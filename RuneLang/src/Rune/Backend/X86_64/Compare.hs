{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Backend.X86_64.Compare
  ( emitCompare,
    getCompareSetOp,
    CompareOp (..),
    isSignedType,
    isUnsignedType,
    isFloatType,
    getFloatRegs,
    stackAddr,
    loadOperand,
    loadFloatOperand,
    getSizeSpec,
    emitCompareInstructions,
    emitIntegerCompare,
    emitFloatCompare
  )
where
#else
module Rune.Backend.X86_64.Compare
  ( emitCompare,
    getCompareSetOp,
    CompareOp (..),
    isFloatType,
    loadFloatOperand,
  )
where
#endif

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Rune.Backend.Helpers (emit)
import Rune.Backend.X86_64.Registers (getRegisterName, x86_64FloatArgsRegisters, getIntegerCompareRegisters)
import Rune.IR.IRHelpers (getCommonType)
import Rune.IR.Nodes (IROperand (..), IRType (..))

--
-- global compare operations
--

data CompareOp = CmpEQ | CmpNEQ | CmpLT | CmpLTE | CmpGT | CmpGTE

--
-- public
--

emitCompare :: Map String Int -> String -> CompareOp -> IROperand -> IROperand -> [String]
emitCompare sm dest cmpOp leftOp rightOp =
  let cmpType = getCommonType leftOp rightOp
      setOp = getCompareSetOp cmpOp cmpType
   in emitCompareInstructions sm dest setOp leftOp rightOp cmpType

getCompareSetOp :: CompareOp -> IRType -> String
getCompareSetOp CmpEQ _ = "sete"
getCompareSetOp CmpNEQ _ = "setne"
getCompareSetOp CmpLT t
  | isSignedType t = "setl"
  | isUnsignedType t = "setb"
  | isFloatType t = "setb"
  | otherwise = error $ "Invalid type for CmpLT comparison: " <> show t
getCompareSetOp CmpLTE t
  | isSignedType t = "setle"
  | isUnsignedType t = "setbe"
  | isFloatType t = "setbe"
  | otherwise = error $ "Invalid type for CmpLTE comparison: " <> show t
getCompareSetOp CmpGT t
  | isSignedType t = "setg"
  | isUnsignedType t = "seta"
  | isFloatType t = "seta"
  | otherwise = error $ "Invalid type for CmpGT comparison: " <> show t
getCompareSetOp CmpGTE t
  | isSignedType t = "setge"
  | isUnsignedType t = "setae"
  | isFloatType t = "setae"
  | otherwise = error $ "Invalid type for CmpGTE comparison: " <> show t

--
-- private
--

emitCompareInstructions :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitCompareInstructions sm dest setOp leftOp rightOp cmpType
  | isFloatType cmpType = emitFloatCompare sm dest setOp leftOp rightOp cmpType
  | otherwise = emitIntegerCompare sm dest setOp leftOp rightOp cmpType

emitIntegerCompare :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitIntegerCompare sm dest setOp leftOp rightOp cmpType =
  let loadL = loadOperand sm "rax" leftOp cmpType
      loadR = loadOperand sm "rbx" rightOp cmpType
      stackLoc = stackAddr sm dest
   in loadL
        <> loadR
        <> [ emit 1 $ "cmp " <> cmpRegL <> ", " <> cmpRegR,
             emit 1 $ setOp <> " al",
             emit 1 $ "mov byte " <> stackLoc <> ", al"
           ]
    where
      (cmpRegL, cmpRegR) = getIntegerCompareRegisters cmpType

emitFloatCompare :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitFloatCompare sm dest setOp leftOp rightOp cmpType =
  let (regL, regR) = getFloatRegs cmpType
      loadL = loadFloatOperand sm regL leftOp cmpType
      loadR = loadFloatOperand sm regR rightOp cmpType
      stackLoc = stackAddr sm dest
      cmpInstr = case cmpType of
        IRF32 -> "ucomiss"
        IRF64 -> "ucomisd"
        _ -> error "Invalid float type for comparison"
   in loadL
        <> loadR
        <> [ emit 1 $ cmpInstr <> " " <> regL <> ", " <> regR,
             emit 1 $ setOp <> " al",
             emit 1 $ "mov byte " <> stackLoc <> ", al"
           ]

isSignedType :: IRType -> Bool
isSignedType IRI8 = True
isSignedType IRI16 = True
isSignedType IRI32 = True
isSignedType IRI64 = True
isSignedType IRChar = False
isSignedType _ = False

isUnsignedType :: IRType -> Bool
isUnsignedType IRU8 = True
isUnsignedType IRU16 = True
isUnsignedType IRU32 = True
isUnsignedType IRU64 = True
isUnsignedType IRChar = True
isUnsignedType IRBool = True
isUnsignedType _ = False

isFloatType :: IRType -> Bool
isFloatType IRF32 = True
isFloatType IRF64 = True
isFloatType _ = False

getFloatRegs :: IRType -> (String, String)
getFloatRegs _ = case x86_64FloatArgsRegisters of
  r0 : r1 : _ -> (r0, r1)
  _ -> ("xmm0", "xmm1")

stackAddr :: Map String Int -> String -> String
stackAddr sm name = case Map.lookup name sm of
  Just offset -> "[rbp" <> show offset <> "]"
  Nothing -> error $ "Variable not found in stack map: " <> name

loadOperand :: Map String Int -> String -> IROperand -> IRType -> [String]
loadOperand _ reg (IRConstInt n) _ = [emit 1 $ "mov " <> reg <> ", " <> show n]
loadOperand _ reg (IRConstChar c) _ = [emit 1 $ "mov " <> reg <> ", " <> show (fromEnum c)]
loadOperand _ reg (IRConstBool b) _ = [emit 1 $ "mov " <> reg <> ", " <> if b then "1" else "0"]
loadOperand sm baseReg (IRTemp name opType) _ =
  let sizeSpec = getSizeSpec opType
      stackLoc = stackAddr sm name
   in case opType of
        IRI8  -> [emit 1 $ "movsx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRI16 -> [emit 1 $ "movsx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRU8  -> [emit 1 $ "movzx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRU16 -> [emit 1 $ "movzx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRChar -> [emit 1 $ "movzx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRBool -> [emit 1 $ "movzx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        _ -> let reg = getRegisterName baseReg opType
              in [emit 1 $ "mov " <> reg <> ", " <> sizeSpec <> " " <> stackLoc]
loadOperand sm baseReg (IRParam name opType) _ =
  let sizeSpec = getSizeSpec opType
      stackLoc = stackAddr sm name
   in case opType of
        IRI8  -> [emit 1 $ "movsx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRI16 -> [emit 1 $ "movsx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRU8  -> [emit 1 $ "movzx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRU16 -> [emit 1 $ "movzx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRChar -> [emit 1 $ "movzx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        IRBool -> [emit 1 $ "movzx " <> baseReg <> ", " <> sizeSpec <> " " <> stackLoc]
        _ -> let reg = getRegisterName baseReg opType
              in [emit 1 $ "mov " <> reg <> ", " <> sizeSpec <> " " <> stackLoc]
loadOperand _ reg _ _ = [emit 1 $ "mov " <> reg <> ", 0"]

loadFloatOperand :: Map String Int -> String -> IROperand -> IRType -> [String]
loadFloatOperand sm reg (IRTemp   name t    ) targetType 
  | t == targetType && targetType == IRF32 = [emit 1 $ "movss " <> reg <> ", dword " <> stackAddr sm name]
  | t == targetType && targetType == IRF64 = [emit 1 $ "movsd " <> reg <> ", qword " <> stackAddr sm name]
  | t == IRF32 && targetType == IRF64      = [emit 1 $ "movss " <> reg <> ", dword " <> stackAddr sm name, emit 1 $ "cvtss2sd " <> reg <> ", " <> reg]
  | t == IRF64 && targetType == IRF32      = [emit 1 $ "movsd " <> reg <> ", qword " <> stackAddr sm name, emit 1 $ "cvtsd2ss " <> reg <> ", " <> reg]
loadFloatOperand sm reg (IRParam  name t    ) targetType
  | t == targetType && targetType == IRF32 = [emit 1 $ "movss " <> reg <> ", dword " <> stackAddr sm name]
  | t == targetType && targetType == IRF64 = [emit 1 $ "movsd " <> reg <> ", qword " <> stackAddr sm name]
  | t == IRF32 && targetType == IRF64      = [emit 1 $ "movss " <> reg <> ", dword " <> stackAddr sm name, emit 1 $ "cvtss2sd " <> reg <> ", " <> reg]
  | t == IRF64 && targetType == IRF32      = [emit 1 $ "movsd " <> reg <> ", qword " <> stackAddr sm name, emit 1 $ "cvtsd2ss " <> reg <> ", " <> reg]
loadFloatOperand _  reg (IRGlobal name t    ) targetType
  | t == targetType && targetType == IRF32 = [emit 1 $ "movss " <> reg <> ", dword [rel " <> name <> "]"]
  | t == targetType && targetType == IRF64 = [emit 1 $ "movsd " <> reg <> ", qword [rel " <> name <> "]"]
  | t == IRF32 && targetType == IRF64      = [emit 1 $ "movss " <> reg <> ", dword [rel " <> name <> "]", emit 1 $ "cvtss2sd " <> reg <> ", " <> reg]
  | t == IRF64 && targetType == IRF32      = [emit 1 $ "movsd " <> reg <> ", qword [rel " <> name <> "]", emit 1 $ "cvtsd2ss " <> reg <> ", " <> reg]
loadFloatOperand _  _   (IRConstFloat val   ) IRF32 = [emit 1 $ "; TODO: load f32 constant " <> show val]
loadFloatOperand _  _   (IRConstFloat val   ) IRF64 = [emit 1 $ "; TODO: load f64 constant " <> show val]
loadFloatOperand _  reg _ _                         = [emit 1 $ "xorps " <> reg <> ", " <> reg]

getSizeSpec :: IRType -> String
getSizeSpec IRI8 = "byte"
getSizeSpec IRI16 = "word"
getSizeSpec IRI32 = "dword"
getSizeSpec IRI64 = "qword"
getSizeSpec IRU8 = "byte"
getSizeSpec IRU16 = "word"
getSizeSpec IRU32 = "dword"
getSizeSpec IRU64 = "qword"
getSizeSpec IRChar = "byte"
getSizeSpec IRBool = "byte"
getSizeSpec IRF32 = "dword"
getSizeSpec IRF64 = "qword"
getSizeSpec (IRPtr _) = "qword"
getSizeSpec _ = "qword"
