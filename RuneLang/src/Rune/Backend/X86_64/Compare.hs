module Rune.Backend.X86_64.Compare
  ( emitCompare,
    getCompareSetOp,
    CompareOp (..),
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Rune.Backend.Helpers (emit)
import Rune.Backend.X86_64.Registers (getRegisterName)
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
  | otherwise = error $ "Invalid type for CmpLT comparison: " ++ show t
getCompareSetOp CmpLTE t
  | isSignedType t = "setle"
  | isUnsignedType t = "setbe"
  | isFloatType t = "setbe"
  | otherwise = error $ "Invalid type for CmpLTE comparison: " ++ show t
getCompareSetOp CmpGT t
  | isSignedType t = "setg"
  | isUnsignedType t = "seta"
  | isFloatType t = "seta"
  | otherwise = error $ "Invalid type for CmpGT comparison: " ++ show t
getCompareSetOp CmpGTE t
  | isSignedType t = "setge"
  | isUnsignedType t = "setae"
  | isFloatType t = "setae"
  | otherwise = error $ "Invalid type for CmpGTE comparison: " ++ show t

--
-- private
--

emitCompareInstructions :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitCompareInstructions sm dest setOp leftOp rightOp cmpType
  | isFloatType cmpType = emitFloatCompare sm dest setOp leftOp rightOp cmpType
  | otherwise = emitIntegerCompare sm dest setOp leftOp rightOp cmpType

emitIntegerCompare :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitIntegerCompare sm dest setOp leftOp rightOp cmpType =
  let regL = getRegisterName "rax" cmpType
      regR = getRegisterName "rbx" cmpType
      loadL = loadOperand sm "rax" leftOp cmpType
      loadR = loadOperand sm "rbx" rightOp cmpType
      stackLoc = stackAddr sm dest
   in loadL
        ++ loadR
        ++ [ emit 1 $ "cmp " ++ regL ++ ", " ++ regR,
             emit 1 $ setOp ++ " al",
             emit 1 $ "mov byte " ++ stackLoc ++ ", al"
           ]

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
        ++ loadR
        ++ [ emit 1 $ cmpInstr ++ " " ++ regL ++ ", " ++ regR,
             emit 1 $ setOp ++ " al",
             emit 1 $ "mov byte " ++ stackLoc ++ ", al"
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
getFloatRegs IRF32 = ("xmm0", "xmm1")
getFloatRegs IRF64 = ("xmm0", "xmm1")
getFloatRegs _ = ("xmm0", "xmm1")

stackAddr :: Map String Int -> String -> String
stackAddr sm name = case Map.lookup name sm of
  Just offset -> "[rbp" ++ show offset ++ "]"
  Nothing -> error $ "Variable not found in stack map: " ++ name

loadOperand :: Map String Int -> String -> IROperand -> IRType -> [String]
loadOperand _ reg (IRConstInt n) _ = [emit 1 $ "mov " ++ reg ++ ", " ++ show n]
loadOperand _ reg (IRConstChar c) _ = [emit 1 $ "mov " ++ reg ++ ", " ++ show (fromEnum c)]
loadOperand _ reg (IRConstBool b) _ = [emit 1 $ "mov " ++ reg ++ ", " ++ if b then "1" else "0"]
loadOperand sm baseReg (IRTemp name _) t =
  let reg = getRegisterName baseReg t
      sizeSpec = getSizeSpec t
   in [emit 1 $ "mov " ++ reg ++ ", " ++ sizeSpec ++ " " ++ stackAddr sm name]
loadOperand sm baseReg (IRParam name _) t =
  let reg = getRegisterName baseReg t
      sizeSpec = getSizeSpec t
   in [emit 1 $ "mov " ++ reg ++ ", " ++ sizeSpec ++ " " ++ stackAddr sm name]
loadOperand _ reg _ _ = [emit 1 $ "mov " ++ reg ++ ", 0"]

loadFloatOperand :: Map String Int -> String -> IROperand -> IRType -> [String]
loadFloatOperand sm reg (IRTemp name _) IRF32 =
  [emit 1 $ "movss " ++ reg ++ ", dword " ++ stackAddr sm name]
loadFloatOperand sm reg (IRParam name _) IRF32 =
  [emit 1 $ "movss " ++ reg ++ ", dword " ++ stackAddr sm name]
loadFloatOperand sm reg (IRTemp name _) IRF64 =
  [emit 1 $ "movsd " ++ reg ++ ", qword " ++ stackAddr sm name]
loadFloatOperand sm reg (IRParam name _) IRF64 =
  [emit 1 $ "movsd " ++ reg ++ ", qword " ++ stackAddr sm name]
loadFloatOperand _ reg _ _ = [emit 1 $ "xorps " ++ reg ++ ", " ++ reg]

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
