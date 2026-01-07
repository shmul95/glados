{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Backend.X86_64.LoadStore (
  stackAddr,
  operandAddr,
  needsRegisterLoad,
  getTestReg,
  storeReg,
  moveStackToStack,
  loadReg,
  loadRegWithExt,
  varStackAddr,
  extendVar,
  loadVarReg,
) where
#else
module Rune.Backend.X86_64.LoadStore (
  stackAddr,
  operandAddr,
  needsRegisterLoad,
  getTestReg,
  storeReg,
  moveStackToStack,
  loadReg,
  loadRegWithExt,
) where
#endif

import Data.Map (Map)
import qualified Data.Map as Map
import Rune.Backend.Helpers (emit)
import Rune.Backend.X86_64.Registers (getRegisterName, getSizeSpecifier)
import Rune.IR.Nodes (IROperand (..), IRType (..))

--
-- public
--

-- | get the nasm representation of an IR variable on the stack: [rbp<+->offset]
stackAddr :: Map String Int -> String -> String
stackAddr sm name = case Map.lookup name sm of
  Just offset -> "[rbp" <> show offset <> "]"
  Nothing -> error $ "Variable not found in stack map: " <> name

-- | get the nasm representation of an operand: const_value | [rbp-offset]
operandAddr :: Map String Int -> IROperand -> String
operandAddr _ (IRConstInt n) = show n
operandAddr _ (IRConstChar c) = show $ fromEnum c
operandAddr _ IRConstNull = "0"
operandAddr _ (IRConstBool b) = if b then "1" else "0"
operandAddr sm (IRTemp name _) = stackAddr sm name
operandAddr sm (IRParam name _) = stackAddr sm name
operandAddr _ (IRGlobal name _) = error $ "Global operand should be loaded via ADDR/LOAD: " <> name
operandAddr _ op = error $ "Unsupported IROperand for direct emission: " <> show op

-- | check if immediate needs register loading
needsRegisterLoad :: Int -> IRType -> Bool
needsRegisterLoad n t =
  t `elem` [IRI64, IRU64] && (n < -2147483648 || n > 2147483647)

-- | helper to get register name for test instruction based on operand type
getTestReg :: IROperand -> String
getTestReg (IRTemp _ t) = getRegisterName "rax" t
getTestReg (IRParam _ t) = getRegisterName "rax" t
getTestReg _ = "rax"

-- | emit: mov size [dest], reg
storeReg :: Map String Int -> String -> String -> IRType -> String
storeReg sm dest baseReg t =
  let sizeSpec = getSizeSpecifier t
      reg = getRegisterName baseReg t
   in emit 1 $ "mov " <> sizeSpec <> " " <> stackAddr sm dest <> ", " <> reg

-- | emit: mov reg, size [src]; mov size [dest], reg (with proper size)
moveStackToStack :: Map String Int -> String -> String -> IRType -> [String]
moveStackToStack sm dest src t =
  let sizeSpec = getSizeSpecifier t
      reg = getRegisterName "rax" t
   in [ emit 1 $ "mov " <> reg <> ", " <> sizeSpec <> " " <> stackAddr sm src,
        emit 1 $ "mov " <> sizeSpec <> " " <> stackAddr sm dest <> ", " <> reg
      ]

varStackAddr :: Map String Int -> IROperand -> String
varStackAddr sm (IRTemp  name _) = stackAddr sm name
varStackAddr sm (IRParam name _) = stackAddr sm name
varStackAddr _ op = error $ "Unsupported IROperand for stack address: " <> show op

extendVar :: Map String Int -> String -> IROperand -> IRType -> [String]
extendVar sm reg op IRI8   = [emit 1 $ "movsx " <> reg <> ", byte " <> varStackAddr sm op]
extendVar sm reg op IRI16  = [emit 1 $ "movsx " <> reg <> ", word " <> varStackAddr sm op]
extendVar sm reg op IRI32  = [emit 1 $ "movsxd " <> reg <> ", dword " <> varStackAddr sm op]
extendVar sm reg op IRU8   = [emit 1 $ "movzx " <> reg <> ", byte " <> varStackAddr sm op]
extendVar sm reg op IRChar = [emit 1 $ "movzx " <> reg <> ", byte " <> varStackAddr sm op]
extendVar sm reg op IRU16  = [emit 1 $ "movzx " <> reg <> ", word " <> varStackAddr sm op]
extendVar sm reg op IRU32  = [emit 1 $ "mov " <> getRegisterName reg IRU32 <> ", dword " <> varStackAddr sm op]
extendVar sm reg op IRBool = [emit 1 $ "movzx " <> reg <> ", byte " <> varStackAddr sm op]
extendVar sm reg op t =
  let targetReg = getRegisterName reg t
      sizeSpec = getSizeSpecifier t
   in [emit 1 $ "mov " <> targetReg <> ", " <> sizeSpec <> " " <> varStackAddr sm op]

-- | emit: mov <reg>, <operand> (basic load without extension)
loadReg :: Map String Int -> String -> IROperand -> [String]
loadReg _ reg (IRConstInt n) = [emit 1 $ "mov " <> reg <> ", " <> show n]
loadReg _ reg (IRConstChar c) = [emit 1 $ "mov " <> reg <> ", " <> show (fromEnum c)]
loadReg _ reg IRConstNull = [emit 1 $ "mov " <> reg <> ", 0"]
loadReg _ reg (IRConstBool b) = [emit 1 $ "mov " <> reg <> ", " <> if b then "1" else "0"]
loadReg _ reg (IRGlobal name _) = [emit 1 $ "mov " <> reg <> ", " <> name]
loadReg sm baseReg op@(IRTemp _ t) = loadVarReg sm baseReg op t
loadReg sm baseReg op@(IRParam _ t) = loadVarReg sm baseReg op t
loadReg sm baseReg op = [emit 1 $ "mov " <> baseReg <> ", qword " <> varStackAddr sm op]

loadVarReg :: Map String Int -> String -> IROperand -> IRType -> [String]
loadVarReg sm baseReg op t =
  let reg = getRegisterName baseReg t
      sizeSpec = getSizeSpecifier t
   in [emit 1 $ "mov " <> reg <> ", " <> sizeSpec <> " " <> varStackAddr sm op]

-- | emit: mov <reg>, <operand> with proper sign/zero extension for arguments
loadRegWithExt :: Map String Int -> (String, IROperand) -> [String]
loadRegWithExt _  (reg, IRConstInt n)     = [emit 1 $ "mov " <> reg <> ", " <> show n]
loadRegWithExt _  (reg, IRConstChar c)    = [emit 1 $ "mov " <> reg <> ", " <> show (fromEnum c)]
loadRegWithExt _  (reg, IRConstNull)      = [emit 1 $ "mov " <> reg <> ", 0"]
loadRegWithExt _  (reg, IRConstBool b)    = [emit 1 $ "mov " <> reg <> ", " <> if b then "1" else "0"]
loadRegWithExt _  (reg, IRGlobal "stderr" _) = [emit 1 $ "mov " <> reg <> ", [rel stderr]"]
loadRegWithExt _  (reg, IRGlobal name _)  = [emit 1 $ "mov " <> reg <> ", " <> name]
loadRegWithExt sm (reg, op@(IRTemp _ t))  = extendVar sm reg op t
loadRegWithExt sm (reg, op@(IRParam _ t)) = extendVar sm reg op t
loadRegWithExt sm (reg, op)               = [emit 1 $ "mov " <> reg <> ", qword " <> varStackAddr sm op]
