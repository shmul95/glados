module Rune.Backend.X86_64.Codegen
  ( emitAssembly,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Rune.Backend.Helpers (calculateStackMap, collectTopLevels, emit, escapeString)
import Rune.Backend.Types (Extern, Function, GlobalString)
import Rune.Backend.X86_64.Registers (getMovType, getRegisterName, getSizeSpecifier, x86_64ArgsRegisters)
import Rune.IR.Nodes
  ( IRFunction (IRFunction),
    IRInstruction (..),
    IRLabel (IRLabel),
    IROperand (..),
    IRProgram (IRProgram),
    IRType (..),
  )

--
-- public
--

emitAssembly :: IRProgram -> String
emitAssembly (IRProgram _ topLevels) =
  let (externs, globalStrings, functions) = collectTopLevels topLevels
   in unlines $
        emitExterns externs
          ++ emitDataSection globalStrings
          ++ emitTextSection functions

--
-- top level
--

-- | extern <function>
emitExterns :: [Extern] -> [String]
emitExterns [] = []
emitExterns xs = map ("extern " ++) xs

--
-- section .data
--

-- | emit global strings
-- <name> db "<value>", 0
emitDataSection :: [GlobalString] -> [String]
emitDataSection [] = []
emitDataSection gs = "section .data" : map emitGlobal gs
  where
    emitGlobal (name, val) = name ++ " db " ++ escapeString val ++ ", 0"

--
-- section .text
--

emitTextSection :: [Function] -> [String]
emitTextSection [] = []
emitTextSection fs = "section .text" : concatMap emitFunction fs

--
-- function emission
--

-- | emit function: prologue, parameter setup, body, epilogue
-- global <name>
-- <name>:
--     push rbp
--     mov rbp, rsp
--     sub rsp, <frame_size>
--     ...
-- .L.function_end_<name>:
--     mov rsp, rbp
--     pop rbp
--     ret
emitFunction :: Function -> [String]
emitFunction fn@(IRFunction name params _ body) =
  let (stackMap, frameSize) = calculateStackMap fn
      endLabel = ".L.function_end_" ++ name
      prologue = emitFunctionPrologue fn frameSize
      paramSetup = emitParameters params stackMap
      bodyInstrs = concatMap (emitInstruction stackMap endLabel) body
      epilogue = emitFunctionEpilogue endLabel
   in prologue ++ paramSetup ++ bodyInstrs ++ epilogue

emitFunctionPrologue :: Function -> Int -> [String]
emitFunctionPrologue (IRFunction name _ _ _) frameSize =
  [ "global " ++ name,
    name ++ ":",
    emit 1 "push rbp",
    emit 1 "mov rbp, rsp",
    emit 1 $ "sub rsp, " ++ show frameSize
  ]

emitFunctionEpilogue :: String -> [String]
emitFunctionEpilogue endLabel =
  [ endLabel ++ ":",
    emit 1 "mov rsp, rbp",
    emit 1 "pop rbp",
    emit 1 "ret",
    ""
  ]

-- | emit function parameters
emitParameters :: [(String, IRType)] -> Map String Int -> [String]
emitParameters params stackMap =
  let argRegs = x86_64ArgsRegisters
      indexedParams = zip [0 ..] params
   in mapMaybe (emitParam stackMap argRegs) indexedParams

emitParam :: Map String Int -> [String] -> (Int, (String, IRType)) -> Maybe String
emitParam sm regs (idx, (irName, t))
  | idx < length regs = Just $ storeReg sm irName (regs !! idx) t
  | otherwise = Nothing

--
-- instruction emission
--

-- | emit a single IR instruction to nasm
emitInstruction :: Map String Int -> String -> IRInstruction -> [String]
emitInstruction sm _ (IRASSIGN dest op t) = emitAssign sm dest op t
emitInstruction _ _ (IRLABEL (IRLabel lbl)) = [lbl ++ ":"]
emitInstruction _ _ (IRJUMP (IRLabel lbl)) = [emit 1 $ "jmp " ++ lbl]
emitInstruction sm _ (IRJUMP_EQ0 op (IRLabel lbl)) = emitConditionalJump sm op "je" lbl
emitInstruction sm _ (IRJUMP_FALSE op (IRLabel lbl)) = emitConditionalJump sm op "je" lbl
emitInstruction sm _ (IRJUMP_TRUE op (IRLabel lbl)) = emitConditionalJump sm op "jne" lbl
emitInstruction sm _ (IRCALL dest funcName args mbType) = emitCall sm dest funcName args mbType
emitInstruction sm endLbl (IRRET mbOp) = emitRet sm endLbl mbOp
emitInstruction sm _ (IRDEREF dest ptr typ) = emitDeref sm dest ptr typ
emitInstruction sm _ (IRINC op) = emitIncDec sm op "add"
emitInstruction sm _ (IRDEC op) = emitIncDec sm op "sub"
emitInstruction sm _ (IRADDR dest source typ) = emitAddr sm dest source typ
emitInstruction sm _ (IRADD_OP dest l r t) = emitBinaryOp sm dest "add" l r t
emitInstruction sm _ (IRSUB_OP dest l r t) = emitBinaryOp sm dest "sub" l r t
emitInstruction sm _ (IRMUL_OP dest l r t) = emitBinaryOp sm dest "imul" l r t
emitInstruction sm _ (IRAND_OP dest l r t) = emitBinaryOp sm dest "and" l r t
emitInstruction sm _ (IROR_OP dest l r t) = emitBinaryOp sm dest "or" l r t
emitInstruction sm _ (IRCMP_EQ dest l r) = emitCompareOp sm dest "sete" l r
emitInstruction sm _ (IRCMP_NEQ dest l r) = emitCompareOp sm dest "setne" l r
emitInstruction sm _ (IRCMP_LT dest l r) = emitCompareOp sm dest "setl" l r
emitInstruction sm _ (IRCMP_LTE dest l r) = emitCompareOp sm dest "setle" l r
emitInstruction sm _ (IRCMP_GT dest l r) = emitCompareOp sm dest "setg" l r
emitInstruction sm _ (IRCMP_GTE dest l r) = emitCompareOp sm dest "setge" l r
emitInstruction _ _ instr = [emit 1 $ "; TODO: " ++ show instr]

-- | emit dest = op
emitAssign :: Map String Int -> String -> IROperand -> IRType -> [String]
emitAssign sm dest (IRConstInt n) t
  | needsRegisterLoad n t = [emit 1 $ "mov rax, " ++ show n, storeReg sm dest "rax" t]
  | otherwise = [emit 1 $ "mov " ++ getSizeSpecifier t ++ " " ++ stackAddr sm dest ++ ", " ++ show n]
emitAssign sm dest (IRConstChar c) _ =
  [emit 1 $ "mov byte " ++ stackAddr sm dest ++ ", " ++ show (fromEnum c)]
emitAssign sm dest (IRConstBool b) _ =
  [emit 1 $ "mov byte " ++ stackAddr sm dest ++ ", " ++ if b then "1" else "0"]
emitAssign sm dest IRConstNull t =
  [emit 1 $ "mov " ++ getSizeSpecifier t ++ " " ++ stackAddr sm dest ++ ", 0"]
emitAssign sm dest (IRTemp name _) t = moveStackToStack sm dest name t
emitAssign sm dest (IRParam name _) t = moveStackToStack sm dest name t
emitAssign sm dest op t =
  [ emit 1 $ "; WARNING: Unsupported IRASSIGN operand: " ++ show op,
    emit 1 $ "mov " ++ getSizeSpecifier t ++ " " ++ stackAddr sm dest ++ ", 0"
  ]

-- | emit call dest = funcName(args)
emitCall :: Map String Int -> String -> String -> [IROperand] -> Maybe IRType -> [String]
emitCall sm dest funcName args mbType =
  let argSetup = concatMap (loadRegWithExt sm) (zip x86_64ArgsRegisters args)
      callInstr = [emit 1 $ "call " ++ funcName]
      retSave = saveCallResult sm dest mbType
   in argSetup ++ callInstr ++ retSave

saveCallResult :: Map String Int -> String -> Maybe IRType -> [String]
saveCallResult _ "" _ = []
saveCallResult sm dest (Just t) = [storeReg sm dest "rax" t]
saveCallResult sm dest Nothing = [emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"]

-- | emit a return instruction
--  cases:
--  1- no return value: xor rax, rax to avoid returning garbage value
--  2- return value: load into rax
emitRet :: Map String Int -> String -> Maybe IROperand -> [String]
emitRet _ endLbl Nothing = [emit 1 "xor rax, rax", emit 1 $ "jmp " ++ endLbl]
emitRet sm endLbl (Just op) = loadReg sm "rax" op ++ [emit 1 $ "jmp " ++ endLbl]

-- | emit deref ptr
--  cases:
--  1- load pointer address into rax
--  2- deref based on type size into rax
--  3- store rax into dest stack location
emitDeref :: Map String Int -> String -> IROperand -> IRType -> [String]
emitDeref sm dest ptr typ =
  [ emit 1 $ "mov rax, qword " ++ operandAddr sm ptr,
    emit 1 $ getMovType typ ++ " [rax]",
    storeReg sm dest "rax" typ
  ]

-- | emit INC/DEC on pointer operand
emitIncDec :: Map String Int -> IROperand -> String -> [String]
emitIncDec sm (IRTemp name _) asmOp = [emit 1 $ asmOp ++ " qword " ++ stackAddr sm name ++ ", 1"]
emitIncDec _ op _ = [emit 1 $ "; TODO: " ++ show op ++ " on non-pointer"]

-- | emit ADDR dest, source
--  cases:
--  1- source is a global string: load its address directly
--  2- source is a local variable: lea its address
emitAddr :: Map String Int -> String -> String -> IRType -> [String]
emitAddr sm dest source t
  | take 4 source == "str_" = [emit 1 $ "mov rax, " ++ source, storeReg sm dest "rax" t]
  | otherwise = [emit 1 $ "lea rax, " ++ stackAddr sm source, storeReg sm dest "rax" t]

-- | emit conditional jump based on test (zero/not-zero)
emitConditionalJump :: Map String Int -> IROperand -> String -> String -> [String]
emitConditionalJump sm op jumpInstr lbl =
  loadReg sm "rax" op
    ++ [emit 1 $ "test " ++ getTestReg op ++ ", " ++ getTestReg op]
    ++ [emit 1 $ jumpInstr ++ " " ++ lbl]

-- | emit dest = left <asmOp> right
emitBinaryOp :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitBinaryOp sm dest asmOp leftOp rightOp t =
  loadReg sm "rax" leftOp
    ++ loadReg sm "rbx" rightOp
    ++ [emit 1 $ asmOp ++ " rax, rbx"]
    ++ [storeReg sm dest "rax" t]

-- | emit dest = left <asmOp> right (comparison)
emitCompareOp :: Map String Int -> String -> String -> IROperand -> IROperand -> [String]
emitCompareOp sm dest setOp leftOp rightOp =
  loadReg sm "rax" leftOp
    ++ loadReg sm "rbx" rightOp
    ++ [ emit 1 "cmp rax, rbx",
         emit 1 $ setOp ++ " al",
         emit 1 "movzx eax, al",
         emit 1 $ "mov dword " ++ stackAddr sm dest ++ ", eax"
       ]

--
-- helpers
--

-- | get the nasm representation of an IR variable on the stack: [rbp<+->offset]
stackAddr :: Map String Int -> String -> String
stackAddr sm name = case Map.lookup name sm of
  Just offset -> "[rbp" ++ show offset ++ "]"
  Nothing -> error $ "Variable not found in stack map: " ++ name

-- | get the nasm representation of an operand: const_value | [rbp-offset]
operandAddr :: Map String Int -> IROperand -> String
operandAddr _ (IRConstInt n) = show n
operandAddr _ (IRConstChar c) = show $ fromEnum c
operandAddr _ IRConstNull = "0"
operandAddr _ (IRConstBool b) = if b then "1" else "0"
operandAddr sm (IRTemp name _) = stackAddr sm name
operandAddr sm (IRParam name _) = stackAddr sm name
operandAddr _ (IRGlobal name _) = error $ "Global operand should be loaded via ADDR/LOAD: " ++ name
operandAddr _ op = error $ "Unsupported IROperand for direct emission: " ++ show op

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
   in emit 1 $ "mov " ++ sizeSpec ++ " " ++ stackAddr sm dest ++ ", " ++ reg

-- | emit: mov reg, size [src]; mov size [dest], reg (with proper size)
moveStackToStack :: Map String Int -> String -> String -> IRType -> [String]
moveStackToStack sm dest src t =
  let sizeSpec = getSizeSpecifier t
      reg = getRegisterName "rax" t
   in [ emit 1 $ "mov " ++ reg ++ ", " ++ sizeSpec ++ " " ++ stackAddr sm src,
        emit 1 $ "mov " ++ sizeSpec ++ " " ++ stackAddr sm dest ++ ", " ++ reg
      ]

-- | emit: mov <reg>, <operand> (basic load without extension)
loadReg :: Map String Int -> String -> IROperand -> [String]
loadReg _ reg (IRConstInt n) = [emit 1 $ "mov " ++ reg ++ ", " ++ show n]
loadReg _ reg (IRConstChar c) = [emit 1 $ "mov " ++ reg ++ ", " ++ show (fromEnum c)]
loadReg _ reg IRConstNull = [emit 1 $ "mov " ++ reg ++ ", 0"]
loadReg _ reg (IRConstBool b) = [emit 1 $ "mov " ++ reg ++ ", " ++ if b then "1" else "0"]
loadReg sm baseReg op@(IRTemp _ t) = loadVarReg sm baseReg op t
loadReg sm baseReg op@(IRParam _ t) = loadVarReg sm baseReg op t
loadReg sm baseReg op = [emit 1 $ "mov " ++ baseReg ++ ", qword " ++ varStackAddr sm op]

loadVarReg :: Map String Int -> String -> IROperand -> IRType -> [String]
loadVarReg sm baseReg op t =
  let reg = getRegisterName baseReg t
      sizeSpec = getSizeSpecifier t
   in [emit 1 $ "mov " ++ reg ++ ", " ++ sizeSpec ++ " " ++ varStackAddr sm op]

varStackAddr :: Map String Int -> IROperand -> String
varStackAddr sm (IRTemp name _) = stackAddr sm name
varStackAddr sm (IRParam name _) = stackAddr sm name
varStackAddr _ op = error $ "Unsupported IROperand for stack address: " ++ show op

-- | emit: mov <reg>, <operand> with proper sign/zero extension for arguments
loadRegWithExt :: Map String Int -> (String, IROperand) -> [String]
loadRegWithExt _ (reg, IRConstInt n) = [emit 1 $ "mov " ++ reg ++ ", " ++ show n]
loadRegWithExt _ (reg, IRConstChar c) = [emit 1 $ "mov " ++ reg ++ ", " ++ show (fromEnum c)]
loadRegWithExt _ (reg, IRConstNull) = [emit 1 $ "mov " ++ reg ++ ", 0"]
loadRegWithExt _ (reg, IRConstBool b) = [emit 1 $ "mov " ++ reg ++ ", " ++ if b then "1" else "0"]
loadRegWithExt sm (reg, op@(IRTemp _ t)) = extendVar sm reg op t
loadRegWithExt sm (reg, op@(IRParam _ t)) = extendVar sm reg op t
loadRegWithExt sm (reg, op) = [emit 1 $ "mov " ++ reg ++ ", qword " ++ varStackAddr sm op]

extendVar :: Map String Int -> String -> IROperand -> IRType -> [String]
extendVar sm reg op IRI8 = [emit 1 $ "movsx " ++ reg ++ ", byte " ++ varStackAddr sm op]
extendVar sm reg op IRI16 = [emit 1 $ "movsx " ++ reg ++ ", word " ++ varStackAddr sm op]
extendVar sm reg op IRI32 = [emit 1 $ "movsxd " ++ reg ++ ", dword " ++ varStackAddr sm op]
extendVar sm reg op IRU8 = [emit 1 $ "movzx " ++ reg ++ ", byte " ++ varStackAddr sm op]
extendVar sm reg op IRChar = [emit 1 $ "movzx " ++ reg ++ ", byte " ++ varStackAddr sm op]
extendVar sm reg op IRU16 = [emit 1 $ "movzx " ++ reg ++ ", word " ++ varStackAddr sm op]
extendVar sm reg op IRU32 = [emit 1 $ "mov " ++ getRegisterName reg IRU32 ++ ", dword " ++ varStackAddr sm op]
extendVar sm reg op IRBool = [emit 1 $ "movzx " ++ reg ++ ", byte " ++ varStackAddr sm op]
extendVar sm reg op t =
  let targetReg = getRegisterName reg t
      sizeSpec = getSizeSpecifier t
   in [emit 1 $ "mov " ++ targetReg ++ ", " ++ sizeSpec ++ " " ++ varStackAddr sm op]
