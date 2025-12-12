module Rune.Backend.X86_64.Codegen
  ( emitAssembly,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Rune.Backend.Helpers (calculateStackMap, collectTopLevels, emit, escapeString)
import Rune.Backend.Types (Extern, Function, Global)
import Rune.Backend.X86_64.Compare (emitCompare, loadFloatOperand, isFloatType)
import qualified Rune.Backend.X86_64.Compare as Cmp
import Rune.Backend.X86_64.Registers (getMovType, getRegisterName, getSizeSpecifier, x86_64ArgsRegisters, x86_64FloatArgsRegisters)
import Rune.IR.Nodes
  ( IRFunction (IRFunction),
    IRInstruction (..),
    IRLabel (IRLabel),
    IROperand (..),
    IRProgram (IRProgram),
    IRType (..),
    IRGlobalValue (..),
  )
import Rune.IR.IRHelpers (getOperandType)

--
-- public
--

emitAssembly :: IRProgram -> String
emitAssembly (IRProgram _ topLevels) =
  let (externs, globals, functions) = collectTopLevels topLevels
   in unlines $
        emitExterns externs
          ++ emitRoDataSection globals
          ++ emitTextSection functions
          ++ emitRmWarning

--
-- top level
--

-- | extern <function>
emitExterns :: [Extern] -> [String]
emitExterns [] = []
emitExterns xs = map ("extern " ++) xs

--
-- section .data / .rodata
--

-- | emit global strings and float literals in read-only data
-- <label>: db "<value>", 0   ; strings
-- <label>: dd/dq <value>     ; floats
emitRoDataSection :: [Global] -> [String]
emitRoDataSection [] = []
emitRoDataSection gs = "section .rodata" : map emitGlobal gs
  where
    emitGlobal (name, IRGlobalStringVal val) = name ++ " db " ++ escapeString val ++ ", 0"
    emitGlobal (name, IRGlobalFloatVal val IRF32) = name ++ " dd " ++ " " ++ show val
    emitGlobal (name, IRGlobalFloatVal val IRF64) = name ++ " dq " ++ " " ++ show val
    emitGlobal (name, IRGlobalFloatVal val _)     = name ++ " dd " ++ " " ++ show val

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
  [ "global " ++ name
  , name ++ ":"
  , emit 1 "push rbp"
  , emit 1 "mov rbp, rsp"
  , emit 1 $ "sub rsp, " ++ show frameSize
  ]

emitFunctionEpilogue :: String -> [String]
emitFunctionEpilogue endLabel =
  [ endLabel ++ ":"
  , emit 1 "mov rsp, rbp"
  , emit 1 "pop rbp"
  , emit 1 "ret"
  , ""
  ]

-- | emit function parameters
emitParameters :: [(String, IRType)] -> Map String Int -> [String]
emitParameters params stackMap =
  let (instrs, _, _) = foldl step ([], 0, 0) params
   in instrs
  where
    getStoreInstr sizeSpec irName xmmReg IRF32 = emit 1 $ "movss " ++ sizeSpec ++ " " ++ stackAddr stackMap irName ++ ", " ++ xmmReg
    getStoreInstr sizeSpec irName xmmReg IRF64 = emit 1 $ "movsd " ++ sizeSpec ++ " " ++ stackAddr stackMap irName ++ ", " ++ xmmReg
    getStoreInstr _ _ _ t                      = emit 1 $ "; TODO: unsupported float param type: " ++ show t

    step (acc, intIdx, floatIdx) (irName, t)
      | isFloatType t && floatIdx < length x86_64FloatArgsRegisters =
          let xmmReg = x86_64FloatArgsRegisters !! floatIdx
              sizeSpec = getSizeSpecifier t
              storeInstr = getStoreInstr irName xmmReg t
           in (acc ++ [storeInstr], intIdx, floatIdx + 1)
      | not (isFloatType t) && intIdx < length x86_64ArgsRegisters =
          let reg = x86_64ArgsRegisters !! intIdx
              storeInstr = storeReg stackMap irName reg t
           in (acc ++ [storeInstr], intIdx + 1, floatIdx)
      | otherwise = (acc, intIdx, floatIdx)

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
emitInstruction sm _ (IRCMP_EQ dest l r) = emitCompare sm dest Cmp.CmpEQ l r
emitInstruction sm _ (IRCMP_NEQ dest l r) = emitCompare sm dest Cmp.CmpNEQ l r
emitInstruction sm _ (IRCMP_LT dest l r) = emitCompare sm dest Cmp.CmpLT l r
emitInstruction sm _ (IRCMP_LTE dest l r) = emitCompare sm dest Cmp.CmpLTE l r
emitInstruction sm _ (IRCMP_GT dest l r) = emitCompare sm dest Cmp.CmpGT l r
emitInstruction sm _ (IRCMP_GTE dest l r) = emitCompare sm dest Cmp.CmpGTE l r
emitInstruction _ _ instr = [emit 1 $ "; TODO: " ++ show instr]

-- | emit dest = op
emitAssign :: Map String Int -> String -> IROperand -> IRType -> [String]
emitAssign sm dest (IRConstInt n) t
  | needsRegisterLoad n t = [emit 1 $ "mov rax, " ++ show n, storeReg sm dest "rax" t]
  | otherwise = [emit 1 $ "mov " ++ getSizeSpecifier t ++ " " ++ stackAddr sm dest ++ ", " ++ show n]
emitAssign sm dest (IRGlobal name IRF32) IRF32 =
  case x86_64FloatArgsRegisters of
    []      -> [ emit 1 $ "; WARNING: no more float register" ]
    (reg:_) -> [ emit 1 $ "movss " ++ reg ++ ", dword [rel " ++ name ++ "]"
               , emit 1 $ "movss dword " ++ stackAddr sm dest ++ ", " ++ reg
               ]
emitAssign sm dest (IRConstChar c) _ =
  [emit 1 $ "mov byte " ++ stackAddr sm dest ++ ", " ++ show (fromEnum c)]
emitAssign sm dest (IRConstBool b) _ =
  [emit 1 $ "mov byte " ++ stackAddr sm dest ++ ", " ++ if b then "1" else "0"]
emitAssign sm dest IRConstNull t =
  [emit 1 $ "mov " ++ getSizeSpecifier t ++ " " ++ stackAddr sm dest ++ ", 0"]
emitAssign sm dest (IRGlobal name _) t =
  [emit 1 $ "mov rax, " ++ name, storeReg sm dest "rax" t]
emitAssign sm dest (IRTemp name _) t = moveStackToStack sm dest name t
emitAssign sm dest (IRParam name _) t = moveStackToStack sm dest name t
emitAssign sm dest op t =
  [ emit 1 $ "; WARNING: Unsupported IRASSIGN operand: " ++ show op
  , emit 1 $ "mov " ++ getSizeSpecifier t ++ " " ++ stackAddr sm dest ++ ", 0"
  ]

-- | emit call dest = funcName(args)
emitCall :: Map String Int -> String -> String -> [IROperand] -> Maybe IRType -> [String]
emitCall sm dest funcName args mbType =
  let argSetup    = setupCallArgs sm args
      hasFloatArg = any (maybe False isFloatType . getOperandType) args
      printfFixup = printfFixupHelp hasFloatArg x86_64FloatArgsRegisters
      callInstr   = [emit 1 $ "call " ++ funcName]
      retSave     = saveCallResult sm dest mbType
   in argSetup ++ printfFixup ++ callInstr ++ retSave
  where
    printfFixupHelp hasFloatArg (floatReg:_)
      | funcName == "printf" && hasFloatArg =
        [ emit 1 $ "cvtss2sd " ++ floatReg ++ ", " ++ floatReg
        , emit 1   "mov eax, 1"
        ]
      | otherwise = []
    printfFixupHelp _ _
      = [ emit 1 $ "; WARNING: no more float register" ]

setupCallArgs :: Map String Int -> [IROperand] -> [String]
setupCallArgs sm args =
  let (instrs, _, _) = foldl step ([], 0, 0) args
   in instrs
  where
    step :: ([String], Int, Int) -> IROperand -> ([String], Int, Int)
    step (acc, intIdx, floatIdx) op =
      let mt = getOperandType op
       in stepType (acc, intIdx, floatIdx) op mt

    stepType (acc, intIdx, floatIdx) _ Nothing
      = (acc, intIdx, floatIdx)
    stepType (acc, intIdx, floatIdx) op (Just t)
      | isFloatType t , floatIdx < length x86_64FloatArgsRegisters =
          let xmm = x86_64FloatArgsRegisters !! floatIdx
          in (acc ++ loadFloatOperand sm xmm op t , intIdx , floatIdx + 1)
      | not (isFloatType t) , intIdx < length x86_64ArgsRegisters =
          let reg = x86_64ArgsRegisters !! intIdx
          in (acc ++ loadRegWithExt sm (reg, op) , intIdx + 1 , floatIdx)
      | otherwise = (acc, intIdx, floatIdx)

saveCallResult :: Map String Int -> String -> Maybe IRType -> [String]
saveCallResult _ "" _           = []
saveCallResult sm dest Nothing  = [emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"]
saveCallResult sm dest (Just t)
  | isFloatType t = saveFloat t x86_64FloatArgsRegisters
  | otherwise     = [ storeReg sm dest "rax" t ]
  where

    saveFloat IRF32 (xmmRet:_) = [ emit 1 $ "movss dword " ++ stackAddr sm dest ++ ", " ++ xmmRet ]
    saveFloat IRF64 (xmmRet:_) = [ emit 1 $ "movsd qword " ++ stackAddr sm dest ++ ", " ++ xmmRet ]
    saveFloat other _          = [ emit 1 $ "; TODO: unsupported float return type: " ++ show other ]


-- | emit a return instruction
--  cases:
--  1- no return value: xor rax, rax to avoid returning garbage value
--  2- return value: load into the appropriate return register
emitRet :: Map String Int -> String -> Maybe IROperand -> [String]
emitRet _ endLbl Nothing = [emit 1 "xor rax, rax", emit 1 $ "jmp " ++ endLbl]
emitRet sm endLbl (Just op) = emitRetHelper $ getOperandType op
  where
    getReg = loadReg sm "rax" op ++ [emit 1 $ "jmp " ++ endLbl]

    getFloatReg t (xmmRet:_) = loadFloatOperand sm xmmRet op t ++ [emit 1 $ "jmp " ++ endLbl]
    getFloatReg t []         = [ emit 1 $ "; WARNING: no float return register available"
                               ] ++ loadFloatOperand sm "xmm0" op t ++ [emit 1 $ "jmp " ++ endLbl]

    emitRetHelper (Just IRNull)
      = [ emit 1 "xor rax, rax", emit 1 $ "jmp " ++ endLbl ]
    emitRetHelper (Just t)
      | isFloatType t = getFloatReg t x86_64FloatArgsRegisters
    emitRetHelper _ = getReg


-- | emit deref ptr
--  cases:
--  1- load pointer address into rax
--  2- deref based on type size into rax
--  3- store rax into dest stack location
emitDeref :: Map String Int -> String -> IROperand -> IRType -> [String]
emitDeref sm dest ptr typ =
  [ emit 1 $ "mov rax, qword " ++ operandAddr sm ptr
  , emit 1 $ getMovType typ ++ " [rax]"
  , storeReg sm dest "rax" typ
  ]

-- | emit INC/DEC on pointer or numeric operand
emitIncDec :: Map String Int -> IROperand -> String -> [String]
emitIncDec sm (IRTemp name t) asmOp = 
  let sizeSpec = getSizeSpecifier t
  in [emit 1 $ asmOp ++ " " ++ sizeSpec ++ " " ++ stackAddr sm name ++ ", 1"]
emitIncDec _ op _ = [emit 1 $ "; TODO: " ++ show op ++ " on non-temp/pointer"]

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
  loadRegWithExt sm ("rax", op)
    ++ [emit 1 $ "test " ++ getTestReg op ++ ", " ++ getTestReg op]
    ++ [emit 1 $ jumpInstr ++ " " ++ lbl]

-- | emit dest = left <asmOp> right
emitBinaryOp :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitBinaryOp sm dest asmOp leftOp rightOp t
  | isFloatType t = emitFloatBinaryOp sm dest asmOp leftOp rightOp t
  | otherwise =
      let regL = getRegisterName "rax" t
          regR = getRegisterName "rbx" t
       in loadReg sm "rax" leftOp
       ++ loadReg sm "rbx" rightOp
       ++ [emit 1 $ asmOp ++ " " ++ regL ++ ", " ++ regR]
       ++ [storeReg sm dest "rax" t]

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
    ++ load right xmmR t
    ++ [emit 1 (mnemonic t ++ " " ++ xmmL ++ ", " ++ xmmR)]
    ++ [store t]
  where

    (xmmL, xmmR) =
      case x86_64FloatArgsRegisters of
        r0 : r1 : _ -> (r0, r1)
        _           -> ("xmm0", "xmm1")   -- safe fallback; never used

    load op reg ty  = loadFloatOperand sm reg op ty

    left  = leftOp
    right = rightOp

    -- The SSE instruction names
    mnemonic IRF32 = prefix "ss"
    mnemonic IRF64 = prefix "sd"
    mnemonic _     = "movss"

    prefix suf =
      case asmOp of
        "add"  -> "add" ++ suf
        "sub"  -> "sub" ++ suf
        "imul" -> "mul" ++ suf
        _      -> "movss"

    -- Store result
    store IRF32 = emit 1 $ "movss dword " ++ stackAddr sm dest ++ ", " ++ xmmL
    store IRF64 = emit 1 $ "movsd qword " ++ stackAddr sm dest ++ ", " ++ xmmL
    store other = emit 1 $ "; TODO: unsupported float binary result type: " ++ show other

emitRmWarning :: [String]
emitRmWarning =
  [ "; this section is to remove the gcc GNU related warning" 
  , "section .note.GNU-stack noalloc noexec nowrite"
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
loadReg _ reg (IRGlobal name _) = [emit 1 $ "mov " ++ reg ++ ", " ++ name]
loadReg sm baseReg op@(IRTemp _ t) = loadVarReg sm baseReg op t
loadReg sm baseReg op@(IRParam _ t) = loadVarReg sm baseReg op t
loadReg sm baseReg op = [emit 1 $ "mov " ++ baseReg ++ ", qword " ++ varStackAddr sm op]

loadVarReg :: Map String Int -> String -> IROperand -> IRType -> [String]
loadVarReg sm baseReg op t =
  let reg = getRegisterName baseReg t
      sizeSpec = getSizeSpecifier t
   in [emit 1 $ "mov " ++ reg ++ ", " ++ sizeSpec ++ " " ++ varStackAddr sm op]

varStackAddr :: Map String Int -> IROperand -> String
varStackAddr sm (IRTemp  name _) = stackAddr sm name
varStackAddr sm (IRParam name _) = stackAddr sm name
varStackAddr _ op = error $ "Unsupported IROperand for stack address: " ++ show op

-- | emit: mov <reg>, <operand> with proper sign/zero extension for arguments
loadRegWithExt :: Map String Int -> (String, IROperand) -> [String]
loadRegWithExt _  (reg, IRConstInt n)     = [emit 1 $ "mov " ++ reg ++ ", " ++ show n]
loadRegWithExt _  (reg, IRConstChar c)    = [emit 1 $ "mov " ++ reg ++ ", " ++ show (fromEnum c)]
loadRegWithExt _  (reg, IRConstNull)      = [emit 1 $ "mov " ++ reg ++ ", 0"]
loadRegWithExt _  (reg, IRConstBool b)    = [emit 1 $ "mov " ++ reg ++ ", " ++ if b then "1" else "0"]
loadRegWithExt _  (reg, IRGlobal name _)  = [emit 1 $ "mov " ++ reg ++ ", " ++ name]
loadRegWithExt sm (reg, op@(IRTemp _ t))  = extendVar sm reg op t
loadRegWithExt sm (reg, op@(IRParam _ t)) = extendVar sm reg op t
loadRegWithExt sm (reg, op)               = [emit 1 $ "mov " ++ reg ++ ", qword " ++ varStackAddr sm op]

extendVar :: Map String Int -> String -> IROperand -> IRType -> [String]
extendVar sm reg op IRI8   = [emit 1 $ "movsx " ++ reg ++ ", byte " ++ varStackAddr sm op]
extendVar sm reg op IRI16  = [emit 1 $ "movsx " ++ reg ++ ", word " ++ varStackAddr sm op]
extendVar sm reg op IRI32  = [emit 1 $ "movsxd " ++ reg ++ ", dword " ++ varStackAddr sm op]
extendVar sm reg op IRU8   = [emit 1 $ "movzx " ++ reg ++ ", byte " ++ varStackAddr sm op]
extendVar sm reg op IRChar = [emit 1 $ "movzx " ++ reg ++ ", byte " ++ varStackAddr sm op]
extendVar sm reg op IRU16  = [emit 1 $ "movzx " ++ reg ++ ", word " ++ varStackAddr sm op]
extendVar sm reg op IRU32  = [emit 1 $ "mov " ++ getRegisterName reg IRU32 ++ ", dword " ++ varStackAddr sm op]
extendVar sm reg op IRBool = [emit 1 $ "movzx " ++ reg ++ ", byte " ++ varStackAddr sm op]
extendVar sm reg op t =
  let targetReg = getRegisterName reg t
      sizeSpec = getSizeSpecifier t
   in [emit 1 $ "mov " ++ targetReg ++ ", " ++ sizeSpec ++ " " ++ varStackAddr sm op]

