module Rune.Backend.X86_64.Codegen
  ( emitAssembly,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Rune.Backend.Helpers (calculateStackMap, collectTopLevels, emit, escapeString)
import Rune.Backend.Types (Extern, Function, GlobalString, GlobalFloat)
import Rune.Backend.X86_64.Compare (emitCompare)
import qualified Rune.Backend.X86_64.Compare as Cmp
import Rune.Backend.X86_64.Registers (getMovType, getRegisterName, getSizeSpecifier, x86_64ArgsRegisters, x86_64FloatArgsRegisters)
import Rune.IR.Nodes
  ( IRFunction (IRFunction),
    IRInstruction (..),
    IRLabel (IRLabel),
    IROperand (..),
    IRProgram (IRProgram),
    IRType (..),
  )
import Rune.IR.IRHelpers (getOperandType)

--
-- public
--

-- explanation
-- Emit full assembly including externs, literals in .rodata, text section, and GNU-stack note
emitAssembly :: IRProgram -> String
emitAssembly (IRProgram _ topLevels) =
  let (externs, globalStrings, globalFloats, functions) = collectTopLevels topLevels
   in unlines $
        emitExterns externs
          ++ emitRoDataSection globalStrings globalFloats
          ++ emitTextSection functions
          ++ [ "    ; this section is to remove the gcc GNU related warning",
               "section .note.GNU-stack noalloc noexec nowrite"
             ]
-- old code commented out
-- emitAssembly :: IRProgram -> String
-- emitAssembly (IRProgram _ topLevels) =
--   let (externs, globalStrings, functions) = collectTopLevels topLevels
--    in unlines $
--         emitExterns externs
--           ++ emitDataSection globalStrings
--           ++ emitTextSection functions

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
-- # explanation
-- Emit both string and float literals into a single .rodata section
emitRoDataSection :: [GlobalString] -> [GlobalFloat] -> [String]
emitRoDataSection [] [] = []
emitRoDataSection gs fs = "section .rodata" : (map emitStr gs ++ map emitFloat fs)
  where
    emitStr (name, val) = name ++ " db " ++ escapeString val ++ ", 0"
    emitFloat (name, val, typ) =
      let dir = case typ of
            IRF32 -> "dd"
            IRF64 -> "dq"
            _ -> "dd"
       in name ++ ": " ++ dir ++ " " ++ show val
-- old code commented out
-- -- | emit global strings
-- -- <name> db "<value>", 0
-- -- # explanation
-- -- Emit string literals in the read-only .rodata section alongside float literals
-- emitDataSection :: [GlobalString] -> [String]
-- emitDataSection [] = []
-- emitDataSection gs = "section .rodata" : map emitGlobal gs
--   where
--     emitGlobal (name, val) = name ++ " db " ++ escapeString val ++ ", 0"
--
-- -- | emit global float literals in read-only data
-- -- <label>: dd <value>   ; IRF32
-- -- <label>: dq <value>   ; IRF64
-- emitRoDataSection :: [GlobalFloat] -> [String]
-- emitRoDataSection [] = []
-- emitRoDataSection fs = "section .rodata" : map emitFloat fs
--   where
--     emitFloat (name, val, typ) =
--       let dir = case typ of
--             IRF32 -> "dd"
--             IRF64 -> "dq"
--             _ -> "dd"
--        in name ++ ": " ++ dir ++ " " ++ show val

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
-- explanation
-- Store incoming parameters from integer and float argument registers according to SysV AMD64 ABI
emitParameters params stackMap =
  let (instrs, _, _) = foldl step ([], 0, 0) params
   in instrs
  where
    step (acc, intIdx, floatIdx) (irName, t)
      | isFloatType t && floatIdx < length x86_64FloatArgsRegisters =
          let xmmReg = x86_64FloatArgsRegisters !! floatIdx
              sizeSpec = getSizeSpecifier t
              storeInstr =
                case t of
                  IRF32 -> emit 1 $ "movss " ++ sizeSpec ++ " " ++ stackAddr stackMap irName ++ ", " ++ xmmReg
                  IRF64 -> emit 1 $ "movsd " ++ sizeSpec ++ " " ++ stackAddr stackMap irName ++ ", " ++ xmmReg
                  _ -> emit 1 $ "; TODO: unsupported float param type: " ++ show t
           in (acc ++ [storeInstr], intIdx, floatIdx + 1)
      | not (isFloatType t) && intIdx < length x86_64ArgsRegisters =
          let reg = x86_64ArgsRegisters !! intIdx
              storeInstr = storeReg stackMap irName reg t
           in (acc ++ [storeInstr], intIdx + 1, floatIdx)
      | otherwise = (acc, intIdx, floatIdx)
-- old code commented out
-- emitParameters :: [(String, IRType)] -> Map String Int -> [String]
-- emitParameters params stackMap =
--   let argRegs = x86_64ArgsRegisters
--       indexedParams = zip [0 ..] params
--    in mapMaybe (emitParam stackMap argRegs) indexedParams
--
-- emitParam :: Map String Int -> [String] -> (Int, (String, IRType)) -> Maybe String
-- emitParam sm regs (idx, (irName, t))
--   | idx < length regs = Just $ storeReg sm irName (regs !! idx) t
--   | otherwise = Nothing

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
-- explanation
-- Load interned f32 constants via the first SysV float-arg register instead of hard-coded xmm0
emitAssign sm dest (IRGlobal name IRF32) IRF32 =
  case x86_64FloatArgsRegisters of
    reg : _ ->
      [ emit 1 $ "movss " ++ reg ++ ", dword [rel " ++ name ++ "]",
        emit 1 $ "movss dword " ++ stackAddr sm dest ++ ", " ++ reg
      ]
    _ ->
      [ emit 1 $ "movss xmm0, dword [rel " ++ name ++ "]",
        emit 1 $ "movss dword " ++ stackAddr sm dest ++ ", xmm0"
      ]
-- old code commented out
-- emitAssign sm dest (IRGlobal name IRF32) IRF32 =
--   [ emit 1 $ "movss xmm0, dword [rel " ++ name ++ "]",
--     emit 1 $ "movss dword " ++ stackAddr sm dest ++ ", xmm0"
--   ]
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
  [ emit 1 $ "; WARNING: Unsupported IRASSIGN operand: " ++ show op,
    emit 1 $ "mov " ++ getSizeSpecifier t ++ " " ++ stackAddr sm dest ++ ", 0"
  ]

-- | emit call dest = funcName(args)
-- explanation
-- Route integer/pointer args into GPRs, float args into XMM registers, and honor printf float promotion
emitCall :: Map String Int -> String -> String -> [IROperand] -> Maybe IRType -> [String]
emitCall sm dest funcName args mbType =
  let argSetup = setupCallArgs sm args
      printfFixup =
        if funcName == "printf"
           && any (\op -> maybe False isFloatType (getOperandType op)) args
          then
            -- explanation
            -- Promote first float arg using the first SysV float-arg register instead of literal xmm0
            case x86_64FloatArgsRegisters of
              reg0 : _ ->
                [ emit 1 $ "cvtss2sd " ++ reg0 ++ ", " ++ reg0
                , emit 1 $ "mov eax, 1"
                ]
              _ ->
                [ emit 1 "cvtss2sd xmm0, xmm0"
                , emit 1 $ "mov eax, 1"
                ]
          else []

      callInstr = [emit 1 $ "call " ++ funcName]
      retSave = saveCallResult sm dest mbType
   in argSetup ++ printfFixup ++ callInstr ++ retSave

-- explanation
-- Assign call arguments to integer or float registers based on operand IRType
setupCallArgs :: Map String Int -> [IROperand] -> [String]
setupCallArgs sm args =
  let (instrs, _, _) = foldl step ([], 0, 0) args
   in instrs
  where
    step (acc, intIdx, floatIdx) op =
      case getOperandType op of
        Just t | isFloatType t && floatIdx < length x86_64FloatArgsRegisters ->
          let xmmReg = x86_64FloatArgsRegisters !! floatIdx
              loadInstrs = loadFloatOperand sm xmmReg op t
           in (acc ++ loadInstrs, intIdx, floatIdx + 1)
        Just t | not (isFloatType t) && intIdx < length x86_64ArgsRegisters ->
          let reg = x86_64ArgsRegisters !! intIdx
              loadInstrs = loadRegWithExt sm (reg, op)
           in (acc ++ loadInstrs, intIdx + 1, floatIdx)
        _ ->
          (acc ++ loadRegWithExt sm ("rax", op), intIdx, floatIdx)

saveCallResult :: Map String Int -> String -> Maybe IRType -> [String]
saveCallResult _ "" _ = []
-- explanation
-- Store float return values from xmm0 and integer/pointer returns from rax
saveCallResult sm dest (Just t)
  | isFloatType t =
      case t of
        IRF32 ->
          case x86_64FloatArgsRegisters of
            retXmm : _ ->
              [emit 1 $ "movss dword " ++ stackAddr sm dest ++ ", " ++ retXmm]
            _ ->
              [emit 1 $ "movss dword " ++ stackAddr sm dest ++ ", xmm0"]
        IRF64 ->
          case x86_64FloatArgsRegisters of
            retXmm : _ ->
              [emit 1 $ "movsd qword " ++ stackAddr sm dest ++ ", " ++ retXmm]
            _ ->
              [emit 1 $ "movsd qword " ++ stackAddr sm dest ++ ", xmm0"]
        _ -> [emit 1 $ "; TODO: unsupported float return type: " ++ show t]
  | otherwise = [storeReg sm dest "rax" t]
saveCallResult sm dest Nothing = [emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"]
-- old code commented out
-- -- | emit call dest = funcName(args)
-- emitCall :: Map String Int -> String -> String -> [IROperand] -> Maybe IRType -> [String]
-- emitCall sm dest funcName args mbType =
--   let argSetup = concatMap (loadRegWithExt sm) (zip x86_64ArgsRegisters args)
--       callInstr = [emit 1 $ "call " ++ funcName]
--       retSave = saveCallResult sm dest mbType
--    in argSetup ++ callInstr ++ retSave
--
-- saveCallResult :: Map String Int -> String -> Maybe IRType -> [String]
-- saveCallResult _ "" _ = []
-- saveCallResult sm dest (Just t) = [storeReg sm dest "rax" t]
-- saveCallResult sm dest Nothing = [emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"]

-- | emit a return instruction
--  cases:
--  1- no return value: xor rax, rax to avoid returning garbage value
--  2- return value: load into rax
emitRet :: Map String Int -> String -> Maybe IROperand -> [String]
-- explanation
-- Return float values via xmm0 and integer/pointer values via rax
emitRet _ endLbl Nothing = [emit 1 "xor rax, rax", emit 1 $ "jmp " ++ endLbl]
emitRet sm endLbl (Just op) =
  case getOperandType op of
    Just t | isFloatType t ->
      case x86_64FloatArgsRegisters of
        retXmm : _ ->
          loadFloatOperand sm retXmm op t ++ [emit 1 $ "jmp " ++ endLbl]
        _ ->
          loadFloatOperand sm "xmm0" op t ++ [emit 1 $ "jmp " ++ endLbl]
    _ ->
      loadReg sm "rax" op ++ [emit 1 $ "jmp " ++ endLbl]
-- old code commented out
-- emitRet :: Map String Int -> String -> Maybe IROperand -> [String]
-- emitRet _ endLbl Nothing = [emit 1 "xor rax, rax", emit 1 $ "jmp " ++ endLbl]
-- emitRet sm endLbl (Just op) = loadReg sm "rax" op ++ [emit 1 $ "jmp " ++ endLbl]

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
-- explanation
-- Use SSE for float arithmetic and GPRs for integer/bitwise operations
emitBinaryOp sm dest asmOp leftOp rightOp t
  | isFloatType t = emitFloatBinaryOp sm dest asmOp leftOp rightOp t
  | otherwise =
      let regL = getRegisterName "rax" t
          regR = getRegisterName "rbx" t
       in loadReg sm "rax" leftOp
            ++ loadReg sm "rbx" rightOp
            ++ [emit 1 $ asmOp ++ " " ++ regL ++ ", " ++ regR]
            ++ [storeReg sm dest "rax" t]

-- explanation
-- Map IR float binary ops to scalar SSE instructions and store results back to the stack
emitFloatBinaryOp :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
emitFloatBinaryOp sm dest asmOp leftOp rightOp t =
  let (xmmL, xmmR) =
        case x86_64FloatArgsRegisters of
          r0 : r1 : _ -> (r0, r1)
          _ -> ("xmm0", "xmm1")
      loadL = loadFloatOperand sm xmmL leftOp t
      loadR = loadFloatOperand sm xmmR rightOp t
      opInstr =
        case (asmOp, t) of
          ("add", IRF32) -> "addss"
          ("add", IRF64) -> "addsd"
          ("sub", IRF32) -> "subss"
          ("sub", IRF64) -> "subsd"
          ("imul", IRF32) -> "mulss"
          ("imul", IRF64) -> "mulsd"
          _ -> "movss"
      storeInstr =
        case t of
          IRF32 -> emit 1 $ "movss dword " ++ stackAddr sm dest ++ ", " ++ xmmL
          IRF64 -> emit 1 $ "movsd qword " ++ stackAddr sm dest ++ ", " ++ xmmL
          _ -> emit 1 $ "; TODO: unsupported float binary result type: " ++ show t
   in loadL
        ++ loadR
        ++ [emit 1 $ opInstr ++ " " ++ xmmL ++ ", " ++ xmmR]
        ++ [storeInstr]
-- old code commented out
-- emitBinaryOp :: Map String Int -> String -> String -> IROperand -> IROperand -> IRType -> [String]
-- emitBinaryOp sm dest asmOp leftOp rightOp t =
--   let regL = getRegisterName "rax" t
--       regR = getRegisterName "rbx" t
--    in loadReg sm "rax" leftOp
--         ++ loadReg sm "rbx" rightOp
--         ++ [emit 1 $ asmOp ++ " " ++ regL ++ ", " ++ regR]
--         ++ [storeReg sm dest "rax" t]

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
varStackAddr sm (IRTemp name _) = stackAddr sm name
varStackAddr sm (IRParam name _) = stackAddr sm name
varStackAddr _ op = error $ "Unsupported IROperand for stack address: " ++ show op

-- | emit: mov <reg>, <operand> with proper sign/zero extension for arguments
loadRegWithExt :: Map String Int -> (String, IROperand) -> [String]
loadRegWithExt _ (reg, IRConstInt n) = [emit 1 $ "mov " ++ reg ++ ", " ++ show n]
loadRegWithExt _ (reg, IRConstChar c) = [emit 1 $ "mov " ++ reg ++ ", " ++ show (fromEnum c)]
loadRegWithExt _ (reg, IRConstNull) = [emit 1 $ "mov " ++ reg ++ ", 0"]
loadRegWithExt _ (reg, IRConstBool b) = [emit 1 $ "mov " ++ reg ++ ", " ++ if b then "1" else "0"]
loadRegWithExt _ (reg, IRGlobal name _) = [emit 1 $ "mov " ++ reg ++ ", " ++ name]
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

-- explanation
-- Load float operands into xmm registers from stack slots or interned .rodata literals
loadFloatOperand :: Map String Int -> String -> IROperand -> IRType -> [String]
loadFloatOperand sm reg (IRTemp name _) IRF32 =
  [emit 1 $ "movss " ++ reg ++ ", dword " ++ stackAddr sm name]
loadFloatOperand sm reg (IRParam name _) IRF32 =
  [emit 1 $ "movss " ++ reg ++ ", dword " ++ stackAddr sm name]
loadFloatOperand sm reg (IRTemp name _) IRF64 =
  [emit 1 $ "movsd " ++ reg ++ ", qword " ++ stackAddr sm name]
loadFloatOperand sm reg (IRParam name _) IRF64 =
  [emit 1 $ "movsd " ++ reg ++ ", qword " ++ stackAddr sm name]
loadFloatOperand _ reg (IRGlobal name IRF32) IRF32 =
  [emit 1 $ "movss " ++ reg ++ ", dword [rel " ++ name ++ "]"]
loadFloatOperand _ reg (IRGlobal name IRF64) IRF64 =
  [emit 1 $ "movsd " ++ reg ++ ", qword [rel " ++ name ++ "]"]
loadFloatOperand _ reg _ _ =
  [emit 1 $ "; WARNING: unsupported float operand, zeroing " ++ reg, emit 1 $ "xorps " ++ reg ++ ", " ++ reg]

-- explanation
-- Helper to detect float IR types used in backend lowering
isFloatType :: IRType -> Bool
isFloatType IRF32 = True
isFloatType IRF64 = True
isFloatType _ = False
