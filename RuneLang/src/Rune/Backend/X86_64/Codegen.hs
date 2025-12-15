module Rune.Backend.X86_64.Codegen
  ( emitAssembly,
  )
where

import Data.Map.Strict (Map)

import Rune.Backend.Helpers (calculateStackMap, collectTopLevels, emit, escapeString)
import Rune.Backend.Types (Extern, Function, Global)
import Rune.Backend.X86_64.Compare (emitCompare, loadFloatOperand, isFloatType)
import qualified Rune.Backend.X86_64.Compare as Cmp
import Rune.Backend.X86_64.LoadStore (getTestReg, loadReg, loadRegWithExt, moveStackToStack, needsRegisterLoad, operandAddr, stackAddr, storeReg)
import Rune.Backend.X86_64.Operations (emitBinaryOp, emitDivOp, emitModOp)
import Rune.Backend.X86_64.Registers (getMovType, getSizeSpecifier, x86_64ArgsRegisters, x86_64FloatArgsRegisters)
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
          <> emitRoDataSection globals
          <> emitTextSection functions
          <> emitRmWarning

--
-- top level
--

-- | extern <function>
emitExterns :: [Extern] -> [String]
emitExterns [] = []
emitExterns xs = map ("extern " <>) xs

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
    emitGlobal (name, IRGlobalStringVal val) = name <> " db " <> escapeString val <> ", 0"
    emitGlobal (name, IRGlobalFloatVal val IRF32) = name <> " dd " <> " " <> show val
    emitGlobal (name, IRGlobalFloatVal val IRF64) = name <> " dq " <> " " <> show val
    emitGlobal (name, IRGlobalFloatVal val _)     = name <> " dd " <> " " <> show val

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
      endLabel = ".L.function_end_" <> name
      prologue = emitFunctionPrologue fn frameSize
      paramSetup = emitParameters params stackMap
      bodyInstrs = concatMap (emitInstruction stackMap endLabel) body
      epilogue = emitFunctionEpilogue endLabel
   in prologue <> paramSetup <> bodyInstrs <> epilogue

emitFunctionPrologue :: Function -> Int -> [String]
emitFunctionPrologue (IRFunction name _ _ _) frameSize =
  [ "global " <> name
  , name <> ":"
  , emit 1 "push rbp"
  , emit 1 "mov rbp, rsp"
  , emit 1 $ "sub rsp, " <> show frameSize
  ]

emitFunctionEpilogue :: String -> [String]
emitFunctionEpilogue endLabel =
  [ endLabel <> ":"
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
    getStoreInstr sizeSpec irName xmmReg IRF32 = emit 1 $ "movss " <> sizeSpec <> " " <> stackAddr stackMap irName <> ", " <> xmmReg
    getStoreInstr sizeSpec irName xmmReg IRF64 = emit 1 $ "movsd " <> sizeSpec <> " " <> stackAddr stackMap irName <> ", " <> xmmReg
    getStoreInstr _ _ _ t                      = emit 1 $ "; TODO: unsupported float param type: " <> show t

    step (acc, intIdx, floatIdx) (irName, t)
      | isFloatType t && floatIdx < length x86_64FloatArgsRegisters =
          let xmmReg = x86_64FloatArgsRegisters !! floatIdx
              sizeSpec = getSizeSpecifier t
              storeInstr = getStoreInstr sizeSpec irName xmmReg t
           in (acc <> [storeInstr], intIdx, floatIdx + 1)
      | not (isFloatType t) && intIdx < length x86_64ArgsRegisters =
          let reg = x86_64ArgsRegisters !! intIdx
              storeInstr = storeReg stackMap irName reg t
           in (acc <> [storeInstr], intIdx + 1, floatIdx)
      | otherwise = (acc, intIdx, floatIdx)

--
-- instruction emission
--

-- | emit a single IR instruction to nasm
emitInstruction :: Map String Int -> String -> IRInstruction -> [String]
emitInstruction sm _ (IRASSIGN dest op t) = emitAssign sm dest op t
emitInstruction _ _ (IRLABEL (IRLabel lbl)) = [lbl <> ":"]
emitInstruction _ _ (IRJUMP (IRLabel lbl)) = [emit 1 $ "jmp " <> lbl]
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
emitInstruction sm _ (IRDIV_OP dest l r t) = emitDivOp sm dest l r t
emitInstruction sm _ (IRMOD_OP dest l r t) = emitModOp sm dest l r t
emitInstruction sm _ (IRAND_OP dest l r t) = emitBinaryOp sm dest "and" l r t
emitInstruction sm _ (IROR_OP dest l r t) = emitBinaryOp sm dest "or" l r t
emitInstruction sm _ (IRCMP_EQ dest l r) = emitCompare sm dest Cmp.CmpEQ l r
emitInstruction sm _ (IRCMP_NEQ dest l r) = emitCompare sm dest Cmp.CmpNEQ l r
emitInstruction sm _ (IRCMP_LT dest l r) = emitCompare sm dest Cmp.CmpLT l r
emitInstruction sm _ (IRCMP_LTE dest l r) = emitCompare sm dest Cmp.CmpLTE l r
emitInstruction sm _ (IRCMP_GT dest l r) = emitCompare sm dest Cmp.CmpGT l r
emitInstruction sm _ (IRCMP_GTE dest l r) = emitCompare sm dest Cmp.CmpGTE l r
emitInstruction _ _ instr = [emit 1 $ "; TODO: " <> show instr]

-- | emit dest = op
emitAssign :: Map String Int -> String -> IROperand -> IRType -> [String]
emitAssign sm dest (IRConstInt n) t
  | needsRegisterLoad n t = [emit 1 $ "mov rax, " <> show n, storeReg sm dest "rax" t]
  | otherwise = [emit 1 $ "mov " <> getSizeSpecifier t <> " " <> stackAddr sm dest <> ", " <> show n]
emitAssign sm dest (IRGlobal name IRF32) IRF32 =
  case x86_64FloatArgsRegisters of
    []      -> [ emit 1 $ "movss xmm0, dword [rel " <> name <> "]"
               , emit 1 $ "movss dword " <> stackAddr sm dest <> ", xmm0"
               ]
    (reg:_) -> [ emit 1 $ "movss " <> reg <> ", dword [rel " <> name <> "]"
               , emit 1 $ "movss dword " <> stackAddr sm dest <> ", " <> reg
               ]
emitAssign sm dest (IRGlobal name IRF32) IRF64 =
  case x86_64FloatArgsRegisters of
    []      -> [ emit 1 $ "movss xmm0, dword [rel " <> name <> "]"
               , emit 1 $ "cvtss2sd xmm0, xmm0"
               , emit 1 $ "movsd qword " <> stackAddr sm dest <> ", xmm0"
               ]
    (reg:_) -> [ emit 1 $ "movss " <> reg <> ", dword [rel " <> name <> "]"
               , emit 1 $ "cvtss2sd " <> reg <> ", " <> reg
               , emit 1 $ "movsd qword " <> stackAddr sm dest <> ", " <> reg
               ]
emitAssign sm dest (IRGlobal name IRF64) IRF64 =
  case x86_64FloatArgsRegisters of
    []      -> [ emit 1 $ "movsd xmm0, qword [rel " <> name <> "]"
               , emit 1 $ "movsd qword " <> stackAddr sm dest <> ", xmm0"
               ]
    (reg:_) -> [ emit 1 $ "movsd " <> reg <> ", qword [rel " <> name <> "]"
               , emit 1 $ "movsd qword " <> stackAddr sm dest <> ", " <> reg
               ]
emitAssign sm dest (IRGlobal name IRF64) IRF32 =
  case x86_64FloatArgsRegisters of
    []      -> [ emit 1 $ "movsd xmm0, qword [rel " <> name <> "]"
               , emit 1 $ "cvtsd2ss xmm0, xmm0"
               , emit 1 $ "movss dword " <> stackAddr sm dest <> ", xmm0"
               ]
    (reg:_) -> [ emit 1 $ "movsd " <> reg <> ", qword [rel " <> name <> "]"
               , emit 1 $ "cvtsd2ss " <> reg <> ", " <> reg
               , emit 1 $ "movss dword " <> stackAddr sm dest <> ", " <> reg
               ]
emitAssign sm dest (IRConstChar c)   _ = [emit 1 $ "mov byte " <> stackAddr sm dest <> ", " <> show (fromEnum c)]
emitAssign sm dest (IRConstBool b)   _ = [emit 1 $ "mov byte " <> stackAddr sm dest <> ", " <> if b then "1" else "0"]
emitAssign sm dest  IRConstNull      t = [emit 1 $ "mov " <> getSizeSpecifier t <> " " <> stackAddr sm dest <> ", 0"]
emitAssign sm dest (IRGlobal name _) t = [emit 1 $ "mov rax, " <> name, storeReg sm dest "rax" t]
emitAssign sm dest (IRTemp name _)   t = moveStackToStack sm dest name t
emitAssign sm dest (IRParam name _)  t = moveStackToStack sm dest name t
emitAssign sm dest op t =
  [ emit 1 $ "; WARNING: Unsupported IRASSIGN operand: " <> show op
  , emit 1 $ "mov " <> getSizeSpecifier t <> " " <> stackAddr sm dest <> ", 0"
  ]

-- | emit call dest = funcName(args)
emitCall :: Map String Int -> String -> String -> [IROperand] -> Maybe IRType -> [String]
emitCall sm dest funcName args mbType =
  let argSetup    = setupCallArgs sm args
      firstFloatType =
        foldr
          (\op acc -> case (acc, getOperandType op) of
                        (Nothing, Just t) | isFloatType t -> Just t
                        _                                 -> acc)
          Nothing
          args
      printfFixup = printfFixupHelp firstFloatType x86_64FloatArgsRegisters
      callInstr   = [emit 1 $ "call " <> funcName]
      retSave     = saveCallResult sm dest mbType
   in argSetup <> printfFixup <> callInstr <> retSave
  where
    printfFixupHelp (Just IRF32) (floatReg:_)
      | funcName == "printf" =
        [ emit 1 $ "cvtss2sd " <> floatReg <> ", " <> floatReg
        , emit 1   "mov eax, 1"
        ]
    printfFixupHelp (Just IRF64) (_:_) 
      | funcName == "printf" =
        [ emit 1 "mov eax, 1" ]
    printfFixupHelp _ _
      | funcName == "printf" = []
    printfFixupHelp _ _ = []

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
          in (acc <> loadFloatOperand sm xmm op t , intIdx , floatIdx + 1)
      | not (isFloatType t) , intIdx < length x86_64ArgsRegisters =
          let reg = x86_64ArgsRegisters !! intIdx
          in (acc <> loadRegWithExt sm (reg, op) , intIdx + 1 , floatIdx)
      | otherwise = (acc, intIdx, floatIdx)

saveCallResult :: Map String Int -> String -> Maybe IRType -> [String]
saveCallResult _ "" _           = []
saveCallResult sm dest Nothing  = [emit 1 $ "mov qword " <> stackAddr sm dest <> ", rax"]
saveCallResult sm dest (Just t)
  | isFloatType t = saveFloat t x86_64FloatArgsRegisters
  | otherwise     = [ storeReg sm dest "rax" t ]
  where

    saveFloat IRF32 (xmmRet:_) = [ emit 1 $ "movss dword " <> stackAddr sm dest <> ", " <> xmmRet ]
    saveFloat IRF64 (xmmRet:_) = [ emit 1 $ "movsd qword " <> stackAddr sm dest <> ", " <> xmmRet ]
    saveFloat other _          = [ emit 1 $ "; TODO: unsupported float return type: " <> show other ]


-- | emit a return instruction
--  cases:
--  1- no return value: xor rax, rax to avoid returning garbage value
--  2- return value: load into the appropriate return register
emitRet :: Map String Int -> String -> Maybe IROperand -> [String]
emitRet _ endLbl Nothing = [emit 1 "xor rax, rax", emit 1 $ "jmp " <> endLbl]
emitRet sm endLbl (Just op) = emitRetHelper $ getOperandType op
  where
    getReg = loadReg sm "rax" op <> [emit 1 $ "jmp " <> endLbl]

    getFloatReg t (xmmRet:_) = loadFloatOperand sm xmmRet op t <> [emit 1 $ "jmp " <> endLbl]
    getFloatReg t []         = [ emit 1 $ "; WARNING: no float return register available"
                               ] <> loadFloatOperand sm "xmm0" op t <> [emit 1 $ "jmp " <> endLbl]

    emitRetHelper (Just IRNull)
      = [ emit 1 "xor rax, rax", emit 1 $ "jmp " <> endLbl ]
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
  [ emit 1 $ "mov rax, qword " <> operandAddr sm ptr
  , emit 1 $ getMovType typ <> " [rax]"
  , storeReg sm dest "rax" typ
  ]

-- | emit INC/DEC on pointer or numeric operand
emitIncDec :: Map String Int -> IROperand -> String -> [String]
emitIncDec sm (IRTemp name t) asmOp = 
  let sizeSpec = getSizeSpecifier t
  in [emit 1 $ asmOp <> " " <> sizeSpec <> " " <> stackAddr sm name <> ", 1"]
emitIncDec _ op _ = [emit 1 $ "; TODO: " <> show op <> " on non-temp/pointer"]

-- | emit ADDR dest, source
--  cases:
--  1- source is a global string: load its address directly
--  2- source is a local variable: lea its address
emitAddr :: Map String Int -> String -> String -> IRType -> [String]
emitAddr sm dest source t
  | take 4 source == "str_" = [emit 1 $ "mov rax, " <> source, storeReg sm dest "rax" t]
  | otherwise = [emit 1 $ "lea rax, " <> stackAddr sm source, storeReg sm dest "rax" t]

-- | emit conditional jump based on test (zero/not-zero)
emitConditionalJump :: Map String Int -> IROperand -> String -> String -> [String]
emitConditionalJump sm op jumpInstr lbl =
  loadRegWithExt sm ("rax", op)
    <> [emit 1 $ "test " <> getTestReg op <> ", " <> getTestReg op]
    <> [emit 1 $ jumpInstr <> " " <> lbl]

emitRmWarning :: [String]
emitRmWarning =
  [ "; this section is to remove the gcc GNU related warning" 
  , "section .note.GNU-stack noalloc noexec nowrite"
  ]

