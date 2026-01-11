{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.Backend.X86_64.Codegen
  ( emitAssembly,
    emitAssemblyLib,
    emitExterns,
    emitRoDataSection,
    emitDataSection,
    emitTextSection,
    emitTextSectionLib,
    emitFunction,
    emitFunctionLib,
    emitFunctionPrologue,
    emitFunctionEpilogue,
    emitParameters,
    emitInstruction,
    emitAssign,
    emitCall,
    setupCallArgs,
    saveCallResult,
    emitRet,
    emitDeref,
    emitAllocArray,
    emitAllocArrayOnStack,
    emitGetElem,
    emitSetElem,
    emitIncDec,
    emitIncDecHelper,
    emitAddr,
    emitConditionalJump,
    emitDirectCmpJump,
    emitIntCmpJump,
    emitFloatCmpJump,
    emitRmWarning,
    collectStaticArrays,
    isStaticOperand,
    getDataDirective,
    showStaticOperand,
    commaSep
  )
where
#else
module Rune.Backend.X86_64.Codegen
  ( emitAssembly,
    emitAssemblyLib,
  )
where
#endif

import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

import Rune.Backend.Helpers (calculateStackMap, collectTopLevels, emit, escapeString)
import Rune.Backend.Types (Extern, Function, Global)
import Rune.Backend.X86_64.Compare (emitCompare, loadFloatOperand, isFloatType)
import qualified Rune.Backend.X86_64.Compare as Cmp
import Rune.Backend.X86_64.LoadStore (getTestReg, loadReg, loadRegWithExt, moveStackToStack, needsRegisterLoad, operandAddr, stackAddr, storeReg)
import Rune.Backend.X86_64.Operations (emitBinaryOp, emitDivOp, emitModOp, emitShiftOp, emitBitNot)
import Rune.Backend.X86_64.Registers (getMovType, getSizeSpecifier, getRegisterName, x86_64ArgsRegisters, x86_64FloatArgsRegisters)
import Rune.IR.Nodes
  ( IRFunction (IRFunction),
    IRInstruction (..),
    IRLabel (IRLabel),
    IROperand (..),
    IRProgram (IRProgram),
    IRType (..),
    IRGlobalValue (..),
  )
import Rune.IR.IRHelpers (getOperandType, sizeOfIRType)

--
-- public
--

emitAssembly :: IRProgram -> String
emitAssembly (IRProgram _ topLevels) =
  let (externs, globals, functions) = collectTopLevels topLevels
   in unlines $
        emitExterns externs
          <> emitRoDataSection globals
          <> emitDataSection functions
          <> emitTextSectionGen Nothing functions
          <> emitRmWarning

-- | Emit assembly for library code (PIC-compatible, uses PLT for external calls)
emitAssemblyLib :: IRProgram -> String
emitAssemblyLib (IRProgram _ topLevels) =
  let (externs, globals, functions) = collectTopLevels topLevels
   in unlines $
        emitExterns externs
          <> emitRoDataSection globals
          <> emitDataSection functions
          <> emitTextSectionGen (Just externs) functions
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
    emitGlobal (name, IRGlobalStringVal "") = name <> " db 0"  -- empty string is just null terminator
    emitGlobal (name, IRGlobalStringVal val) = name <> " db " <> escapeString val <> ", 0"
    emitGlobal (name, IRGlobalFloatVal val IRF32) = name <> " dd " <> " " <> show val
    emitGlobal (name, IRGlobalFloatVal val IRF64) = name <> " dq " <> " " <> show val
    emitGlobal (name, IRGlobalFloatVal val _)     = name <> " dd " <> " " <> show val

emitDataSection :: [Function] -> [String]
emitDataSection fs =
  let arrays = collectStaticArrays fs
   in case arrays of
        [] -> []
        _  -> "section .data" : concatMap emitArray arrays
  where
    emitArray (lbl, elemType, values) =
      let dir = getDataDirective elemType
          initVals = map (showStaticOperand elemType) values <> ["0"]
       in [lbl <> ": " <> dir <> " " <> commaSep initVals]

emitTextSection :: [Function] -> [String]
emitTextSection = emitTextSectionGen Nothing

-- | Emit text section for library (all exported functions are global, uses PLT)
emitTextSectionLib :: [Function] -> [Extern] -> [String]
emitTextSectionLib fs externs = emitTextSectionGen (Just externs) fs

emitTextSectionGen :: Maybe [Extern] -> [Function] -> [String]
emitTextSectionGen _ [] = []
emitTextSectionGen mbExterns fs = "section .text" : concatMap (emitFunctionGen mbExterns) fs

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
emitFunction = emitFunctionGen Nothing

-- | Emit function for library (exports marked with 'export' become global, uses PLT for externs)
emitFunctionLib :: [Extern] -> Function -> [String]
emitFunctionLib externs = emitFunctionGen $ Just externs

emitFunctionGen :: Maybe [Extern] -> Function -> [String]
emitFunctionGen mbExterns fn@(IRFunction name params _ body _) =
  let (stackMap, frameSize) = calculateStackMap fn
      endLabel = ".L.function_end_" <> name
      prologue = emitFunctionPrologueGen mbExterns fn frameSize
      paramSetup = emitParameters params stackMap
      bodyInstrs = concatMap (emitInstructionGen mbExterns stackMap endLabel name) body
      epilogue = emitFunctionEpilogue endLabel
   in prologue <> paramSetup <> bodyInstrs <> epilogue

emitFunctionPrologue :: Function -> Int -> [String]
emitFunctionPrologue = emitFunctionPrologueGen Nothing

emitFunctionPrologueGen :: Maybe [Extern] -> Function -> Int -> [String]
emitFunctionPrologueGen mbExterns (IRFunction name _ _ _ isExport) frameSize =
  let shouldExport = case mbExterns of
        Nothing -> isExport || name == "main"
        Just _  -> isExport
  in ["global " <> name <> ":function" | shouldExport]
    <> [ name <> ":"
       , emit 1 "push rbp"
       , emit 1 "mov rbp, rsp"
       ]
    <> [emit 1 ("sub rsp, " <> show frameSize) | frameSize > 0]

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
emitInstruction :: Map String Int -> String -> String -> IRInstruction -> [String]
emitInstruction = emitInstructionGen Nothing

emitInstructionGen :: Maybe [Extern] -> Map String Int -> String -> String -> IRInstruction -> [String]
emitInstructionGen _ sm _ _ (IRASSIGN dest op t) = emitAssign sm dest op t
emitInstructionGen _ _ _ _ (IRLABEL (IRLabel lbl)) = [lbl <> ":"]
emitInstructionGen _ _ _ _ (IRJUMP (IRLabel lbl)) = [emit 1 $ "jmp " <> lbl]
emitInstructionGen _ sm _ _ (IRJUMP_EQ0 op (IRLabel lbl)) = emitConditionalJump sm op "je" lbl
emitInstructionGen _ sm _ _ (IRJUMP_FALSE op (IRLabel lbl)) = emitConditionalJump sm op "je" lbl
emitInstructionGen _ sm _ _ (IRJUMP_TRUE op (IRLabel lbl)) = emitConditionalJump sm op "jne" lbl
emitInstructionGen _ sm _ _ (IRJUMP_LT o1 o2 (IRLabel lbl)) = emitDirectCmpJump sm o1 o2 "jl" lbl
emitInstructionGen _ sm _ _ (IRJUMP_LTE o1 o2 (IRLabel lbl)) = emitDirectCmpJump sm o1 o2 "jle" lbl
emitInstructionGen _ sm _ _ (IRJUMP_GT o1 o2 (IRLabel lbl)) = emitDirectCmpJump sm o1 o2 "jg" lbl
emitInstructionGen _ sm _ _ (IRJUMP_GTE o1 o2 (IRLabel lbl)) = emitDirectCmpJump sm o1 o2 "jge" lbl
emitInstructionGen _ sm _ _ (IRJUMP_EQ o1 o2 (IRLabel lbl)) = emitDirectCmpJump sm o1 o2 "je" lbl
emitInstructionGen _ sm _ _ (IRJUMP_NEQ o1 o2 (IRLabel lbl)) = emitDirectCmpJump sm o1 o2 "jne" lbl
emitInstructionGen _ sm _ _ (IRJUMP_TEST_NZ o1 o2 (IRLabel lbl)) = emitTestJump sm o1 o2 "jnz" lbl
emitInstructionGen _ sm _ _ (IRJUMP_TEST_Z o1 o2 (IRLabel lbl)) = emitTestJump sm o1 o2 "jz" lbl
emitInstructionGen mbExterns sm _ _ (IRCALL dest funcName args mbType) = emitCallGen mbExterns sm dest funcName args mbType
emitInstructionGen _ sm endLbl _ (IRRET mbOp) = emitRet sm endLbl mbOp
emitInstructionGen _ sm _ _ (IRDEREF dest ptr typ) = emitDeref sm dest ptr typ
emitInstructionGen _ sm _ _ (IRLOAD_OFFSET dest ptr offset typ) = emitLoadOffset sm dest ptr offset typ
emitInstructionGen _ sm _ fn (IRALLOC_ARRAY dest elemType values) = emitAllocArray sm fn dest elemType values
emitInstructionGen _ sm _ _ (IRGET_ELEM dest targetOp indexOp elemType) = emitGetElem sm dest targetOp indexOp elemType
emitInstructionGen _ sm _ _ (IRSET_ELEM targetOp indexOp valueOp) = emitSetElem sm targetOp indexOp valueOp
emitInstructionGen _ sm _ _ (IRINC op) = emitIncDec sm op "add"
emitInstructionGen _ sm _ _ (IRDEC op) = emitIncDec sm op "sub"
emitInstructionGen _ sm _ _ (IRADDR dest source typ) = emitAddr sm dest source typ
emitInstructionGen _ sm _ _ (IRADD_OP dest l r t) = emitBinaryOp sm dest "add" l r t
emitInstructionGen _ sm _ _ (IRSUB_OP dest l r t) = emitBinaryOp sm dest "sub" l r t
emitInstructionGen _ sm _ _ (IRMUL_OP dest l r t) = emitBinaryOp sm dest "imul" l r t
emitInstructionGen _ sm _ _ (IRDIV_OP dest l r t) = emitDivOp sm dest l r t
emitInstructionGen _ sm _ _ (IRMOD_OP dest l r t) = emitModOp sm dest l r t
emitInstructionGen _ sm _ _ (IRSHR_OP dest l r t) = emitShiftOp sm dest "sar" l r t
emitInstructionGen _ sm _ _ (IRSHL_OP dest l r t) = emitShiftOp sm dest "sal" l r t
emitInstructionGen _ sm _ _ (IRBAND_OP dest l r t) = emitBinaryOp sm dest "and" l r t
emitInstructionGen _ sm _ _ (IRBNOT_OP dest op t) = emitBitNot sm dest op t
emitInstructionGen _ sm _ _ (IRAND_OP dest l r t) = emitBinaryOp sm dest "and" l r t
emitInstructionGen _ sm _ _ (IROR_OP dest l r t) = emitBinaryOp sm dest "or" l r t
emitInstructionGen _ sm _ _ (IRCMP_EQ dest l r) = emitCompare sm dest Cmp.CmpEQ l r
emitInstructionGen _ sm _ _ (IRCMP_NEQ dest l r) = emitCompare sm dest Cmp.CmpNEQ l r
emitInstructionGen _ sm _ _ (IRCMP_LT dest l r) = emitCompare sm dest Cmp.CmpLT l r
emitInstructionGen _ sm _ _ (IRCMP_LTE dest l r) = emitCompare sm dest Cmp.CmpLTE l r
emitInstructionGen _ sm _ _ (IRCMP_GT dest l r) = emitCompare sm dest Cmp.CmpGT l r
emitInstructionGen _ sm _ _ (IRCMP_GTE dest l r) = emitCompare sm dest Cmp.CmpGTE l r
emitInstructionGen _ sm _ _ (IRCAST dest src fromT toT) = emitCast sm dest src fromT toT
emitInstructionGen _ _ _ _ instr = [emit 1 $ "; TODO: " <> show instr]

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
               , emit 1   "cvtss2sd xmm0, xmm0"
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
               , emit 1   "cvtsd2ss xmm0, xmm0"
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
emitCall = emitCallGen Nothing

emitCallGen :: Maybe [Extern] -> Map String Int -> String -> String -> [IROperand] -> Maybe IRType -> [String]
emitCallGen mbExterns sm dest funcName args mbType =
  let argSetup    = setupCallArgs sm args
      firstFloatType =
        foldr
          (\op acc -> case (acc, getOperandType op) of
                        (Nothing, Just t) | isFloatType t -> Just t
                        _                                 -> acc)
          Nothing
          args
      printfFixup = printfFixupHelp firstFloatType x86_64FloatArgsRegisters
      usePlt      = case mbExterns of
                      Just externs -> funcName `elem` externs
                      Nothing      -> False
      callTarget  = if usePlt then funcName <> " wrt ..plt" else funcName
      callInstr   = [emit 1 $ "call " <> callTarget]
      retSave     = saveCallResult sm dest mbType
   in argSetup <> printfFixup <> callInstr <> retSave
  where
    printfFixupHelp (Just IRF32) (floatReg:_)
      | funcName `elem` ["printf", "dprintf"] =
        [ emit 1 $ "cvtss2sd " <> floatReg <> ", " <> floatReg
        , emit 1   "mov eax, 1"
        ]
    printfFixupHelp (Just IRF64) (_:_) 
      | funcName `elem` ["printf", "dprintf"] =
        [ emit 1 "mov eax, 1" ]
    printfFixupHelp Nothing _
      | funcName `elem` ["printf", "dprintf"] = [emit 1 "xor eax, eax"]
    printfFixupHelp _ _
      | funcName `elem` ["printf", "dprintf"] = []
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
    getFloatReg t []         = [ emit 1   "; WARNING: no float return register available"
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

-- | Load a value of given type from pointer + byte offset
-- ptr[offset] where we load sizeof(typ) bytes
emitLoadOffset :: Map String Int -> String -> IROperand -> IROperand -> IRType -> [String]
emitLoadOffset sm dest ptr offset typ =
  let sizeSpec = getSizeSpecifier typ
      reg = getRegisterName "rax" typ
   in loadReg sm "rdi" ptr
   <> loadRegWithExt sm ("rsi", offset)
   <> [ emit 1 $ "mov " <> reg <> ", " <> sizeSpec <> " [rdi + rsi]"
      , storeReg sm dest "rax" typ
      ]

emitAllocArray :: Map String Int -> String -> String -> IRType -> [IROperand] -> [String]
emitAllocArray sm fnName dest elemType values =
  let litLabel = fnName <> "_" <> dest <> "_lit"
      withSentinelLen = length values + 1
      arrType = IRArray elemType withSentinelLen
   in emitStaticOrStack litLabel arrType
  where
    emitStaticOrStack litLabel arrType
      | all isStaticOperand values =
          [ emit 1 $ "mov rax, " <> litLabel
          , storeReg sm dest "rax" (IRPtr arrType)
          ]
      | otherwise = emitAllocArrayOnStack sm dest elemType values arrType

emitAllocArrayOnStack :: Map String Int -> String -> IRType -> [IROperand] -> IRType -> [String]
emitAllocArrayOnStack sm dest elemType values arrType =
  let dataName = dest <> "_data"
      elemSize = sizeOfIRType elemType
      baseOffset =
        case Map.lookup dataName sm of
          Just off -> off
          Nothing -> error $ "Array data not found in stack map: " <> dataName
      addr idx = "[rbp" <> show (baseOffset + idx * elemSize) <> "]"
      sizeSpec = getSizeSpecifier elemType
      reg = getRegisterName "rax" elemType
      storeValue (idx, op) =
        loadReg sm "rax" op
          <> [emit 1 $ "mov " <> sizeSpec <> " " <> addr idx <> ", " <> reg]
      sentinel = [emit 1 $ "mov " <> sizeSpec <> " " <> addr (length values) <> ", 0"]
      pointerSetup =
        [ emit 1 $ "lea rax, [rbp" <> show baseOffset <> "]"
        , storeReg sm dest "rax" (IRPtr arrType)
        ]
   in pointerSetup <> concatMap storeValue (zip [0 ..] values) <> sentinel

emitGetElem :: Map String Int -> String -> IROperand -> IROperand -> IRType -> [String]
emitGetElem sm dest targetOp indexOp elemType =
  let sizeSpec = getSizeSpecifier elemType
      reg = getRegisterName "rax" elemType
   in loadReg sm "rdi" targetOp
        <> loadRegWithExt sm ("rsi", indexOp)
        <> [ emit 1 $ "imul rsi, " <> show (sizeOfIRType elemType)
           , emit 1 $ "mov " <> reg <> ", " <> sizeSpec <> " [rdi + rsi]"
           , storeReg sm dest "rax" elemType
           ]

emitSetElem :: Map String Int -> IROperand -> IROperand -> IROperand -> [String]
emitSetElem sm targetOp indexOp valueOp =
  let elemType =
        case (getOperandType valueOp, getOperandType targetOp) of
          (Just t, _) -> t
          (_, Just (IRPtr (IRArray t _))) -> t
          _ -> IRI64
      sizeSpec = getSizeSpecifier elemType
      reg = getRegisterName "rax" elemType
   in loadReg sm "rdi" targetOp
        <> loadRegWithExt sm ("rsi", indexOp)
        <> [ emit 1 $ "imul rsi, " <> show (sizeOfIRType elemType)
           ]
        <> loadReg sm "rax" valueOp
        <> [ emit 1 $ "mov " <> sizeSpec <> " [rdi + rsi], " <> reg ]

-- | emit INC/DEC on pointer or numeric operand
emitIncDec :: Map String Int -> IROperand -> String -> [String]
emitIncDec sm (IRTemp name t) asmOp = emitIncDecHelper sm name t asmOp
emitIncDec sm (IRParam name t) asmOp = emitIncDecHelper sm name t asmOp
emitIncDec _ op _ = [emit 1 $ "; TODO: " <> show op <> " on non-temp/pointer"]

emitIncDecHelper :: Map String Int -> String -> IRType -> String -> [String]
emitIncDecHelper sm name t asmOp =
  let sizeSpec = getSizeSpecifier t
      step =
        case t of
          IRPtr (IRArray inner _) -> sizeOfIRType inner
          IRPtr inner -> sizeOfIRType inner
          _ -> 1
   in [emit 1 $ asmOp <> " " <> sizeSpec <> " " <> stackAddr sm name <> ", " <> show step]

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

emitDirectCmpJump :: Map String Int -> IROperand -> IROperand -> String -> String -> [String]
emitDirectCmpJump sm op1 op2 jumpInstr lbl =
  let typ1 = getOperandType op1
      typ2 = getOperandType op2
      isFloat = maybe False isFloatType typ1 || maybe False isFloatType typ2
  in if isFloat
     then emitFloatCmpJump sm op1 op2 jumpInstr lbl
     else emitIntCmpJump sm op1 op2 jumpInstr lbl

emitIntCmpJump :: Map String Int -> IROperand -> IROperand -> String -> String -> [String]
emitIntCmpJump sm op1 op2 jumpInstr lbl =
  let typ1 = fromMaybe IRI64 $ getOperandType op1
      typ2 = fromMaybe IRI64 $ getOperandType op2
      -- Use the larger type for comparison to avoid mixing register sizes
      cmpType = if sizeOfIRType typ1 >= sizeOfIRType typ2 then typ1 else typ2
      reg1 = getRegisterName "rax" cmpType
  in loadRegWithExt sm ("rax", op1)
     <> case op2 of
          IRConstInt 0 -> [emit 1 $ "test " <> reg1 <> ", " <> reg1]
          IRConstInt n | n >= -2147483648 && n <= 2147483647 && sizeOfIRType cmpType > 1 ->
            [emit 1 $ "cmp " <> reg1 <> ", " <> show n]
          _ -> loadRegWithExt sm ("rbx", op2)
               <> [emit 1 $ "cmp " <> reg1 <> ", " <> getRegisterName "rbx" cmpType]
     <> [emit 1 $ jumpInstr <> " " <> lbl]

emitFloatCmpJump :: Map String Int -> IROperand -> IROperand -> String -> String -> [String]
emitFloatCmpJump sm op1 op2 jumpInstr lbl =
  let typ = fromMaybe IRF64 $ getOperandType op1
  in loadFloatOperand sm "xmm0" op1 typ
     <> loadFloatOperand sm "xmm1" op2 typ
     <> (case typ of
           IRF32 -> [emit 1 "ucomiss xmm0, xmm1"]
           _     -> [emit 1 "ucomisd xmm0, xmm1"])
     <> [emit 1 $ jumpInstr <> " " <> lbl]

-- | Emit test instruction followed by conditional jump
-- test op1, op2 sets ZF based on op1 AND op2
emitTestJump :: Map String Int -> IROperand -> IROperand -> String -> String -> [String]
emitTestJump sm op1 op2 jumpInstr lbl =
  let typ1 = fromMaybe IRI64 $ getOperandType op1
      reg1 = getRegisterName "rax" typ1
  in loadRegWithExt sm ("rax", op1)
     <> case op2 of
          IRConstInt n | n >= -2147483648 && n <= 2147483647 ->
            [emit 1 $ "test " <> reg1 <> ", " <> show n]
          _ -> loadRegWithExt sm ("rbx", op2)
               <> [emit 1 $ "test " <> reg1 <> ", " <> getRegisterName "rbx" typ1]
     <> [emit 1 $ jumpInstr <> " " <> lbl]

emitRmWarning :: [String]
emitRmWarning =
  [ "; this section is to remove the gcc GNU related warning" 
  , "section .note.GNU-stack noalloc noexec nowrite"
  ]

collectStaticArrays :: [Function] -> [(String, IRType, [IROperand])]
collectStaticArrays = concatMap collectFn
  where
    collectFn (IRFunction fnName _ _ body _) = concatMap (collectInstr fnName) body

    collectInstr fnName (IRALLOC_ARRAY dest elemType values)
      | all isStaticOperand values = [(fnName <> "_" <> dest <> "_lit", elemType, values)]
    collectInstr _ _ = []

isStaticOperand :: IROperand -> Bool
isStaticOperand (IRConstInt _)  = True
isStaticOperand (IRConstChar _) = True
isStaticOperand (IRConstBool _) = True
isStaticOperand IRConstNull     = True
isStaticOperand (IRGlobal _ _)  = True
isStaticOperand _               = False

getDataDirective :: IRType -> String
getDataDirective t =
  case sizeOfIRType t of
    1 -> "db"
    2 -> "dw"
    4 -> "dd"
    8 -> "dq"
    _ -> "dq"

showStaticOperand :: IRType -> IROperand -> String
showStaticOperand _ (IRConstInt n)  = show n
showStaticOperand _ (IRConstChar c) = show (fromEnum c)
showStaticOperand _ (IRConstBool b) = if b then "1" else "0"
showStaticOperand _ IRConstNull     = "0"
showStaticOperand _ (IRGlobal n _)  = n
showStaticOperand _ _               = "0"

-- | Emit type cast instruction
-- Handles conversions between integer types, float types, and int<->float
emitCast :: Map String Int -> String -> IROperand -> IRType -> IRType -> [String]
emitCast sm dest src fromT toT
  -- Same type, just move
  | fromT == toT = loadReg sm "rax" src <> [storeReg sm dest "rax" toT]
  -- Pointer to pointer (reinterpret cast)
  | isPointerType fromT && isPointerType toT =
      loadReg sm "rax" src <> [storeReg sm dest "rax" toT]
  -- Integer to float
  | isIntegerType fromT && isFloatType toT =
      emitIntToFloat sm dest src fromT toT
  -- Float to integer
  | isFloatType fromT && isIntegerType toT =
      emitFloatToInt sm dest src fromT toT
  -- Float to float (f32 <-> f64)
  | isFloatType fromT && isFloatType toT =
      emitFloatToFloat sm dest src fromT toT
  -- Integer to integer (sign/zero extension or truncation)
  | isIntegerType fromT && isIntegerType toT =
      emitIntToInt sm dest src fromT toT
  -- Pointer to integer
  | isPointerType fromT && isIntegerType toT =
      loadReg sm "rax" src <> [storeReg sm dest "rax" toT]
  -- Integer to pointer
  | isIntegerType fromT && isPointerType toT =
      loadReg sm "rax" src <> [storeReg sm dest "rax" toT]
  -- Fallback
  | otherwise =
      [ emit 1 $ "; WARNING: unsupported cast from " <> show fromT <> " to " <> show toT ]
      <> loadReg sm "rax" src <> [storeReg sm dest "rax" toT]

-- | Check if type is an integer type
isIntegerType :: IRType -> Bool
isIntegerType IRI8  = True
isIntegerType IRI16 = True
isIntegerType IRI32 = True
isIntegerType IRI64 = True
isIntegerType IRU8  = True
isIntegerType IRU16 = True
isIntegerType IRU32 = True
isIntegerType IRU64 = True
isIntegerType IRChar = True
isIntegerType IRBool = True
isIntegerType _     = False

-- | Check if type is a pointer type
isPointerType :: IRType -> Bool
isPointerType (IRPtr _) = True
isPointerType _         = False

-- | Check if type is signed integer
isSignedInt :: IRType -> Bool
isSignedInt IRI8  = True
isSignedInt IRI16 = True
isSignedInt IRI32 = True
isSignedInt IRI64 = True
isSignedInt _     = False

-- | Integer to float conversion
emitIntToFloat :: Map String Int -> String -> IROperand -> IRType -> IRType -> [String]
emitIntToFloat sm dest src fromT toT =
  let loadSrc = case fromT of
        IRI8  -> loadReg sm "rax" src <> [emit 1 "movsx eax, al"]
        IRI16 -> loadReg sm "rax" src <> [emit 1 "movsx eax, ax"]
        IRI32 -> loadReg sm "rax" src
        IRI64 -> loadReg sm "rax" src
        IRU8  -> loadReg sm "rax" src <> [emit 1 "movzx eax, al"]
        IRU16 -> loadReg sm "rax" src <> [emit 1 "movzx eax, ax"]
        IRU32 -> loadReg sm "rax" src <> [emit 1 "mov eax, eax"]  -- zero-extend to 64-bit
        IRU64 -> loadReg sm "rax" src
        _     -> loadReg sm "rax" src
      convert = case toT of
        IRF32 -> [emit 1 "cvtsi2ss xmm0, rax"]
        IRF64 -> [emit 1 "cvtsi2sd xmm0, rax"]
        _     -> []
      storeDst = case toT of
        IRF32 -> [emit 1 $ "movss dword " <> stackAddr sm dest <> ", xmm0"]
        IRF64 -> [emit 1 $ "movsd qword " <> stackAddr sm dest <> ", xmm0"]
        _     -> []
   in loadSrc <> convert <> storeDst

-- | Float to integer conversion (truncation)
emitFloatToInt :: Map String Int -> String -> IROperand -> IRType -> IRType -> [String]
emitFloatToInt sm dest src fromT toT =
  let loadSrc = loadFloatOperand sm "xmm0" src fromT
      convert = case fromT of
        IRF32 -> [emit 1 "cvttss2si rax, xmm0"]
        IRF64 -> [emit 1 "cvttsd2si rax, xmm0"]
        _     -> []
   in loadSrc <> convert <> [storeReg sm dest "rax" toT]

-- | Float to float conversion (f32 <-> f64)
emitFloatToFloat :: Map String Int -> String -> IROperand -> IRType -> IRType -> [String]
emitFloatToFloat sm dest src fromT toT =
  let loadSrc = loadFloatOperand sm "xmm0" src fromT
      convert = case (fromT, toT) of
        (IRF32, IRF64) -> [emit 1 "cvtss2sd xmm0, xmm0"]
        (IRF64, IRF32) -> [emit 1 "cvtsd2ss xmm0, xmm0"]
        _              -> []
      storeDst = case toT of
        IRF32 -> [emit 1 $ "movss dword " <> stackAddr sm dest <> ", xmm0"]
        IRF64 -> [emit 1 $ "movsd qword " <> stackAddr sm dest <> ", xmm0"]
        _     -> []
   in loadSrc <> convert <> storeDst

-- | Integer to integer conversion (extension or truncation)
emitIntToInt :: Map String Int -> String -> IROperand -> IRType -> IRType -> [String]
emitIntToInt sm dest src fromT toT =
  let srcSize = sizeOfIRType fromT
      dstSize = sizeOfIRType toT
      loadSrc = loadReg sm "rax" src
      convert
        -- Truncation: just store smaller portion
        | dstSize < srcSize = []
        -- Sign extension
        | isSignedInt fromT = case fromT of
            IRI8  -> [emit 1 "movsx rax, al"]
            IRI16 -> [emit 1 "movsx rax, ax"]
            IRI32 -> [emit 1 "movsxd rax, eax"]
            _     -> []
        -- Zero extension
        | otherwise = case fromT of
            IRU8  -> [emit 1 "movzx rax, al"]
            IRU16 -> [emit 1 "movzx rax, ax"]
            IRU32 -> [emit 1 "mov eax, eax"]  -- clears upper 32 bits
            IRChar -> [emit 1 "movzx rax, al"]
            IRBool -> [emit 1 "movzx rax, al"]
            _     -> []
   in loadSrc <> convert <> [storeReg sm dest "rax" toT]

commaSep :: [String] -> String
commaSep [] = ""
commaSep (x:xs) = foldl (\acc s -> acc <> ", " <> s) x xs
