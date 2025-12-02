module Rune.Backend.X86_64.Codegen
  ( emitAssembly,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Rune.Backend.Helpers (calculateStackMap, collectTopLevels, emit, escapeString)
import Rune.Backend.Types (Extern, Function, GlobalString)
import Rune.Backend.X86_64.Registers (x86_64ArgsRegisters)
import Rune.IR.IRHelpers (sizeOfIRType)
import Rune.IR.Nodes

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
emitExterns xs = map (\n -> "extern " ++ n) xs

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
-- function stack management
--

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
emitParam sm regs (idx, (irName, _))
  | idx < length regs = do
      offset <- Map.lookup irName sm
      Just $ emit 1 $ "mov qword [rbp" ++ show offset ++ "], " ++ regs !! idx
  | otherwise = Nothing

--
-- instruction emission
--

-- | emit a single IR instruction to nasm
emitInstruction :: Map String Int -> String -> IRInstruction -> [String]
emitInstruction sm _ (IRASSIGN dest op _) = emitAssign sm dest op
emitInstruction _ _ (IRLABEL (IRLabel lbl)) = [lbl ++ ":"]
emitInstruction _ _ (IRJUMP (IRLabel lbl)) = [emit 1 $ "jmp " ++ lbl]
emitInstruction sm _ (IRJUMP_EQ0 op (IRLabel lbl)) = emitJumpEQ0 sm op lbl
emitInstruction sm _ (IRJUMP_FALSE op (IRLabel lbl)) = emitJumpFalse sm op lbl
emitInstruction sm _ (IRJUMP_TRUE op (IRLabel lbl)) = emitJumpTrue sm op lbl
emitInstruction sm _ (IRCALL dest funcName args mbType) = emitCall sm dest funcName args mbType
emitInstruction sm endLbl (IRRET mbOp) = emitRet sm endLbl mbOp
emitInstruction sm _ (IRDEREF dest ptr typ) = emitDeref sm dest ptr typ
emitInstruction sm _ (IRINC op) = emitIncDec sm op "add"
emitInstruction sm _ (IRDEC op) = emitIncDec sm op "sub"
emitInstruction sm _ (IRADDR dest source typ) = emitAddr sm dest source typ
emitInstruction sm _ (IRADD_OP dest l r _) = emitBinaryOp sm dest "add" l r
emitInstruction sm _ (IRSUB_OP dest l r _) = emitBinaryOp sm dest "sub" l r
emitInstruction sm _ (IRMUL_OP dest l r _) = emitBinaryOp sm dest "imul" l r
emitInstruction sm _ (IRAND_OP dest l r _) = emitBinaryOp sm dest "and" l r
emitInstruction sm _ (IROR_OP dest l r _) = emitBinaryOp sm dest "or" l r
emitInstruction sm _ (IRCMP_EQ dest l r) = emitCompareOp sm dest "sete" l r
emitInstruction sm _ (IRCMP_NEQ dest l r) = emitCompareOp sm dest "setne" l r
emitInstruction sm _ (IRCMP_LT dest l r) = emitCompareOp sm dest "setl" l r
emitInstruction sm _ (IRCMP_LTE dest l r) = emitCompareOp sm dest "setle" l r
emitInstruction sm _ (IRCMP_GT dest l r) = emitCompareOp sm dest "setg" l r
emitInstruction sm _ (IRCMP_GTE dest l r) = emitCompareOp sm dest "setge" l r
emitInstruction _ _ instr = [emit 1 $ "; TODO: " ++ show instr]

-- | emit dest = op
emitAssign :: Map String Int -> String -> IROperand -> [String]
emitAssign sm dest (IRConstInt n) = [emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", " ++ show n]
emitAssign sm dest (IRConstChar c) = [emit 1 $ "mov byte " ++ stackAddr sm dest ++ ", " ++ show (fromEnum c)]
emitAssign sm dest (IRTemp srcName _) = emitMoveStackToStack sm dest srcName
emitAssign sm dest (IRParam srcName _) = emitMoveStackToStack sm dest srcName
emitAssign _ dest op =
  [ emit 1 $ "; WARNING: Unsupported IRASSIGN operand: " ++ show op,
    emit 1 $ "mov qword " ++ dest ++ ", 0"
  ]

-- | emit call dest
emitCall :: Map String Int -> String -> String -> [IROperand] -> Maybe IRType -> [String]
emitCall sm dest funcName args _ =
  let argSetup = emitCallArgs sm args
      callInstr = emitCallInstr funcName
      retSave = emitCallRet sm dest
   in argSetup ++ callInstr ++ retSave

-- | emit the first 6 arguments in registers
emitArg :: Map String Int -> String -> IROperand -> String
emitArg sm reg op = emit 1 $ "mov " ++ reg ++ ", " ++ getOperandValueString sm op

emitCallArgs :: Map String Int -> [IROperand] -> [String]
emitCallArgs sm args =
  let regs = x86_64ArgsRegisters
   in zipWith (emitArg sm) regs args

emitCallInstr :: String -> [String]
emitCallInstr name = [emit 1 $ "call " ++ name]

--
-- emit return
--

emitCallRet :: Map String Int -> String -> [String]
emitCallRet _ "" = []
emitCallRet sm dest = [emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"]

-- | emit a return instruction
--  cases:
--  1- no return value: xor rax, rax to avoid returning garbage value
--  2- return value: load into rax
emitRet :: Map String Int -> String -> Maybe IROperand -> [String]
emitRet _ endLbl Nothing =
  [ emit 1 "xor rax, rax",
    emit 1 $ "jmp " ++ endLbl
  ]
emitRet sm endLbl (Just op) =
  emitLoadRax sm op ++ [emit 1 $ "jmp " ++ endLbl]

-- | emit deref ptr
--  cases:
--  1- load pointer address into rax
--  2- deref based on type size into rax
--  3- store rax into dest stack location
emitDeref :: Map String Int -> String -> IROperand -> IRType -> [String]
emitDeref sm dest ptr typ =
  let ptrAddr = getOperandConstOrStackAddr sm ptr
      size = sizeOfIRType typ
      movType = case size of
        1 -> "movzx " ++ "rax" ++ ", byte"
        4 -> "mov " ++ "rax" ++ ", dword"
        8 -> "mov " ++ "rax" ++ ", qword"
        _ -> error $ "Unsupported size for DEREF: " ++ show size
   in [ emit 1 $ "mov " ++ "rax" ++ ", qword " ++ ptrAddr,
        emit 1 $ movType ++ " [" ++ "rax" ++ "]",
        emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", " ++ "rax"
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
emitAddr sm dest source _ =
  case source of
    _
      | take 4 source == "str_" ->
          [ emit 1 $ "mov " ++ "rax" ++ ", " ++ source,
            emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", " ++ "rax"
          ]
    _ ->
      [ emit 1 $ "lea " ++ "rax" ++ ", " ++ stackAddr sm source,
        emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", " ++ "rax"
      ]

-- | emit jump if equal to zero
emitJumpEQ0 :: Map String Int -> IROperand -> String -> [String]
emitJumpEQ0 sm op lbl =
  let loadOp = [emit 1 $ "mov " ++ "rax" ++ ", " ++ getOperandValueString sm op]
      testInstr = [emit 1 $ "test " ++ "rax" ++ ", " ++ "rax"]
      jumpInstr = [emit 1 $ "je " ++ lbl]
   in loadOp ++ testInstr ++ jumpInstr

-- | emit jump if false
emitJumpFalse :: Map String Int -> IROperand -> String -> [String]
emitJumpFalse sm op lbl =
  let loadOp = emitLoadRax sm op
      testInstr = [emit 1 $ "test " ++ "rax" ++ ", " ++ "rax"]
      jumpInstr = [emit 1 $ "je " ++ lbl]
   in loadOp ++ testInstr ++ jumpInstr

-- | emit jump if true
emitJumpTrue :: Map String Int -> IROperand -> String -> [String]
emitJumpTrue sm op lbl =
  let loadOp = emitLoadRax sm op
      testInstr = [emit 1 $ "test " ++ "rax" ++ ", " ++ "rax"]
      jumpInstr = [emit 1 $ "jne " ++ lbl]
   in loadOp ++ testInstr ++ jumpInstr

-- | emit dest = left <asmOp> right
emitBinaryOp :: Map String Int -> String -> String -> IROperand -> IROperand -> [String]
emitBinaryOp sm dest asmOp leftOp rightOp =
  let loadLeft = emitLoadRax sm leftOp
      loadRight = [emit 1 $ "mov rbx, " ++ getOperandValueString sm rightOp]
      opInstr = [emit 1 $ asmOp ++ " rax, rbx"]
      saveResult = [emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"]
   in loadLeft ++ loadRight ++ opInstr ++ saveResult

-- | emit dest = left <asmOp> right (comparison)
emitCompareOp :: Map String Int -> String -> String -> IROperand -> IROperand -> [String]
emitCompareOp sm dest setOp leftOp rightOp =
  let loadLeft = emitLoadRax sm leftOp
      loadRight = [emit 1 $ "mov rbx, " ++ getOperandValueString sm rightOp]
      cmpInstr = [emit 1 $ "cmp rax, rbx"]
      setInstr = [emit 1 $ setOp ++ " al"]
      movzxInstr = [emit 1 $ "movzx rax, al"]
      saveResult = [emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"]
   in loadLeft ++ loadRight ++ cmpInstr ++ setInstr ++ movzxInstr ++ saveResult

--
-- private helpers
--

-- | get the nasm representation of an IR variable on the stack: [rbp<+->offset]
stackAddr :: Map String Int -> String -> String
stackAddr sm name = case Map.lookup name sm of
  Just offset -> "[rbp" ++ show offset ++ "]"
  Nothing -> error $ "Variable not found in stack map: " ++ name

-- | get the nasm representation of an operand that lives on the stack: [rbp<+->offset]
getVarStackAddr :: Map String Int -> IROperand -> String
getVarStackAddr sm (IRTemp name _) = stackAddr sm name
getVarStackAddr sm (IRParam name _) = stackAddr sm name
getVarStackAddr _ op = error $ "Unsupported IROperand for stack address: " ++ show op

-- | get the nasm representation of an operand: const_value | [rbp-offset]
getOperandConstOrStackAddr :: Map String Int -> IROperand -> String
getOperandConstOrStackAddr _ (IRConstInt n) = show n
getOperandConstOrStackAddr _ (IRConstChar c) = show $ fromEnum c
getOperandConstOrStackAddr sm (IRTemp name _) = stackAddr sm name
getOperandConstOrStackAddr sm (IRParam name _) = stackAddr sm name
getOperandConstOrStackAddr _ (IRGlobal name _) =
  error "Global operand should be loaded via ADDR/LOAD: " ++ name
getOperandConstOrStackAddr _ op = error $ "Unsupported IROperand for direct emission: " ++ show op

-- | get the nasm representation of an operand's *value*: const_value | qword [rbp-offset]
getOperandValueString :: Map String Int -> IROperand -> String
getOperandValueString _ (IRConstInt n) = show n
getOperandValueString _ (IRConstChar c) = show $ fromEnum c
getOperandValueString sm op = "qword " ++ getVarStackAddr sm op

-- | emit: mov rax, qword [src]; mov qword [dest], rax
emitMoveStackToStack :: Map String Int -> String -> String -> [String]
emitMoveStackToStack sm dest src =
  [ emit 1 $ "mov rax, qword " ++ stackAddr sm src,
    emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"
  ]

-- | emit: mov rax, <operand_value_string>
emitLoadRax :: Map String Int -> IROperand -> [String]
emitLoadRax sm op =
  [emit 1 $ "mov rax, " ++ getOperandValueString sm op]
