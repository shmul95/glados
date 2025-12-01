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

-- | emit function parameters (for now, only handles max 6 parameters)
-- mov qword [rbp <+-> offset], <x86_64ArgsRegisters>
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

-- | get the nasm representation of an IR variable on the stack
-- [rbp-offset]
stackAddr :: Map String Int -> String -> String
stackAddr sm name = case Map.lookup name sm of
  Just offset -> "[rbp" ++ show offset ++ "]"
  Nothing -> error $ "Variable not found in stack map: " ++ name

-- | get the nasm representation of an operand (const | stack address)
emitOperand :: Map String Int -> IROperand -> String
emitOperand sm op = case op of
  IRConstInt n -> show n
  IRConstChar c -> show (fromEnum c)
  IRTemp name _ -> stackAddr sm name
  IRParam name _ -> stackAddr sm name
  IRGlobal name _ -> error "Global operand should be loaded via ADDR/LOAD: " ++ name
  _ -> error $ "Unsupported IROperand for direct emission: " ++ show op

-- | emit a single IR instruction to nasm
emitInstruction :: Map String Int -> String -> IRInstruction -> [String]
emitInstruction sm _ (IRASSIGN dest op _) = emitAssign sm dest op
emitInstruction _ _ (IRLABEL (IRLabel lbl)) = [lbl ++ ":"]
emitInstruction _ _ (IRJUMP (IRLabel lbl)) = [emit 1 $ "jmp " ++ lbl]
emitInstruction sm _ (IRJUMP_EQ0 op (IRLabel lbl)) = emitJumpEQ0 sm op lbl
emitInstruction sm _ (IRCALL dest funcName args mbType) = emitCall sm dest funcName args mbType
emitInstruction _ endLbl (IRRET Nothing) = [emit 1 $ "jmp " ++ endLbl]
emitInstruction sm endLbl (IRRET (Just op)) = emitRet sm endLbl op
emitInstruction sm _ (IRDEREF dest ptr typ) = emitDeref sm dest ptr typ
emitInstruction sm _ (IRINC op) = emitIncDec sm op "add"
emitInstruction sm _ (IRDEC op) = emitIncDec sm op "sub"
emitInstruction sm _ (IRADDR dest source typ) = emitAddr sm dest source typ
emitInstruction _ _ instr = [emit 1 $ "; TODO: " ++ show instr]

-- | emit dest = op
emitAssign :: Map String Int -> String -> IROperand -> [String]
emitAssign sm dest (IRConstInt n) = [emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", " ++ show n]
emitAssign sm dest (IRConstChar c) = [emit 1 $ "mov byte " ++ stackAddr sm dest ++ ", " ++ show (fromEnum c)]
emitAssign sm dest (IRTemp src _) =
  [ emit 1 $ "mov rax, qword " ++ stackAddr sm src,
    emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"
  ]
emitAssign sm dest (IRParam src _) =
  [ emit 1 $ "mov rax, qword " ++ stackAddr sm src,
    emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", rax"
  ]
emitAssign sm dest op =
  [ emit 1 $ "; WARNING: Unsupported IRASSIGN operand: " ++ show op,
    emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", 0"
  ]

-- | emit call dest
-- CALL <name>(args...) -> call <name>
emitCall :: Map String Int -> String -> String -> [IROperand] -> Maybe IRType -> [String]
emitCall sm dest funcName args _ =
  let argSetup = emitCallArgs sm args
      callInstr = emitCallInstr funcName
      retSave = emitCallRet sm dest
   in argSetup ++ callInstr ++ retSave

-- | emit the first 6 arguments in registers
-- mov rdi, qword [rbp-offset]
emitArg :: Map String Int -> String -> IROperand -> String
emitArg sm reg op = emit 1 $ "mov " ++ reg ++ ", " ++ operandValue sm op

operandValue :: Map String Int -> IROperand -> String
operandValue _ (IRConstInt n) = show n
operandValue _ (IRConstChar c) = show (fromEnum c)
operandValue sm other = "qword " ++ emitOperand sm other

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

emitLoadRax :: Map String Int -> IROperand -> [String]
emitLoadRax sm op =
  let val = case op of
        IRConstInt n -> show n
        IRConstChar c -> show (fromEnum c)
        _ -> "qword " ++ emitOperand sm op
   in [emit 1 $ "mov rax, " ++ val]

emitRet :: Map String Int -> String -> IROperand -> [String]
emitRet sm endLbl op = emitLoadRax sm op ++ [emit 1 $ "jmp " ++ endLbl]

-- | emit deref ptr
-- cases:
--  1- RAX = &value
--  2- RAX = *RAX
--  3- dest = RAX
emitDeref :: Map String Int -> String -> IROperand -> IRType -> [String]
emitDeref sm dest ptr typ =
  let ptrAddr = emitOperand sm ptr
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
emitIncDec sm op asmOp = case op of
  IRTemp name (IRPtr _) ->
    [ emit 1 $ asmOp ++ " qword " ++ stackAddr sm name ++ ", 1"
    ]
  _ -> [emit 1 $ "; TODO: " ++ show op ++ " on non-pointer"]

-- | emit ADDR dest, source
--  cases:
--  1- source is a global string: mov rax, str_x; mov [dest], rax
--  2- source is a local variable: lea rax, [rbp-offset]; mov [dest], rax
emitAddr :: Map String Int -> String -> String -> IRType -> [String]
emitAddr sm dest source _ =
  case source of
    _
      | take 4 source == "str_" ->
          [ emit 1 $ "mov qword " ++ "rax" ++ ", " ++ source,
            emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", " ++ "rax"
          ]
    _ ->
      [ emit 1 $ "lea " ++ "rax" ++ ", qword " ++ stackAddr sm source,
        emit 1 $ "mov qword " ++ stackAddr sm dest ++ ", " ++ "rax"
      ]

-- | emit jump if equal to zero
emitJumpEQ0 :: Map String Int -> IROperand -> String -> [String]
emitJumpEQ0 sm op lbl =
  let loadOp = case op of
        IRConstInt n -> [emit 1 $ "mov " ++ "rax" ++ ", " ++ show n]
        IRConstChar c -> [emit 1 $ "mov " ++ "rax" ++ ", " ++ show (fromEnum c)]
        _ -> [emit 1 $ "mov " ++ "rax" ++ ", qword " ++ emitOperand sm op]
      testInstr = [emit 1 $ "test " ++ "rax" ++ ", " ++ "rax"]
      jumpInstr = [emit 1 $ "je " ++ lbl]
   in loadOp ++ testInstr ++ jumpInstr
