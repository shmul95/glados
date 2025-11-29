module Rune.IR.Printer (prettyPrintIR) where

import Data.List (intercalate)
import Rune.IR.Nodes

--
-- public
--

prettyPrintIR :: IRProgram -> String
prettyPrintIR (IRProgram _ defs) = unlines $ map printTopLevel defs

--
-- private
--

printTopLevel :: IRTopLevel -> String
printTopLevel (IRGlobalString name value) =
  "GLOBAL " ++ name ++ ": string = \"" ++ escapeString value ++ "\\0\""
printTopLevel (IRFunctionDef func) =
  printFunction func

printFunction :: IRFunction -> String
printFunction (IRFunction name params _ body) =
  let paramStrs = map (\(n, t) -> n ++ ": " ++ printType t) params
      header = "DEF " ++ name ++ "(" ++ intercalate ", " paramStrs ++ "):"
      bodyStrs = map printInstructionWithIndent body
   in unlines (header : bodyStrs)
  where
    printInstructionWithIndent instr =
      case instr of
        IRLABEL _ -> printInstruction instr
        _ -> "    " ++ printInstruction instr

printInstruction :: IRInstruction -> String
printInstruction (IRASSIGN name op typ) =
  name ++ ": " ++ printType typ ++ " = " ++ printOperand op
printInstruction (IRADD_OP dest left right typ) =
  dest ++ ": " ++ printType typ ++ " = ADD " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRSUB_OP dest left right typ) =
  dest ++ ": " ++ printType typ ++ " = SUB " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRMUL_OP dest left right typ) =
  dest ++ ": " ++ printType typ ++ " = MUL " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRDIV_OP dest left right typ) =
  dest ++ ": " ++ printType typ ++ " = DIV " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRMOD_OP dest left right typ) =
  dest ++ ": " ++ printType typ ++ " = MOD " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRCMP_EQ dest left right) =
  dest ++ " = CMP_EQ " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRCMP_NEQ dest left right) =
  dest ++ " = CMP_NEQ " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRCMP_LT dest left right) =
  dest ++ " = CMP_LT " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRCMP_LTE dest left right) =
  dest ++ " = CMP_LTE " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRCMP_GT dest left right) =
  dest ++ " = CMP_GT " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRCMP_GTE dest left right) =
  dest ++ " = CMP_GTE " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IRLABEL (IRLabel name)) =
  name ++ ":"
printInstruction (IRJUMP (IRLabel target)) =
  "JUMP " ++ target
printInstruction (IRJUMP_TRUE op (IRLabel target)) =
  "JUMP_TRUE " ++ printOperand op ++ ", " ++ target
printInstruction (IRJUMP_FALSE op (IRLabel target)) =
  "JUMP_FALSE " ++ printOperand op ++ ", " ++ target
printInstruction (IRJUMP_EQ0 op (IRLabel target)) =
  "JUMP_EQ0 " ++ printOperand op ++ ", " ++ target
printInstruction (IRCALL dest funcName args _) =
  let argsStr = intercalate ", " (map printOperand args)
   in if null dest
        then "CALL " ++ funcName ++ "(" ++ argsStr ++ ")"
        else dest ++ " = CALL " ++ funcName ++ "(" ++ argsStr ++ ")"
printInstruction (IRRET Nothing) =
  "RET"
printInstruction (IRRET (Just op)) =
  "RET " ++ printOperand op
printInstruction (IRADDR dest name typ) =
  dest ++ ": " ++ printType typ ++ " = ADDR " ++ name
printInstruction (IRDEREF dest ptr typ) =
  dest ++ ": " ++ printType typ ++ " = DEREF " ++ printOperand ptr
printInstruction (IRINC op) =
  "INC " ++ printOperand op
printInstruction (IRDEC op) =
  "DEC " ++ printOperand op
printInstruction (IRSTORE dest src) =
  "STORE " ++ printOperand src ++ " -> " ++ printOperand dest
printInstruction (IRLOAD dest src typ) =
  dest ++ ": " ++ printType typ ++ " = LOAD " ++ printOperand src
printInstruction (IRALLOC name typ) =
  "ALLOC " ++ name ++ ": " ++ printType typ
printInstruction IREMPTYLINE =
  ""

printOperand :: IROperand -> String
printOperand (IRConstInt n) = show n
printOperand (IRConstFloat f) = show f
printOperand (IRConstChar c) = show c
printOperand (IRTemp name _) = name
printOperand (IRParam name _) = name
printOperand (IRGlobal name _) = name

printType :: IRType -> String
printType IRI32 = "i32"
printType IRI64 = "i64"
printType IRF32 = "f32"
printType IRF64 = "f64"
printType IRU8 = "u8"
printType (IRPtr t) = "*" ++ printType t
printType IRVoid = "void"

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar c = [c]
