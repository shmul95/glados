{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Printer
  ( prettyPrintIR
  , printProgram
  , printTopLevel
  , printFunction
  , printInstruction
  , printOperand
  , printType
  ) where
#else
module Rune.IR.Printer
  ( prettyPrintIR
  ) where
#endif

import Data.List (intercalate)
import Lib (escapeString)
import Rune.IR.Nodes

--
-- public
--

prettyPrintIR :: IRProgram -> String
prettyPrintIR (IRProgram name defs) = printProgram (IRProgram name defs)

--
-- private
--

printProgram :: IRProgram -> String
printProgram (IRProgram name defs) =
  let header = "PROGRAM " ++ name ++ ":"
      defStrs = map printTopLevel defs
   in unlines (header : defStrs)

printTopLevel :: IRTopLevel -> String
printTopLevel (IRExtern name) =
  "EXTERN " ++ name
printTopLevel (IRGlobalDef name (IRGlobalStringVal value)) =
  "GLOBAL " ++ name ++ ": string = \"" ++ escapeString value ++ "\\0\""
printTopLevel (IRGlobalDef name (IRGlobalFloatVal value typ)) =
  "GLOBAL " ++ name ++ ": " ++ printType typ ++ " = " ++ show value
printTopLevel (IRFunctionDef func) =
  printFunction func
printTopLevel (IRStructDef name fields) =
  "STRUCT " ++ name ++ " { " ++ intercalate ", " (map (\(n, t) -> n ++ ": " ++ printType t) fields) ++ " }"

printFunction :: IRFunction -> String
printFunction (IRFunction name params _ body _) =
  let paramStrs = map (\(n, t) -> n ++ ": " ++ printType t) params
      header = "DEF " ++ name ++ "(" ++ intercalate ", " paramStrs ++ "):"
      bodyStrs = map printInstructionWithIndent body
   in intercalate "\n" (header : bodyStrs)
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
printInstruction (IRAND_OP dest left right typ) =
  dest ++ ": " ++ printType typ ++ " = AND " ++ printOperand left ++ ", " ++ printOperand right
printInstruction (IROR_OP dest left right typ) =
  dest ++ ": " ++ printType typ ++ " = OR " ++ printOperand left ++ ", " ++ printOperand right
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
printInstruction (IRCALL dest funcName args mbType) =
  let argsStr = intercalate ", " (map printOperand args)
      callStr = "CALL " ++ funcName ++ "(" ++ argsStr ++ ")"
   in if null dest
        then callStr
        else
          let typeStr = case mbType of
                Just t -> ": " ++ printType t
                Nothing -> ": i32"
           in dest ++ typeStr ++ " = " ++ callStr
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
printInstruction (IRGET_FIELD dest base structName field typ) =
  dest ++ ": " ++ printType typ ++ " = GET_FIELD " ++ printOperand base ++ ", \"" ++ structName ++ "\", \"" ++ field ++ "\""
printInstruction (IRSET_FIELD base structName field val) =
  "SET_FIELD " ++ printOperand base ++ ", \"" ++ structName ++ "\", \"" ++ field ++ "\", " ++ printOperand val
printInstruction (IRALLOC_ARRAY dest typ elems) =
  dest ++ ": " ++ printType (IRPtr (IRArray typ (length elems))) ++ " = ALLOC_ARRAY " ++ printType typ ++ " [" ++ intercalate ", " (map printOperand elems) ++ "]"
printInstruction (IRGET_ELEM dest arr idx typ) =
  dest ++ ": " ++ printType typ ++ " = GET_ELEM " ++ printOperand arr ++ "[" ++ printOperand idx ++ "]"
printInstruction (IRSET_ELEM arr idx val) =
  "SET_ELEM " ++ printOperand arr ++ "[" ++ printOperand idx ++ "] = " ++ printOperand val

printOperand :: IROperand -> String
printOperand (IRConstInt n) = show n
printOperand (IRConstFloat f) = show f
printOperand (IRConstChar c) = show c
printOperand (IRConstBool True) = "(true)"
printOperand (IRConstBool False) = "(false)"
printOperand IRConstNull = "(null)"
printOperand (IRTemp name _) = name
printOperand (IRParam name _) = name
printOperand (IRGlobal name _) = name

printType :: IRType -> String
printType IRI8 = "i8"
printType IRI16 = "i16"
printType IRI32 = "i32"
printType IRI64 = "i64"
printType IRF32 = "f32"
printType IRF64 = "f64"
printType IRU8 = "u8"
printType IRChar = "char"
printType IRU16 = "u16"
printType IRU32 = "u32"
printType IRU64 = "u64"
printType (IRPtr t) = "*" ++ printType t
printType (IRStruct s) = s
printType IRNull = "null"
printType IRBool = "bool"
printType (IRArray t len) = "[" ++ printType t ++ " x " ++ show len ++ "]"
