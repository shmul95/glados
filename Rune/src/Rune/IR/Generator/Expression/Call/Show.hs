module Rune.IR.Generator.Expression.Call.Show (genShowCall) where

import Control.Monad (unless)
import Control.Monad.State (gets, modify)
import Rune.AST.Nodes (Expression)
import Rune.IR.IRHelpers (genFormatString, registerCall, newStringGlobal, newTemp, nextLabelIndex, makeLabel)
import Rune.IR.Nodes (IRGen, GenState(..), IRInstruction (..), IROperand (..), IRType (..), IRTopLevel(..), IRFunction(..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

--
-- show calls
-- show is a built-in function that prints the value of its argument to stdout
-- def show(value: any) -> null
--

--
-- public
--

genShowCall :: GenExprCallback -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genShowCall genExpr arg = do
  (instrs, op, typ) <- genExpr arg
  case typ of
    IRBool -> genShowBoolCall instrs op
    IRChar -> genShowCharCall instrs op
    _ -> genShowPrintfCall instrs op typ

--
-- private
--

--
-- helpers
--

mkShowOverride :: String -> IRType -> IRFunction
mkShowOverride name paramType = IRFunction
  { irFuncName = name
  , irFuncParams = [("value", paramType)]
  , irFuncRetType = Just IRNull
  , irFuncBody = []
  }

overrideExists :: IRTopLevel -> String -> Bool
overrideExists (IRFunctionDef f) name = irFuncName f == name
overrideExists _ _ = False

--
-- def show(value: any) -> null
--

-- | as show is a built-in "printf"-like function
-- def show(value: any) -> null
-- we need to format the arguments accordingly to the input
genShowFmtCall :: IROperand -> IRType -> IROperand -> IRGen ([IRInstruction], [IROperand])
genShowFmtCall originalOp typ finalOp = case getFormatSpecifier originalOp typ of
  Just fmt -> do
    (i, f) <- genFormatString fmt
    return (i, [f, finalOp])
  Nothing -> return ([], [finalOp])

getShowFunc :: IROperand -> IRType -> String
getShowFunc _ (IRStruct s) = "show_" ++ s
getShowFunc _ (IRPtr (IRStruct s)) = "show_" ++ s
getShowFunc _ _ = "printf"

getFormatSpecifier :: IROperand -> IRType -> Maybe String
getFormatSpecifier _ IRI8 = Just "%hhd"
getFormatSpecifier _ IRI16 = Just "%hd"
getFormatSpecifier _ IRI32 = Just "%d"
getFormatSpecifier _ IRI64 = Just "%ld"
getFormatSpecifier _ IRU8 = Just "%hhu"
getFormatSpecifier _ IRU16 = Just "%hu"
getFormatSpecifier _ IRU32 = Just "%u"
getFormatSpecifier _ IRU64 = Just "%lu"
getFormatSpecifier _ IRChar = Just "%c"
getFormatSpecifier _ IRF32 = Just "%f"
getFormatSpecifier _ IRF64 = Just "%lf"
getFormatSpecifier _ IRNull = Just "(null)"
getFormatSpecifier _ (IRPtr IRChar) = Just "%s"
getFormatSpecifier _ _ = Nothing

prepareAddr :: IROperand -> IRType -> ([IRInstruction], IROperand)
prepareAddr (IRTemp n _) (IRStruct t) =
  ( [IRADDR ("addr_" ++ n) n (IRPtr (IRStruct t))]
  , IRTemp ("addr_" ++ n) (IRPtr (IRStruct t))
  )
prepareAddr (IRTemp n _) (IRPtr (IRStruct t)) =
  ( [IRADDR ("addr_" ++ n) n (IRPtr (IRPtr (IRStruct t)))]
  , IRTemp ("addr_" ++ n) (IRPtr (IRPtr (IRStruct t)))
  )
prepareAddr op _ = ([], op)

--
-- def show(value: bool) -> null
--

-- | show(<bool>) -> show_bool(<bool>)
-- instead of printing 1 | 0 we print "(true)" | "(false)"
genShowBoolCall :: [IRInstruction] -> IROperand -> IRGen ([IRInstruction], IROperand, IRType)
genShowBoolCall instrs op = do
  ensureShowBoolFunc
  registerCall "show_bool"
  let callInstr = IRCALL "" "show_bool" [op] Nothing
  return (instrs ++ [callInstr], IRTemp "t_null" IRNull, IRNull)

-- | show(<char>) -> putchar(<char>)
-- optimized: use putchar instead of printf("%c", char)
genShowCharCall :: [IRInstruction] -> IROperand -> IRGen ([IRInstruction], IROperand, IRType)
genShowCharCall instrs op = do
  registerCall "putchar"
  let callInstr = IRCALL "" "putchar" [op] Nothing
  return (instrs ++ [callInstr], IRTemp "t_null" IRNull, IRNull)

-- | generic printf fallback for other types
genShowPrintfCall :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genShowPrintfCall instrs op typ = do
  let funcName = getShowFunc op typ
      (prep, finalOp) = prepareAddr op typ
  
  registerCall funcName
  (fmtInstrs, callArgs) <- genShowFmtCall op typ finalOp
  
  let callInstr = IRCALL "" funcName callArgs Nothing
  return (instrs ++ prep ++ fmtInstrs ++ [callInstr], IRTemp "t_null" IRNull, IRNull)


ensureShowBoolFunc :: IRGen ()
ensureShowBoolFunc = do
  globals <- gets gsGlobals
  let alreadyExists = any (`overrideExists` "show_bool") globals
  unless alreadyExists mkShowBoolFunc

mkShowBoolFunc :: IRGen ()
mkShowBoolFunc = do
  strTrue <- newStringGlobal "(true)"
  strFalse <- newStringGlobal "(false)"
  tTrue <- newTemp "fmt_true" (IRPtr IRChar)
  tFalse <- newTemp "fmt_false" (IRPtr IRChar)
  idx <- nextLabelIndex

  let func = (mkShowOverride "show_bool" IRBool)
        { irFuncBody =
            [ IRJUMP_TRUE (IRParam "value" IRBool) (makeLabel "bool_true" idx)
            , IRADDR tFalse strFalse (IRPtr IRChar)
            , IRCALL "" "printf" [IRTemp tFalse (IRPtr IRChar)] Nothing
            , IRJUMP (makeLabel "bool_end" idx)
            , IRLABEL (makeLabel "bool_true" idx)
            , IRADDR tTrue strTrue (IRPtr IRChar)
            , IRCALL "" "printf" [IRTemp tTrue (IRPtr IRChar)] Nothing
            , IRLABEL (makeLabel "bool_end" idx)
            , IRRET Nothing
            ]
        }

  modify $ \s -> s { gsGlobals = IRFunctionDef func : gsGlobals s }
  registerCall "printf"

