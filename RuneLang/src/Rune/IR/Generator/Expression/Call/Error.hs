{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.IR.Generator.Expression.Call.Error
  ( genErrorCall,
    genErrorBoolCall,
    genErrorCharCall,
    genErrorPrintfCall,
    getErrorFunc,
    ensureErrorBoolFunc,
    mkErrorBoolFunc
  )
where
#else
module Rune.IR.Generator.Expression.Call.Error (genErrorCall) where
#endif

import Control.Monad (unless)
import Control.Monad.State (gets, modify)
import Rune.AST.Nodes (Expression)
import Rune.IR.Generator.Expression.Call.Show
  ( genShowFmtCall,
    prepareAddr,
    mkShowOverride,
    overrideExists
  )
import Rune.IR.IRHelpers (registerCall, newStringGlobal, newTemp, nextLabelIndex, makeLabel)
import Rune.IR.Nodes (IRGen, GenState(..), IRInstruction (..), IROperand (..), IRType (..), IRTopLevel(..), IRFunction(..))

--
-- callbacks to avoid circular dependencies
--

type GenExprCallback = Expression -> IRGen ([IRInstruction], IROperand, IRType)

stderrFd :: IROperand
stderrFd = IRConstInt 2

--
-- public
--

genErrorCall :: GenExprCallback -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genErrorCall genExpr arg = do
  (instrs, op, typ) <- genExpr arg
  case typ of
    IRBool -> genErrorBoolCall instrs op
    IRChar -> genErrorCharCall instrs op
    _ -> genErrorPrintfCall instrs op typ

--
-- def error(value: any) -> null
--

getErrorFunc :: IROperand -> IRType -> String
getErrorFunc _ (IRStruct s) = "error_" <> s
getErrorFunc _ (IRPtr (IRStruct s)) = "error_" <> s
getErrorFunc _ _ = "dprintf"

--
-- def error(value: bool) -> null
--

genErrorBoolCall :: [IRInstruction] -> IROperand -> IRGen ([IRInstruction], IROperand, IRType)
genErrorBoolCall instrs op = do
  ensureErrorBoolFunc
  registerCall "error_bool"
  let callInstr = IRCALL "" "error_bool" [op] Nothing
  return (instrs <> [callInstr], IRTemp "t_null" IRNull, IRNull)

--
-- def error(value: char) -> null
--

genErrorCharCall :: [IRInstruction] -> IROperand -> IRGen ([IRInstruction], IROperand, IRType)
genErrorCharCall instrs op = genErrorPrintfCall instrs op IRChar

genErrorPrintfCall :: [IRInstruction] -> IROperand -> IRType -> IRGen ([IRInstruction], IROperand, IRType)
genErrorPrintfCall instrs op typ = do
  let funcName = getErrorFunc op typ
      (prep, finalOp) = prepareAddr op typ

  registerCall funcName
  (fmtInstrs, callArgs) <- genShowFmtCall op typ finalOp

  let args = case funcName of
        "dprintf" -> stderrFd : callArgs
        _ -> callArgs
      callInstr = IRCALL "" funcName args Nothing
  return (instrs <> prep <> fmtInstrs <> [callInstr], IRTemp "t_null" IRNull, IRNull)

--
-- bool helper
--

ensureErrorBoolFunc :: IRGen ()
ensureErrorBoolFunc = do
  globals <- gets gsGlobals
  let alreadyExists = any (`overrideExists` "error_bool") globals
  unless alreadyExists mkErrorBoolFunc

mkErrorBoolFunc :: IRGen ()
mkErrorBoolFunc = do
  strTrue <- newStringGlobal "(true)"
  strFalse <- newStringGlobal "(false)"
  tTrue <- newTemp "fmt_true" (IRPtr IRChar)
  tFalse <- newTemp "fmt_false" (IRPtr IRChar)
  idx <- nextLabelIndex

  let func = (mkShowOverride "error_bool" IRBool)
        { irFuncBody =
            [ IRJUMP_TRUE (IRParam "value" IRBool) (makeLabel "bool_true" idx)
            , IRADDR tFalse strFalse (IRPtr IRChar)
            , IRCALL "" "dprintf" [stderrFd, IRTemp tFalse (IRPtr IRChar)] Nothing
            , IRJUMP (makeLabel "bool_end" idx)
            , IRLABEL (makeLabel "bool_true" idx)
            , IRADDR tTrue strTrue (IRPtr IRChar)
            , IRCALL "" "dprintf" [stderrFd, IRTemp tTrue (IRPtr IRChar)] Nothing
            , IRLABEL (makeLabel "bool_end" idx)
            , IRRET Nothing
            ]
        }

  modify $ \s -> s { gsGlobals = IRFunctionDef func : gsGlobals s }
  registerCall "dprintf"
