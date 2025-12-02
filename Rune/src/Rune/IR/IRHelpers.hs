module Rune.IR.IRHelpers
  ( astTypeToIRType,
    registerVar,
    newTemp,
    nextLabelIndex,
    makeLabel,
    newStringGlobal,
    endsWithRet,
    pushLoopContext,
    popLoopContext,
    getCurrentLoop,
    mangleMethodName,
  )
where

import Control.Monad.State (gets, modify)
import Data.Map.Strict (insert)
import Data.Maybe (fromMaybe)
import Rune.AST.Nodes (Type (..))
import Rune.IR.Nodes (GenState (..), IRGen, IRInstruction (..), IRLabel (..), IROperand (..), IRTopLevel (..), IRType (..))

--
-- type conversion
--

astTypeToIRType :: Type -> IRType
astTypeToIRType TypeI32 = IRI32
astTypeToIRType TypeI64 = IRI64
astTypeToIRType TypeF32 = IRF32
astTypeToIRType TypeF64 = IRF64
astTypeToIRType TypeU8 = IRU8
astTypeToIRType TypeString = IRPtr IRU8
astTypeToIRType TypeNull = IRVoid
astTypeToIRType (TypeCustom s) = IRStruct s
astTypeToIRType _ = IRI32

--
-- symbol table
--

registerVar :: String -> IROperand -> IRType -> IRGen ()
registerVar name op typ = do
  modify $ \s -> s {gsSymTable = insert name (op, typ) (gsSymTable s)}

--
-- naming & globals
--

newTemp :: String -> IRType -> IRGen String
newTemp prefix _ = do
  counter <- gets gsTempCounter
  modify $ \s -> s {gsTempCounter = counter + 1}
  return $ prefix ++ show counter

nextLabelIndex :: IRGen Int
nextLabelIndex = do
  counter <- gets gsLabelCounter
  modify $ \s -> s {gsLabelCounter = counter + 1}
  return counter

makeLabel :: String -> Int -> IRLabel
makeLabel prefix idx = IRLabel $ ".L." ++ prefix ++ show idx

newStringGlobal :: String -> IRGen String
newStringGlobal value = do
  counter <- gets gsStringCounter
  maybeFuncName <- gets gsCurrentFunc
  let baseName = fromMaybe "global" maybeFuncName
      name = "str_" ++ baseName ++ show counter
  modify $ \s ->
    s
      { gsStringCounter = counter + 1,
        gsGlobals = IRGlobalString name value : gsGlobals s
      }
  return name

mangleMethodName :: String -> String -> String
mangleMethodName structName methodName = structName ++ "_" ++ methodName

--
-- control flow
--

endsWithRet :: [IRInstruction] -> Bool
endsWithRet [] = False
endsWithRet xs = case last xs of
  IRRET _ -> True
  IRJUMP _ -> True
  _ -> False

pushLoopContext :: IRLabel -> IRLabel -> IRGen ()
pushLoopContext header end =
  modify $ \s -> s {gsLoopStack = (header, end) : gsLoopStack s}

popLoopContext :: IRGen ()
popLoopContext =
  modify $ \s -> s {gsLoopStack = drop 1 (gsLoopStack s)}

getCurrentLoop :: IRGen (Maybe (IRLabel, IRLabel))
getCurrentLoop = do
  stack <- gets gsLoopStack
  case stack of
    (current : _) -> return (Just current)
    [] -> return Nothing
