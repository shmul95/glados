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
  )
where

import Control.Monad.State (gets, modify)
import Data.Map.Strict (insert)
import Rune.AST.Nodes (Type (..))
import Rune.IR.Nodes (GenState (..), IRGen, IRInstruction (..), IRLabel (..), IROperand (..), IRTopLevel (..), IRType (..))

--
-- public
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

-- | registers a variable in the symtable
registerVar :: String -> IROperand -> IRType -> IRGen ()
registerVar name op typ = do
  modify $ \s -> s {gsSymTable = insert name (op, typ) (gsSymTable s)}

-- | generates a new temporary variable name
newTemp :: String -> IRType -> IRGen String
newTemp prefix _ = do
  counter <- gets gsTempCounter
  modify $ \s -> s {gsTempCounter = counter + 1}
  return $ prefix ++ show counter

-- | generates the next label index
nextLabelIndex :: IRGen Int
nextLabelIndex = do
  counter <- gets gsLabelCounter
  modify $ \s -> s {gsLabelCounter = counter + 1}
  return counter

-- | makes a label from a prefix and index
makeLabel :: String -> Int -> IRLabel
makeLabel prefix idx = IRLabel $ ".L." ++ prefix ++ show idx

-- | creates a new global string and returns its name
newStringGlobal :: String -> IRGen String
newStringGlobal value = do
  counter <- gets gsStringCounter
  maybeFuncName <- gets gsCurrentFunc
  let baseName = maybe "global" id maybeFuncName
  let name = "str_" ++ baseName ++ show counter
  modify $ \s ->
    s
      { gsStringCounter = counter + 1,
        gsGlobals = IRGlobalString name value : gsGlobals s
      }
  return name

-- | checks if a list of IR instructions ends with a return or jump instruction
endsWithRet :: [IRInstruction] -> Bool
endsWithRet [] = False
endsWithRet xs = case reverse xs of
  (IRRET _ : _) -> True
  (IRJUMP _ : _) -> True
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
