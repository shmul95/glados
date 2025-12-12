{-# OPTIONS_GHC -cpp #-}

#if defined(TESTING_EXPORT)
module Rune.IR.IRHelpers
  ( astTypeToIRType,
    sizeOfIRType,
    registerVar,
    registerCall,
    newTemp,
    nextLabelIndex,
    makeLabel,
    newStringGlobal,
    newFloatGlobal,
    genFormatString,
    endsWithRet,
    pushLoopContext,
    popLoopContext,
    getCurrentLoop,
    mangleMethodName,
    getOperandType,
    getCommonType,
    selectReturnType,
    irTypeToASTType,
    unsignedTypeOfWidth,
    signedTypeOfWidth,
    intWidth,
    isSigned,
    isIntType,
    promoteTypes
  )
where
#else
module Rune.IR.IRHelpers
  ( astTypeToIRType,
    sizeOfIRType,
    registerVar,
    registerCall,
    newTemp,
    nextLabelIndex,
    makeLabel,
    newStringGlobal,
    genFormatString,
    endsWithRet,
    pushLoopContext,
    popLoopContext,
    getCurrentLoop,
    mangleMethodName,
    getOperandType,
    getCommonType,
    selectReturnType
  )
where
#endif

import Control.Monad.State (gets, modify)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Rune.AST.Nodes (Type (..))
import Rune.IR.Nodes (GenState (..), IRGen, IRInstruction (..), IRLabel (..), IROperand (..), IRTopLevel (..), IRType (..), IRGlobalValue (..))
import Rune.Semantics.Type (FuncStack)
import Rune.Semantics.Helper (selectSignature)

--
-- type conversion
--

selectReturnType :: FuncStack -> String -> [IRType] -> IRType
selectReturnType fs funcName actualIRTypes =
  let actualASTTypes = map irTypeToASTType actualIRTypes
   in case selectSignature fs funcName actualASTTypes of
        Just retASTType -> astTypeToIRType retASTType
        Nothing -> error $ "Semantic error: No matching signature found for function call: " ++ funcName
                            ++ " with arguments: " ++ show actualASTTypes

-- TODO: handle more types
astTypeToIRType :: Type -> IRType
astTypeToIRType TypeI8 = IRI8
astTypeToIRType TypeI16 = IRI16
astTypeToIRType TypeI32 = IRI32
astTypeToIRType TypeI64 = IRI64
astTypeToIRType TypeU8 = IRU8
astTypeToIRType TypeU16 = IRU16
astTypeToIRType TypeU32 = IRU32
astTypeToIRType TypeU64 = IRU64
astTypeToIRType TypeChar = IRChar
astTypeToIRType TypeF32 = IRF32
astTypeToIRType TypeF64 = IRF64
astTypeToIRType TypeBool = IRBool
astTypeToIRType TypeNull = IRNull
astTypeToIRType (TypeCustom name) = IRStruct name
astTypeToIRType TypeString = IRPtr IRChar
astTypeToIRType _ = error "Unsupported type conversion from AST to IR"

irTypeToASTType :: IRType -> Type
irTypeToASTType IRI8 = TypeI8
irTypeToASTType IRI16 = TypeI16
irTypeToASTType IRI32 = TypeI32
irTypeToASTType IRI64 = TypeI64
irTypeToASTType IRF32 = TypeF32
irTypeToASTType IRF64 = TypeF64
irTypeToASTType IRBool = TypeBool
irTypeToASTType IRU8 = TypeU8
irTypeToASTType IRU16 = TypeU16
irTypeToASTType IRU32 = TypeU32
irTypeToASTType IRU64 = TypeU64
irTypeToASTType IRChar = TypeChar
irTypeToASTType IRNull = TypeNull
irTypeToASTType (IRStruct s) = TypeCustom s
irTypeToASTType (IRPtr IRChar) = TypeString
irTypeToASTType (IRPtr (IRStruct s)) = TypeCustom s
irTypeToASTType (IRPtr _) = TypeAny

-- TODO: treat struct properly
-- currently they are treated as 8 byte references/pointers
sizeOfIRType :: IRType -> Int
sizeOfIRType IRI8 = 1
sizeOfIRType IRI16 = 2
sizeOfIRType IRI32 = 4
sizeOfIRType IRI64 = 8
sizeOfIRType IRF32 = 4
sizeOfIRType IRF64 = 8
sizeOfIRType IRU8 = 1
sizeOfIRType IRChar = 1
sizeOfIRType IRU16 = 2
sizeOfIRType IRU32 = 4
sizeOfIRType IRU64 = 8
sizeOfIRType IRBool = 1
sizeOfIRType (IRPtr _) = 8 -- Ô_ö
sizeOfIRType (IRStruct _) = 8 -- ö_Ô
sizeOfIRType IRNull = 8


getCommonType :: IROperand -> IROperand -> IRType
getCommonType l r =
  case (getOperandType l, getOperandType r) of
    (Just t1, Just t2) -> promoteTypes t1 t2
    (Just t, _)        -> t
    (_, Just t)        -> t
    _                  -> IRI32


--
-- symbol table
--

registerVar :: String -> IROperand -> IRType -> IRGen ()
registerVar name op typ = do
  modify $ \s -> s {gsSymTable = Map.insert name (op, typ) (gsSymTable s)}

registerCall :: String -> IRGen ()
registerCall funcName = do
  modify $ \s -> s {gsCalledFuncs = Set.insert funcName (gsCalledFuncs s)}

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

-- | check if global string already exists
--  if so     -> return its name
--  otherwise -> create a new global string and return its name
newStringGlobal :: String -> IRGen String
newStringGlobal value = do
  mp <- gets gsStringMap
  maybe (createStringGlobal value) pure (Map.lookup value mp)

createStringGlobal :: String -> IRGen String
createStringGlobal value = do
  name <- freshStringName
  insertGlobalString name value
  pure name

freshStringName :: IRGen String
freshStringName = do
  counter <- gets gsStringCounter
  func    <- gets gsCurrentFunc
  let base = fromMaybe "global" func
      name = "str_" ++ base ++ show counter
  modify $ \s -> s { gsStringCounter = counter + 1 }
  pure name

insertGlobalString :: String -> String -> IRGen ()
insertGlobalString name value =
  modify $ \s ->
    s { gsGlobals   = IRGlobalDef name (IRGlobalStringVal value) : gsGlobals s
      , gsStringMap = Map.insert value name (gsStringMap s)
      }

newFloatGlobal :: Double -> IRType -> IRGen String
newFloatGlobal value typ = do
  mp <- gets gsFloatMap
  case Map.lookup value mp of
    Just name -> pure name
    Nothing -> createFloatGlobal value typ

createFloatGlobal :: Double -> IRType -> IRGen String
createFloatGlobal value typ = do
  counter <- gets gsFloatCounter
  func    <- gets gsCurrentFunc

  let base = fromMaybe "global" func
      name = "float_" ++ base ++ show counter
  modify $ \s ->
    s { gsFloatCounter = counter + 1
      , gsGlobals = IRGlobalDef name (IRGlobalFloatVal value typ) : gsGlobals s
      , gsFloatMap = Map.insert value name (gsFloatMap s)
      }
  pure name


genFormatString :: String -> IRGen ([IRInstruction], IROperand)
genFormatString value = do
  stringName <- newStringGlobal value
  return ([], IRGlobal stringName (IRPtr IRChar))

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

--
-- operand helpers
--

-- | determine the type of an IROperand 
getOperandType :: IROperand -> Maybe IRType
getOperandType (IRTemp _ t) = Just t
getOperandType (IRParam _ t) = Just t
getOperandType (IRGlobal _ t) = Just t
getOperandType (IRConstBool _) = Just IRBool
getOperandType (IRConstChar _) = Just IRChar
getOperandType (IRConstFloat _) = Just IRF32
getOperandType (IRConstInt _) = Just IRI32
getOperandType IRConstNull = Just IRNull

-- | promote two IRTypes to a common type according to simple rules:
--  - floats: IRF64 > IRF32
--  - integers: promote to the wider width; if mixing signed/unsigned, promote to signed of max width
--  - identical types -> that type
--  - bool/char preserved when both equal
--  - fallback -> IRI32
promoteTypes :: IRType -> IRType -> IRType
promoteTypes t1 t2
  -- float promotion:
  --  - IRF64 > IRF32 > integers
  | t1 == IRF64 || t2 == IRF64 = IRF64
  | t1 == IRF32 || t2 == IRF32 = IRF32
  -- exact same type
  | t1 == t2 = t1
  -- integer promotion (signed/unsigned, all widths)
  | isIntType t1 && isIntType t2 =
      let w1 = intWidth t1
          w2 = intWidth t2
          s1 = isSigned t1
          s2 = isSigned t2
          maxW = max w1 w2
       in case (s1, s2) of
            (True, True)   -> signedTypeOfWidth maxW
            (False, False) -> unsignedTypeOfWidth maxW
            _              -> signedTypeOfWidth maxW
  -- bool and char
  | otherwise = IRI32

--
-- promote type helpers
--

isIntType :: IRType -> Bool
isIntType IRI8   = True
isIntType IRI16  = True
isIntType IRI32  = True
isIntType IRI64  = True
isIntType IRU8   = True
isIntType IRU16  = True
isIntType IRU32  = True
isIntType IRU64  = True
isIntType _      = False

intWidth :: IRType -> Int
intWidth IRI8   = 8
intWidth IRU8   = 8
intWidth IRI16  = 16
intWidth IRU16  = 16
intWidth IRI32  = 32
intWidth IRU32  = 32
intWidth IRI64  = 64
intWidth IRU64  = 64
intWidth _      = 0

isSigned :: IRType -> Bool
isSigned IRI8   = True
isSigned IRI16  = True
isSigned IRI32  = True
isSigned IRI64  = True
isSigned _      = False

signedTypeOfWidth :: Int -> IRType
signedTypeOfWidth 8  = IRI8
signedTypeOfWidth 16 = IRI16
signedTypeOfWidth 32 = IRI32
signedTypeOfWidth 64 = IRI64
signedTypeOfWidth _  = IRI32

unsignedTypeOfWidth :: Int -> IRType
unsignedTypeOfWidth 8  = IRU8
unsignedTypeOfWidth 16 = IRU16
unsignedTypeOfWidth 32 = IRU32
unsignedTypeOfWidth 64 = IRU64
unsignedTypeOfWidth _  = IRU32
