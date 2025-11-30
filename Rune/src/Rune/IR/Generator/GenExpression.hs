module Rune.IR.Generator.GenExpression (genExpression) where

import Control.Monad.State (gets)
import qualified Data.Map.Strict as Map
import Rune.AST.Nodes (BinaryOp (..), Expression (..), UnaryOp (..))
import Rune.IR.IRHelpers (newStringGlobal, newTemp)
import Rune.IR.Nodes
  ( GenState (..),
    IRGen,
    IRInstruction (..),
    IROperand (..),
    IRType (..),
  )

--
-- public
--

genExpression :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genExpression expr = case expr of
  ExprLitInt n -> genLitInt n
  ExprLitFloat f -> genLitFloat f
  ExprLitChar c -> genLitChar c
  ExprLitBool True -> genLitBool True
  ExprLitBool False -> genLitBool False
  ExprLitNull -> genLitNull
  ExprLitString s -> genLitString s
  ExprVar name -> genVar name
  ExprBinary op l r -> genBinary op l r
  ExprUnary op e -> genUnary op e
  ExprCall "show" [a] -> genShowCall a
  ExprCall name args -> genCall name args
  ExprAccess t f -> genAccess t f
  ExprStructInit name fields -> genStructInit name fields

--
-- private helpers
--

binaryInstr :: BinaryOp -> String -> IROperand -> IROperand -> IRType -> IRInstruction
binaryInstr Add res l r t = IRADD_OP res l r t
binaryInstr Sub res l r t = IRSUB_OP res l r t
binaryInstr Mul res l r t = IRMUL_OP res l r t
binaryInstr Div res l r t = IRDIV_OP res l r t
binaryInstr Mod res l r t = IRMOD_OP res l r t
binaryInstr Eq res l r _ = IRCMP_EQ res l r
binaryInstr Neq res l r _ = IRCMP_NEQ res l r
binaryInstr Lt res l r _ = IRCMP_LT res l r
binaryInstr Lte res l r _ = IRCMP_LTE res l r
binaryInstr Gt res l r _ = IRCMP_GT res l r
binaryInstr Gte res l r _ = IRCMP_GTE res l r
binaryInstr And res l r t = IRAND_OP res l r t
binaryInstr Or res l r t = IROR_OP res l r t

--
-- private generators
--

genLitInt :: Int -> IRGen ([IRInstruction], IROperand, IRType)
genLitInt n = return ([], IRConstInt n, IRI32)

genLitFloat :: Double -> IRGen ([IRInstruction], IROperand, IRType)
genLitFloat f = return ([], IRConstFloat f, IRF32)

genLitChar :: Char -> IRGen ([IRInstruction], IROperand, IRType)
genLitChar c = return ([], IRConstChar c, IRU8)

genLitBool :: Bool -> IRGen ([IRInstruction], IROperand, IRType)
genLitBool True = return ([], IRConstInt 1, IRI32)
genLitBool False = return ([], IRConstInt 0, IRI32)

genLitNull :: IRGen ([IRInstruction], IROperand, IRType)
genLitNull = return ([], IRConstInt 0, IRVoid)

genLitString :: String -> IRGen ([IRInstruction], IROperand, IRType)
genLitString s = do
  stringName <- newStringGlobal s
  ptrName <- newTemp "p_ptr" (IRPtr IRU8)
  let addrInstr = IRADDR ptrName stringName (IRPtr IRU8)
  return ([addrInstr], IRTemp ptrName (IRPtr IRU8), IRPtr IRU8)

genVar :: String -> IRGen ([IRInstruction], IROperand, IRType)
genVar name = do
  symTable <- gets gsSymTable
  case Map.lookup name symTable of
    Just (op, typ) -> return ([], op, typ)
    Nothing -> return ([], IRTemp name IRI32, IRI32)

genBinary :: BinaryOp -> Expression -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genBinary op left right = do
  (lInstrs, lOp, lType) <- genExpression left
  (rInstrs, rOp, _) <- genExpression right

  resultTemp <- newTemp "t" lType
  let opInstr = binaryInstr op resultTemp lOp rOp lType

  return (lInstrs ++ rInstrs ++ [opInstr], IRTemp resultTemp lType, lType)

genUnary :: UnaryOp -> Expression -> IRGen ([IRInstruction], IROperand, IRType)
genUnary op expr = do
  (instrs, operand, typ) <- genExpression expr
  case op of
    Negate -> do
      resultTemp <- newTemp "t" typ
      let negInstr = IRSUB_OP resultTemp (IRConstInt 0) operand typ
      return (instrs ++ [negInstr], IRTemp resultTemp typ, typ)
    PrefixInc -> return (instrs ++ [IRINC operand], operand, typ)
    PrefixDec -> return (instrs ++ [IRDEC operand], operand, typ)
    PostfixInc -> do
      resultTemp <- newTemp "t" typ
      return (instrs ++ [IRASSIGN resultTemp operand typ, IRINC operand], IRTemp resultTemp typ, typ)
    PostfixDec -> do
      resultTemp <- newTemp "t" typ
      return (instrs ++ [IRASSIGN resultTemp operand typ, IRDEC operand], IRTemp resultTemp typ, typ)
    PropagateError -> return (instrs, operand, typ)

genShowCall :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
genShowCall arg = do
  (argInstrs, argOp, argType) <- genExpression arg

  let functionName = case argType of
        IRStruct s -> "show_" ++ s
        IRPtr (IRStruct s) -> "show_" ++ s
        IRPtr IRU8 -> "puts"
        IRU8 -> "putchar"
        _ -> "printf"

  let (prepInstrs, finalOp) = case argType of
        IRStruct _ -> case argOp of
          IRTemp name t -> ([IRADDR ("addr_" ++ name) name (IRPtr t)], IRTemp ("addr_" ++ name) (IRPtr t))
          _ -> ([], argOp)
        _ -> ([], argOp)

  let callInstr = IRCALL "" functionName [finalOp] Nothing
  return (argInstrs ++ prepInstrs ++ [callInstr], IRTemp "t_void" IRVoid, IRVoid)

genCall :: String -> [Expression] -> IRGen ([IRInstruction], IROperand, IRType)
genCall funcName args = do
  argsData <- mapM genExpression args

  let mangledName = case argsData of
        ((_, _, IRStruct s) : _) -> s ++ "_" ++ funcName
        ((_, _, IRPtr (IRStruct s)) : _) -> s ++ "_" ++ funcName
        _ -> funcName

  let (argInstrs, argOps) =
        unzip $
          map
            ( \(i, o, t) ->
                case t of
                  IRStruct _ -> case o of
                    IRTemp n _ -> (i ++ [IRADDR ("p_" ++ n) n (IRPtr t)], IRTemp ("p_" ++ n) (IRPtr t))
                    _ -> (i, o)
                  _ -> (i, o)
            )
            argsData

  let allInstrs = concat argInstrs
  let retType =
        case argsData of
          ((_, _, IRStruct s) : _) -> IRStruct s
          ((_, _, IRPtr (IRStruct s)) : _) -> IRStruct s
          _ -> IRI32

  retTemp <- newTemp "t" retType
  let callInstr = IRCALL retTemp mangledName argOps (Just retType)

  return (allInstrs ++ [callInstr], IRTemp retTemp retType, retType)

genAccess :: Expression -> String -> IRGen ([IRInstruction], IROperand, IRType)
genAccess target field = do
  (tInstrs, tOp, tType) <- genExpression target

  let (structName, ptrOp, setupInstrs) = case tType of
        IRStruct s -> case tOp of
          IRTemp n _ -> (s, IRTemp ("p_" ++ n) (IRPtr tType), [IRADDR ("p_" ++ n) n (IRPtr tType)])
          _ -> (s, tOp, [])
        IRPtr (IRStruct s) -> (s, tOp, [])
        _ -> error $ "Cannot access field '" ++ field ++ "' of non-struct type"

  structs <- gets gsStructs
  let fieldType = case Map.lookup structName structs of
        Just fields -> case lookup field fields of
          Just t -> t
          Nothing -> IRI32
        Nothing -> IRI32
  resTemp <- newTemp ("f_" ++ field) fieldType
  let getInstr = IRGET_FIELD resTemp ptrOp structName field fieldType

  return (tInstrs ++ setupInstrs ++ [getInstr], IRTemp resTemp fieldType, fieldType)

genStructInit :: String -> [(String, Expression)] -> IRGen ([IRInstruction], IROperand, IRType)
genStructInit name fields = do
  let structType = IRStruct name

  resName <- newTemp "struct" structType
  let allocInstr = IRALLOC resName structType
      resOp = IRTemp resName structType

  fieldInstrs <-
    mapM
      ( \(fName, fExpr) -> do
          (valInstrs, valOp, _) <- genExpression fExpr
          ptrName <- newTemp "p_init" (IRPtr structType)
          let addrInstr = IRADDR ptrName resName (IRPtr structType)
              setInstr = IRSET_FIELD (IRTemp ptrName (IRPtr structType)) name fName valOp
          return (valInstrs ++ [addrInstr, setInstr])
      )
      fields

  return ([allocInstr] ++ concat fieldInstrs, resOp, structType)
