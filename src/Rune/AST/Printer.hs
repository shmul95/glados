{-# LANGUAGE TupleSections #-}

module Rune.AST.Printer (prettyPrint) where

import Data.List (intercalate)
import Rune.AST.Nodes
import Rune.AST.Visitor (RuneVisitor (..))

data PrinterState = PrinterState
  { psIndent :: Int,
    psOutput :: String
  }

newtype Printer a = Printer {runState :: PrinterState -> (a, PrinterState)}

instance Functor Printer where
  fmap f (Printer p) = Printer $ \s ->
    let (x, s') = p s
     in (f x, s')

instance Applicative Printer where
  pure x = Printer (x,)
  (Printer pf) <*> (Printer px) = Printer $ \s ->
    let (f, s') = pf s
        (x, s'') = px s'
     in (f x, s'')

instance Monad Printer where
  (Printer p) >>= f = Printer $ \s ->
    let (x, s') = p s
        (Printer p') = f x
     in p' s'

prettyPrint :: Program -> String
prettyPrint prog =
  let initialState = PrinterState 0 ""
      (_, finalState) = runState (visitProgram prog) initialState
   in psOutput finalState

instance RuneVisitor Printer where
  visitFunction (DefFunction name params retType body) = do
    emit $ "def " ++ name
    emitParams params
    emit $ " -> " ++ showType retType
    newLine
    emit "{"
    visitBody body
    newLine
    emit "}"
    newLine
    newLine
  visitFunction _ = return ()

  visitStruct (DefStruct name fields methods) = do
    emit $ "struct " ++ name
    newLine
    emit "{"
    indent
    mapM_ (\f -> newLine >> emitField f) fields
    mapM_ (\m -> newLine >> visitTopLevel m) methods
    dedent
    newLine
    emit "}"
    newLine
    newLine
    where
      emitField (Field n t) = emit (n ++ ": " ++ showType t ++ ";")
  visitStruct _ = return ()

  visitOverride (DefOverride name params retType body) = do
    emit $ "override def " ++ name
    emitParams params
    emit $ " -> " ++ showType retType
    newLine
    emit "{"
    visitBody body
    newLine
    emit "}"
    newLine
    newLine
  visitOverride _ = return ()

  visitVarDecl name maybeType expr = do
    emit name
    case maybeType of
      Just t -> emit $ ": " ++ showType t
      Nothing -> return ()
    emit " = "
    visitExpression expr
    emit ";"

  visitReturn maybeExpr = do
    emit "return"
    case maybeExpr of
      Just e -> emit " " >> visitExpression e
      Nothing -> return ()
    emit ";"

  visitIf cond thenB elseB = do
    emit "if "
    visitExpression cond
    emit " {"
    visitBody thenB
    newLine
    emit "}"
    case elseB of
      Just eb -> do
        emit " else {"
        visitBody eb
        newLine
        emit "}"
      Nothing -> return ()

  visitFor name start end body = do
    emit $ "for " ++ name ++ " = "
    visitExpression start
    emit " to "
    visitExpression end
    emit " {"
    visitBody body
    newLine
    emit "}"

  visitForEach name iterable body = do
    emit $ "for " ++ name ++ " in "
    visitExpression iterable
    emit " {"
    visitBody body
    newLine
    emit "}"

  visitExpression (ExprBinary op l r) = do
    emit "("
    visitExpression l
    emit $ " " ++ showBinaryOp op ++ " "
    visitExpression r
    emit ")"
  visitExpression (ExprUnary op val) = do
    emit (showUnaryOp op)
    emit "("
    visitExpression val
    emit ")"
  visitExpression (ExprCall name args) = do
    emit $ name ++ "("
    printArgs args
    emit ")"
  visitExpression (ExprStructInit name fields) = do
    emit $ name ++ " { "
    printInitFields fields
    emit " }"
  visitExpression (ExprAccess target field) = do
    visitExpression target
    emit $ "." ++ field
  visitExpression (ExprLitInt i) = emit $ show i
  visitExpression (ExprLitFloat f) = emit $ show f
  visitExpression (ExprLitString s) = emit $ show s
  visitExpression (ExprLitBool b) = emit $ if b then "true" else "false"
  visitExpression ExprLitNull = emit "null"
  visitExpression (ExprVar v) = emit v

visitBody :: Block -> Printer ()
visitBody block = do
  indent
  mapM_ (\stmt -> newLine >> visitStatement stmt) block
  dedent

emit :: String -> Printer ()
emit str = Printer $ \s -> ((), s {psOutput = psOutput s ++ str})

newLine :: Printer ()
newLine = Printer $ \s ->
  let indentStr = replicate (psIndent s * 4) ' '
   in ((), s {psOutput = psOutput s ++ "\n" ++ indentStr})

indent :: Printer ()
indent = Printer $ \s -> ((), s {psIndent = psIndent s + 1})

dedent :: Printer ()
dedent = Printer $ \s -> ((), s {psIndent = psIndent s - 1})

emitParams :: [Parameter] -> Printer ()
emitParams params = do
  emit "("
  let pStrs = map (\p -> paramName p ++ ": " ++ showType (paramType p)) params
  emit (intercalate ", " pStrs)
  emit ")"

printArgs :: [Expression] -> Printer ()
printArgs [] = return ()
printArgs [x] = visitExpression x
printArgs (x : xs) = visitExpression x >> emit ", " >> printArgs xs

printInitFields :: [(String, Expression)] -> Printer ()
printInitFields [] = return ()
printInitFields [(n, e)] = emit (n ++ ": ") >> visitExpression e
printInitFields ((n, e) : xs) = emit (n ++ ": ") >> visitExpression e >> emit ", " >> printInitFields xs

showType :: Type -> String
showType TypeI8 = "i8"
showType TypeI16 = "i16"
showType TypeI32 = "i32"
showType TypeI64 = "i64"
showType TypeF32 = "f32"
showType TypeF64 = "f64"
showType TypeBool = "bool"
showType TypeU8 = "u8"
showType TypeU16 = "u16"
showType TypeU32 = "u32"
showType TypeString = "string"
showType TypeAny = "any"
showType TypeNull = "null"
showType (TypeCustom s) = s

showBinaryOp :: BinaryOp -> String
showBinaryOp Add = "+"
showBinaryOp Sub = "-"
showBinaryOp Mul = "*"
showBinaryOp Div = "/"
showBinaryOp Mod = "%"
showBinaryOp Eq = "=="
showBinaryOp Neq = "!="
showBinaryOp Lt = "<"
showBinaryOp Lte = "<="
showBinaryOp Gt = ">"
showBinaryOp Gte = ">="
showBinaryOp And = "&&"
showBinaryOp Or = "||"

showUnaryOp :: UnaryOp -> String
showUnaryOp Negate = "-"
showUnaryOp PropagateError = "?"
