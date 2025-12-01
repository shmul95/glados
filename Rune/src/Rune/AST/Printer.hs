{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Rune.AST.Printer (prettyPrint) where

import Control.Monad (void)
import Rune.AST.Nodes
import Rune.AST.Visitor (RuneVisitor (..))

--
-- state Monad for pretty-printing using ShowS (Efficient string building)
--

data PrinterState = PrinterState
  { psIndent :: Int,
    psOutput :: ShowS
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

--
-- public
--

prettyPrint :: Program -> String
prettyPrint prog =
  let initialState = PrinterState 0 id
      (_, finalState) = runState (visitProgram prog) initialState
   in psOutput finalState ""

--
-- private AST visitor instanciation
--

instance RuneVisitor Printer where
  visitProgram :: Program -> Printer ()
  visitProgram (Program name defs) = do
    emit $ "Program: " ++ name
    indent
    mapM_ (\d -> newLine >> visitTopLevel d) defs
    dedent

  visitFunction :: TopLevelDef -> Printer ()
  visitFunction (DefFunction name params retType body) = do
    emit $ "DefFunction " ++ name
    indent
    emitBlock "Parameters:" (mapM_ emitParam params)
    newLine
    emit $ "ReturnType: " ++ showType retType
    emitBlock "Body:" (visitBody body)
    dedent
  visitFunction _ = return ()

  visitStruct :: TopLevelDef -> Printer ()
  visitStruct (DefStruct name fields methods) = do
    emit $ "DefStruct " ++ name
    indent
    emitBlock "Fields:" (mapM_ emitField fields)
    emitBlock "Methods:" (mapM_ (\m -> newLine >> visitTopLevel m) methods)
    dedent
    where
      emitField (Field n t) = newLine >> emit (n ++ ": " ++ showType t)
  visitStruct _ = return ()

  visitOverride :: TopLevelDef -> Printer ()
  visitOverride (DefOverride name params retType body) = do
    emit $ "DefOverride " ++ name
    indent
    emitBlock "Parameters:" (mapM_ emitParam params)
    newLine
    emit $ "ReturnType: " ++ showType retType
    emitBlock "Body:" (visitBody body)
    dedent
  visitOverride _ = return ()

  visitVarDecl :: String -> Maybe Type -> Expression -> Printer ()
  visitVarDecl name maybeType expr = do
    emit $ "StmtVarDecl " ++ name
    case maybeType of
      Just t -> emit $ " : " ++ showType t
      Nothing -> return ()
    emitBlock "Value:" (newLine >> visitExpression expr)

  visitAssignment :: Expression -> Expression -> Printer ()
  visitAssignment l r = do
    emit "StmtAssignment"
    indent
    emitBlock "LValue:" (newLine >> visitExpression l)
    emitBlock "RValue:" (newLine >> visitExpression r)
    dedent

  visitReturn :: Maybe Expression -> Printer ()
  visitReturn maybeExpr = do
    emit "StmtReturn"
    case maybeExpr of
      Just e -> do
        indent
        newLine
        visitExpression e
        dedent
      Nothing -> return ()

  visitIf :: Expression -> Block -> Maybe Block -> Printer ()
  visitIf cond thenB elseB = do
    emit "StmtIf"
    indent
    emitBlock "Condition:" (newLine >> visitExpression cond)
    emitBlock "Then:" (visitBody thenB)
    case elseB of
      Just eb -> emitBlock "Else:" (visitBody eb)
      Nothing -> return ()
    dedent

  visitFor :: String -> Maybe Type -> Maybe Expression -> Expression -> Block -> Printer ()
  visitFor name maybeType mStart end body = do
    emit $ "StmtFor " ++ name
    case maybeType of
      Just t -> emit $ " : " ++ showType t
      Nothing -> return ()
    indent
    case mStart of
      Just start -> emitBlock "Start:" (newLine >> visitExpression start)
      Nothing -> newLine >> emit "Start: <Implicit>"
    emitBlock "End:" (newLine >> visitExpression end)
    emitBlock "Body:" (visitBody body)
    dedent

  visitForEach :: String -> Maybe Type -> Expression -> Block -> Printer ()
  visitForEach name maybeType iterable body = do
    emit $ "StmtForEach " ++ name
    case maybeType of
      Just t -> emit $ " : " ++ showType t
      Nothing -> return ()
    indent
    emitBlock "Iterable:" (newLine >> visitExpression iterable)
    emitBlock "Body:" (visitBody body)
    dedent

  visitLoop :: Block -> Printer ()
  visitLoop body = do
    emit "StmtLoop"
    indent
    emitBlock "Body:" (visitBody body)
    dedent

  visitStop :: Printer ()
  visitStop = emit "StmtStop"

  visitNext :: Printer ()
  visitNext = emit "StmtNext"

  visitStatement :: Statement -> Printer ()
  visitStatement (StmtVarDecl name typeDecl expr) = visitVarDecl name typeDecl expr
  visitStatement (StmtAssignment l r) = visitAssignment l r
  visitStatement (StmtReturn expr) = visitReturn expr
  visitStatement (StmtIf cond thenB elseB) = visitIf cond thenB elseB
  visitStatement (StmtFor var t mStart end body) = visitFor var t mStart end body
  visitStatement (StmtForEach var t iterable body) = visitForEach var t iterable body
  visitStatement (StmtLoop body) = visitLoop body
  visitStatement StmtStop = visitStop
  visitStatement StmtNext = visitNext
  visitStatement (StmtExpr expr) = do
    emit "StmtExpr"
    indent
    newLine
    visitExpression expr
    dedent

  visitExpression :: Expression -> Printer ()
  visitExpression (ExprBinary op l r) = do
    emit $ "ExprBinary " ++ showBinaryOp op
    indent
    newLine
    visitExpression l
    newLine
    visitExpression r
    dedent
  visitExpression (ExprUnary op val) = do
    emit $ "ExprUnary " ++ showUnaryOp op
    indent
    newLine
    visitExpression val
    dedent
  visitExpression (ExprCall name args) = do
    emit $ "ExprCall " ++ name
    emitBlock "Arguments:" (mapM_ (\a -> newLine >> visitExpression a) args)
  visitExpression (ExprStructInit name fields) = do
    emit $ "ExprStructInit " ++ name
    emitBlock "Fields:" (mapM_ emitInitField fields)
    where
      emitInitField (n, e) = do
        newLine
        emit $ n ++ ":"
        emitBlock "" (newLine >> visitExpression e)
  visitExpression (ExprAccess target field) = do
    emit $ "ExprAccess ." ++ field
    indent
    newLine
    visitExpression target
    dedent
  visitExpression (ExprLitInt i) = emit $ "ExprLitInt " ++ show i
  visitExpression (ExprLitFloat f) = emit $ "ExprLitFloat " ++ show f
  visitExpression (ExprLitString s) = emit $ "ExprLitString " ++ show s
  visitExpression (ExprLitChar c) = emit $ "ExprLitChar " ++ show c
  visitExpression (ExprLitBool b) = emit $ "ExprLitBool " ++ show b
  visitExpression ExprLitNull = emit "ExprLitNull"
  visitExpression (ExprVar v) = emit $ "ExprVar " ++ v

--
-- private helpers
--

emit :: String -> Printer ()
emit str = Printer $ \s -> ((), s {psOutput = psOutput s . showString str})

emitBlock :: String -> Printer a -> Printer ()
emitBlock label action = do
  newLine
  emit label
  indent
  void action
  dedent

visitBody :: Block -> Printer ()
visitBody = mapM_ (\stmt -> newLine >> visitStatement stmt)

newLine :: Printer ()
newLine = Printer $ \s ->
  let indentStr = replicate (psIndent s * 2) ' '
      newlineAndIndent = showString "\n" . showString indentStr
   in ((), s {psOutput = psOutput s . newlineAndIndent})

indent :: Printer ()
indent = Printer $ \s -> ((), s {psIndent = psIndent s + 1})

dedent :: Printer ()
dedent = Printer $ \s -> ((), s {psIndent = psIndent s - 1})

emitParam :: Parameter -> Printer ()
emitParam p = newLine >> emit (paramName p ++ ": " ++ showType (paramType p))

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
showUnaryOp PrefixInc = "++(prefix)"
showUnaryOp PrefixDec = "--(prefix)"
showUnaryOp PostfixInc = "(postfix)++"
showUnaryOp PostfixDec = "(postfix)--"
