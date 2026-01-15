{-# LANGUAGE CPP #-}

#if defined(TESTING_EXPORT)
module Rune.AST.Printer
  ( prettyPrint,
    visitProgram,
    visitTopLevel,
    visitFunction,
    visitStruct,
    visitStatement,
    visitVarDecl,
    visitAssignment,
    visitReturn,
    visitIf,
    visitFor,
    visitForEach,
    visitLoop,
    visitStop,
    visitNext,
    visitExpression,
    visitBody,
    newLine,
    indent,
    dedent,
    emitParam,
    showType,
    showBinaryOp,
    showUnaryOp,
    PrinterState(..),
    Printer
  )
where
#else
module Rune.AST.Printer (prettyPrint) where
#endif

import Control.Monad (void, when)
import Control.Monad.State.Strict (State, execState, get, modify)
import Rune.AST.Nodes

--
-- state Monad for pretty-printing using ShowS
--

data PrinterState = PrinterState
  { psIndent :: Int,
    psOutput :: ShowS
  }

type Printer a = State PrinterState a

--
-- public entry point
--

prettyPrint :: Program -> String
prettyPrint prog =
  let initialState = PrinterState 0 id
      finalState = execState (visitProgram prog) initialState
   in psOutput finalState ""

--
-- AST visitor functions
--

visitProgram :: Program -> Printer ()
visitProgram (Program name defs) = do
  emit $ "Program: " <> name
  indent
  mapM_ (\d -> newLine >> visitTopLevel d) defs
  dedent

visitTopLevel :: TopLevelDef -> Printer ()
visitTopLevel d@DefFunction {} = visitFunction d
visitTopLevel d@DefStruct {} = visitStruct d
visitTopLevel d@DefSomewhere {} = visitSomewhere d

visitFunction :: TopLevelDef -> Printer ()
visitFunction (DefFunction name params retType body isExport) = do
  emit $ (if isExport then "export " else "") <> "DefFunction " <> name
  indent
  emitBlock "Parameters:" (mapM_ emitParam params)
  newLine
  emit $ "ReturnType: " <> showType retType
  emitBlock "Body:" (visitBody body)
  dedent
visitFunction _ = return ()

visitStruct :: TopLevelDef -> Printer ()
visitStruct (DefStruct name fields methods) = do
  emit $ "DefStruct " <> name
  indent
  emitBlock "Fields:" (mapM_ emitField fields)
  emitBlock "Methods:" (mapM_ (\m -> newLine >> visitTopLevel m) methods)
  dedent
  where
    emitField (Field n t) = newLine >> emit (n <> ": " <> showType t)
visitStruct _ = return ()

visitSomewhere :: TopLevelDef -> Printer ()
visitSomewhere (DefSomewhere sigs) = do
  emit "DefSomewhere"
  indent
  emitBlock "Signatures:" (mapM_ emitSig sigs)
  dedent
  where
    emitSig (FunctionSignature name paramTypes retType isExtern) = do
      newLine
      when isExtern $ emit "extern "
      emit $ name <> "("
      emit $ unwords (map showType paramTypes)
      emit $ ") -> " <> showType retType
visitSomewhere _ = return ()

visitStatement :: Statement -> Printer ()
visitStatement (StmtVarDecl _ name typeDecl expr) = visitVarDecl name typeDecl expr
visitStatement (StmtAssignment _ l r) = visitAssignment l r
visitStatement (StmtReturn _ expr) = visitReturn expr
visitStatement (StmtIf _ cond thenB elseB) = visitIf cond thenB elseB
visitStatement (StmtFor _ var t mStart end body) = visitFor var t mStart end body
visitStatement (StmtForEach _ var t iterable body) = visitForEach var t iterable body
visitStatement (StmtLoop _ body) = visitLoop body
visitStatement (StmtStop _) = visitStop
visitStatement (StmtNext _) = visitNext
visitStatement (StmtExpr _ expr) = do
  emit "StmtExpr"
  indent
  newLine
  visitExpression expr
  dedent

visitVarDecl :: String -> Maybe Type -> Expression -> Printer ()
visitVarDecl name maybeType expr = do
  emit $ "StmtVarDecl " <> name
  case maybeType of
    Just t -> emit $ " : " <> showType t
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
  emit $ "StmtFor " <> name
  case maybeType of
    Just t -> emit $ " : " <> showType t
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
  emit $ "StmtForEach " <> name
  case maybeType of
    Just t -> emit $ " : " <> showType t
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

visitExpression :: Expression -> Printer ()
visitExpression (ExprBinary _ op l r) = do
  emit $ "ExprBinary " <> showBinaryOp op
  indent
  newLine
  visitExpression l
  newLine
  visitExpression r
  dedent
visitExpression (ExprUnary _ op val) = do
  emit $ "ExprUnary " <> showUnaryOp op
  indent
  newLine
  visitExpression val
  dedent
visitExpression (ExprCall _ name args) = do
  emit "ExprCall"
  indent
  emitBlock "Target:" (newLine >> visitExpression name)
  emitBlock "Arguments:" (mapM_ (\a -> newLine >> visitExpression a) args)
  dedent
visitExpression (ExprStructInit _ name fields) = do
  emit $ "ExprStructInit " <> name
  emitBlock "Fields:" (mapM_ emitInitField fields)
  where
    emitInitField (n, e) = do
      newLine
      emit $ n <> ":"
      emitBlock "" (newLine >> visitExpression e)
visitExpression (ExprAccess _ target field) = do
  emit $ "ExprAccess ." <> field
  indent
  newLine
  visitExpression target
  dedent
visitExpression (ExprIndex _ target index) = do
  emit "ExprIndex"
  indent
  emitBlock "Target:" (newLine >> visitExpression target)
  emitBlock "Index:" (newLine >> visitExpression index)
  dedent
visitExpression (ExprCast _ expr typ) = do
  emit $ "ExprCast -> " <> showType typ
  indent
  newLine
  visitExpression expr
  dedent
visitExpression (ExprLitArray _ elems) = do
  emit "ExprArrayLiteral"
  emitBlock "Elements:" (mapM_ (\e -> newLine >> visitExpression e) elems)
visitExpression (ExprLitInt _ i) = emit $ "ExprLitInt " <> show i
visitExpression (ExprLitFloat _ f) = emit $ "ExprLitFloat " <> show f
visitExpression (ExprLitString _ s) = emit $ "ExprLitString " <> show s
visitExpression (ExprLitChar _ c) = emit $ "ExprLitChar " <> show c
visitExpression (ExprLitBool _ b) = emit $ "ExprLitBool " <> show b
visitExpression (ExprLitNull _) = emit "ExprLitNull"
visitExpression (ExprVar _ v) = emit $ "ExprVar " <> v
visitExpression (ExprSizeof _ val) = do
  emit "ExprSizeof"
  indent
  newLine
  case val of
    Left t  -> emit $ "Type: " <> showType t
    Right e -> visitExpression e
  dedent

--
-- private helpers
--

emit :: String -> Printer ()
emit str = modify (\s -> s {psOutput = psOutput s . showString str})

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
newLine = do
  s <- get
  let indentStr = replicate (psIndent s * 2) ' '
  modify (\s' -> s' {psOutput = psOutput s' . showString "\n" . showString indentStr})

indent :: Printer ()
indent = modify (\s -> s {psIndent = psIndent s + 1})

dedent :: Printer ()
dedent = modify (\s -> s {psIndent = psIndent s - 1})

emitParam :: Parameter -> Printer ()
emitParam p = do
  newLine
  emit (paramName p <> ": " <> showType (paramType p))
  case paramDefault p of
    Just defaultExpr -> emit (" = " <> show defaultExpr)
    Nothing -> pure ()

showType :: Type -> String
showType TypeI8 = "i8"
showType TypeI16 = "i16"
showType TypeI32 = "i32"
showType TypeI64 = "i64"
showType TypeU8 = "u8"
showType TypeU16 = "u16"
showType TypeU32 = "u32"
showType TypeU64 = "u64"
showType TypeChar = "char"
showType TypeF32 = "f32"
showType TypeF64 = "f64"
showType TypeBool = "bool"
showType TypeString = "string"
showType TypeAny = "any"
showType TypeNull = "null"
showType (TypeCustom s) = s
showType (TypeArray t) = "[" <> showType t <> "]"
showType (TypePtr t) = "*" <> showType t
showType (TypeRef t) = "&" <> showType t
showType (TypeVariadic t) = "..." <> showType t

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
showBinaryOp BitAnd = "&"

showUnaryOp :: UnaryOp -> String
showUnaryOp Negate = "-"
showUnaryOp Not = "!"
showUnaryOp BitNot = "~"
showUnaryOp PropagateError = "?"
showUnaryOp PrefixInc = "++(prefix)"
showUnaryOp PrefixDec = "--(prefix)"
showUnaryOp PostfixInc = "(postfix)++"
showUnaryOp PostfixDec = "(postfix)--"
