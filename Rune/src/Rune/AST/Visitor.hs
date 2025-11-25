module Rune.AST.Visitor
  ( RuneVisitor (..),
  )
where

import Control.Monad (void)
import Rune.AST.Nodes

-- | ast-node-visitor pattern
class (Monad m) => RuneVisitor m where
  -- | program entry-point
  visitProgram :: Program -> m ()
  visitProgram (Program _ defs) = mapM_ visitTopLevel defs

  -- | dispatcher for top-level declarations
  visitTopLevel :: TopLevelDef -> m ()
  visitTopLevel d@(DefFunction {}) = visitFunction d
  visitTopLevel d@(DefStruct {}) = visitStruct d
  visitTopLevel d@(DefOverride {}) = visitOverride d

  -- | virtual methods
  visitFunction :: TopLevelDef -> m ()

  visitStruct :: TopLevelDef -> m ()
  visitOverride :: TopLevelDef -> m ()

  -- | block statements
  visitBlock :: Block -> m ()
  visitBlock = mapM_ visitStatement

  -- | dispatcher for statements
  visitStatement :: Statement -> m ()
  visitStatement (StmtVarDecl name typeDecl expr) = visitVarDecl name typeDecl expr
  visitStatement (StmtReturn expr) = visitReturn expr
  visitStatement (StmtIf cond thenB elseB) = visitIf cond thenB elseB
  visitStatement (StmtFor var start end body) = visitFor var start end body
  visitStatement (StmtForEach var iterable body) = visitForEach var iterable body
  visitStatement (StmtExpr expr) = void $ visitExpression expr

  -- | virtual methods for statements
  visitVarDecl :: String -> Maybe Type -> Expression -> m ()

  visitReturn :: Maybe Expression -> m ()
  visitIf :: Expression -> Block -> Maybe Block -> m ()
  visitFor :: String -> Expression -> Expression -> Block -> m ()
  visitForEach :: String -> Expression -> Block -> m ()

  -- | expressions
  visitExpression :: Expression -> m ()
  visitExpression (ExprBinary _ l r) = visitExpression l >> visitExpression r
  visitExpression (ExprUnary _ val) = visitExpression val
  visitExpression (ExprCall _ args) = mapM_ visitExpression args
  visitExpression (ExprStructInit _ fields) = mapM_ (visitExpression . snd) fields
  visitExpression (ExprAccess target _) = visitExpression target
  visitExpression _ = return ()
