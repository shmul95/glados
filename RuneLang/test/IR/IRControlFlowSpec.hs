module IR.IRControlFlowSpec (irControlFlowTests) where

import Control.Monad.State (runState, modify)
import Data.Map (empty)
import qualified Data.Set as Set
import Rune.AST.Nodes (Expression (..), Statement (..))
import Rune.IR.Generator.Statement.ControlFlow
  ( genIfElse,
    genIfNoElse,
    genNext,
    genStop,
  )
import Rune.IR.IRHelpers (makeLabel)
import Rune.IR.Nodes
  ( GenState (..),
    IRGen,
    IRInstruction (..),
    IRLabel (..),
    IROperand (..),
    IRType (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--
-- helpers for testing
--

initialState :: GenState
initialState =
  GenState
    { gsTempCounter = 0,
      gsLabelCounter = 0,
      gsStringCounter = 0,
      gsGlobals = [],
      gsCurrentFunc = Nothing,
      gsSymTable = empty,
      gsStructs = empty,
      gsLoopStack = [],
      gsCalledFuncs = Set.empty
    }

runIRGen :: IRGen a -> (a, GenState)
runIRGen m = runState m initialState

setLoopContext :: Maybe (IRLabel, IRLabel) -> IRGen ()
setLoopContext Nothing = modify $ \s -> s {gsLoopStack = []}
setLoopContext (Just ctx) = modify $ \s -> s {gsLoopStack = [ctx]}

mockGenExpr :: Expression -> IRGen ([IRInstruction], IROperand, IRType)
mockGenExpr _ = pure ([IRALLOC "cond_var" IRI32], IRTemp "cond_var" IRI32, IRI32)

mockGenBlock :: [Statement] -> IRGen [IRInstruction]
mockGenBlock _ = pure [IRINC (IRTemp "i" IRI32)]

mockBlockEndingWithRet :: [Statement] -> IRGen [IRInstruction]
mockBlockEndingWithRet _ = pure [IRINC (IRTemp "i" IRI32), IRRET Nothing]

--
-- public
--

irControlFlowTests :: TestTree
irControlFlowTests =
  testGroup
    "Rune.IR.Generator.Statement.ControlFlow Specs"
    [ testCase "genIfNoElse" testGenIfNoElse,
      testCase "genIfElse with jump to end" testGenIfElseWithJump,
      testCase "genIfElse without jump to end (then-block returns)" testGenIfElseWithoutJump,
      testCase "genStop inside loop" testGenStopInLoop,
      testCase "genStop outside loop" testGenStopOutsideLoop,
      testCase "genNext inside loop" testGenNextInLoop,
      testCase "genNext outside loop" testGenNextOutsideLoop
    ]

--
-- private
--

testGenIfNoElse :: IO ()
testGenIfNoElse =
  let cond = ExprLitBool True
      thenBlock = [StmtExpr (ExprLitInt 1)]
      (result, finalState) = runIRGen $ genIfNoElse mockGenExpr mockGenBlock cond thenBlock
      condOp = IRTemp "cond_var" IRI32
      endLbl = makeLabel "end" 0

      expectedInstrs =
        mconcat
          [ [IRALLOC "cond_var" IRI32],
            [IRJUMP_FALSE condOp endLbl],
            [IRINC (IRTemp "i" IRI32)],
            [IRLABEL endLbl]
          ]
   in do
        result @?= expectedInstrs
        gsLabelCounter finalState @?= 1

testGenIfElseWithJump :: IO ()
testGenIfElseWithJump =
  let cond = ExprLitBool True
      thenBlock = [StmtExpr (ExprLitInt 1)]
      elseBlock = [StmtExpr (ExprLitInt 2)]
      (result, finalState) = runIRGen $ genIfElse mockGenExpr mockGenBlock cond thenBlock elseBlock
      condOp = IRTemp "cond_var" IRI32
      elseLbl = makeLabel "else" 0
      endLbl = makeLabel "end" 0

      expectedInstrs =
        mconcat
          [ [IRALLOC "cond_var" IRI32],
            [IRJUMP_FALSE condOp elseLbl],
            [IRINC (IRTemp "i" IRI32)],
            [IRJUMP endLbl],
            [IRLABEL elseLbl],
            [IRINC (IRTemp "i" IRI32)],
            [IRLABEL endLbl]
          ]
   in do
        result @?= expectedInstrs
        gsLabelCounter finalState @?= 1

testGenIfElseWithoutJump :: IO ()
testGenIfElseWithoutJump =
  let cond = ExprLitBool True
      thenBlock = [StmtReturn Nothing]
      elseBlock = [StmtExpr (ExprLitInt 2)]
      (result, finalState) = runIRGen $ genIfElse mockGenExpr mockBlockEndingWithRet cond thenBlock elseBlock
      condOp = IRTemp "cond_var" IRI32
      elseLbl = makeLabel "else" 0
      endLbl = makeLabel "end" 0
      expectedInstrs =
        mconcat
          [ [IRALLOC "cond_var" IRI32],
            [IRJUMP_FALSE condOp elseLbl],
            [IRINC (IRTemp "i" IRI32), IRRET Nothing],
            [IRLABEL elseLbl],
            [IRINC (IRTemp "i" IRI32), IRRET Nothing],
            [IRLABEL endLbl]
          ]
   in do
        result @?= expectedInstrs
        gsLabelCounter finalState @?= 1

testGenStopInLoop :: IO ()
testGenStopInLoop =
  let header = makeLabel "loop_header" 5
      end = makeLabel "loop_end" 5
      (result, _) = runIRGen $ do
        setLoopContext (Just (header, end))
        genStop
   in result @?= [IRJUMP end]

testGenStopOutsideLoop :: IO ()
testGenStopOutsideLoop =
  let (result, _) = runIRGen $ do
        setLoopContext Nothing
        genStop
   in result @?= []

testGenNextInLoop :: IO ()
testGenNextInLoop =
  let header = makeLabel "loop_header" 5
      end = makeLabel "loop_end" 5
      (result, _) = runIRGen $ do
        setLoopContext (Just (header, end))
        genNext
   in result @?= [IRJUMP header]

testGenNextOutsideLoop :: IO ()
testGenNextOutsideLoop =
  let (result, _) = runIRGen $ do
        setLoopContext Nothing
        genNext
   in result @?= []
