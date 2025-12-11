{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.Call.ShowSpecs (showCallTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.IR.Generator.Expression.Call.Show
import Rune.IR.Nodes (IRType(..), IROperand(..), IRInstruction(..))
import Rune.AST.Nodes (Expression(..))
import IR.TestUtils (runGen)

--
-- public
--

showCallTests :: TestTree
showCallTests = testGroup "Rune.IR.Generator.Expression.Call.Show"
  [ testGenShowCall
  , testGetShowFunc
  , testGetFormatSpecifier
  , testPrepareAddr
  ]

--
-- private
--

testGenShowCall :: TestTree
testGenShowCall = testGroup "genShowCall"
  [ testCase "Routes bool to genShowBoolCall" $
      let genExpr (ExprLitBool b) = return ([], IRConstBool b, IRBool)
          genExpr _ = return ([], IRConstBool True, IRBool)
          (instrs, _, typ) = runGen (genShowCall genExpr (ExprLitBool True))
      in do
        typ @?= IRNull
        assertBool "Should have instructions" $ not $ null instrs

  , testCase "Routes char to genShowCharCall" $
      let genExpr (ExprLitChar c) = return ([], IRConstChar c, IRChar)
          genExpr _ = return ([], IRConstChar 'a', IRChar)
          (instrs, _, typ) = runGen (genShowCall genExpr (ExprLitChar 'x'))
      in do
        typ @?= IRNull
        assertBool "Should have putchar call" $ any isCall instrs

  , testCase "Routes other types to genShowPrintfCall" $
      let genExpr (ExprLitInt n) = return ([], IRConstInt n, IRI32)
          genExpr _ = return ([], IRConstInt 0, IRI32)
          (instrs, _, typ) = runGen (genShowCall genExpr (ExprLitInt 42))
      in do
        typ @?= IRNull
        assertBool "Should have printf call" $ any isCall instrs
  ]

testGetShowFunc :: TestTree
testGetShowFunc = testGroup "getShowFunc"
  [ testCase "Returns show_StructName for struct" $
      getShowFunc (IRTemp "s" (IRStruct "Point")) (IRStruct "Point") @?= "show_Point"

  , testCase "Returns show_StructName for pointer to struct" $
      getShowFunc (IRTemp "p" (IRPtr (IRStruct "Vec"))) (IRPtr (IRStruct "Vec")) @?= "show_Vec"

  , testCase "Returns printf for primitives" $
      getShowFunc (IRConstInt 42) IRI32 @?= "printf"
  ]

testGetFormatSpecifier :: TestTree
testGetFormatSpecifier = testGroup "getFormatSpecifier"
  [ testCase "Returns format for all integer types" $ do
      getFormatSpecifier (IRConstInt 0) IRI8 @?= Just "%hhd"
      getFormatSpecifier (IRConstInt 0) IRI16 @?= Just "%hd"
      getFormatSpecifier (IRConstInt 0) IRI32 @?= Just "%d"
      getFormatSpecifier (IRConstInt 0) IRI64 @?= Just "%ld"

  , testCase "Returns format for unsigned types" $ do
      getFormatSpecifier (IRConstInt 0) IRU8 @?= Just "%hhu"
      getFormatSpecifier (IRConstInt 0) IRU16 @?= Just "%hu"
      getFormatSpecifier (IRConstInt 0) IRU32 @?= Just "%u"
      getFormatSpecifier (IRConstInt 0) IRU64 @?= Just "%lu"

  , testCase "Returns format for float types" $ do
      getFormatSpecifier (IRConstFloat 0.0) IRF32 @?= Just "%f"
      getFormatSpecifier (IRConstFloat 0.0) IRF64 @?= Just "%lf"

  , testCase "Returns format for char and string" $ do
      getFormatSpecifier (IRConstChar 'a') IRChar @?= Just "%c"
      getFormatSpecifier (IRGlobal "s" (IRPtr IRChar)) (IRPtr IRChar) @?= Just "%s"

  , testCase "Returns format for null" $
      getFormatSpecifier IRConstNull IRNull @?= Just "(null)"

  , testCase "Returns Nothing for unknown types" $
      getFormatSpecifier (IRConstInt 0) (IRStruct "Unknown") @?= Nothing
  ]

testPrepareAddr :: TestTree
testPrepareAddr = testGroup "prepareAddr"
  [ testCase "Adds IRADDR for struct temp" $
      let (instrs, op) = prepareAddr (IRTemp "s" (IRStruct "Point")) (IRStruct "Point")
      in do
        assertBool "Should have IRADDR" $ not $ null instrs
        case op of
          IRTemp name (IRPtr (IRStruct "Point")) -> assertBool "Name prefixed" $ "addr_" `elem` [take 5 name]
          _ -> assertBool "Expected addr temp" False

  , testCase "Adds IRADDR for pointer to struct" $
      let (instrs, op) = prepareAddr (IRTemp "p" (IRPtr (IRStruct "Vec"))) (IRPtr (IRStruct "Vec"))
      in do
        assertBool "Should have IRADDR" $ not $ null instrs
        case op of
          IRTemp _ (IRPtr (IRPtr (IRStruct "Vec"))) -> return ()
          _ -> assertBool "Expected double pointer" False

  , testCase "Passes through non-struct operands" $
      let (instrs, op) = prepareAddr (IRConstInt 42) IRI32
      in do
        instrs @?= []
        op @?= IRConstInt 42
  ]

--
-- helpers
--

isCall :: IRInstruction -> Bool
isCall (IRCALL _ _ _ _) = True
isCall _ = False
