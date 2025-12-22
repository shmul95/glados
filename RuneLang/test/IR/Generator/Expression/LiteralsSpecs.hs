{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.LiteralsSpecs (literalsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.IR.Generator.Expression.Literals
import Rune.IR.Nodes (IRType(..), IROperand(..))
import IR.TestUtils (runGenUnsafe)

--
-- public
--

literalsTests :: TestTree
literalsTests = testGroup "Rune.IR.Generator.Expression.Literals"
  [ testGenLitInt
  , testGenLitFloat
  , testGenLitChar
  , testGenLitBool
  , testGenLitNull
  , testGenLitString
  ]

--
-- private
--

testGenLitInt :: TestTree
testGenLitInt = testGroup "genLitInt"
  [ testCase "Generates int constant" $
      let (instrs, op, typ) = runGenUnsafe (genLitInt 42)
      in do
        instrs @?= []
        op @?= IRConstInt 42
        typ @?= IRI32

  , testCase "Handles negative int" $
      let (_, op, _) = runGenUnsafe (genLitInt (-10))
      in op @?= IRConstInt (-10)
  ]

testGenLitFloat :: TestTree
testGenLitFloat = testGroup "genLitFloat"
  [ testCase "Generates float constant" $
      let (instrs, op, typ) = runGenUnsafe (genLitFloat 3.14)
      in do
        instrs @?= []
        op @?= IRGlobal "f32_global0" IRF32
        typ @?= IRF32
  ]

testGenLitChar :: TestTree
testGenLitChar = testGroup "genLitChar"
  [ testCase "Generates char constant" $
      let (instrs, op, typ) = runGenUnsafe (genLitChar 'a')
      in do
        instrs @?= []
        op @?= IRConstChar 'a'
        typ @?= IRChar
  ]

testGenLitBool :: TestTree
testGenLitBool = testGroup "genLitBool"
  [ testCase "Generates bool true" $
      let (instrs, op, typ) = runGenUnsafe (genLitBool True)
      in do
        instrs @?= []
        op @?= IRConstBool True
        typ @?= IRBool

  , testCase "Generates bool false" $
      let (_, op, _) = runGenUnsafe (genLitBool False)
      in op @?= IRConstBool False
  ]

testGenLitNull :: TestTree
testGenLitNull = testGroup "genLitNull"
  [ testCase "Generates null constant" $
      let (instrs, op, typ) = runGenUnsafe genLitNull
      in do
        instrs @?= []
        op @?= IRConstNull
        typ @?= IRNull
  ]

testGenLitString :: TestTree
testGenLitString = testGroup "genLitString"
  [ testCase "Generates global string" $
      let (_, op, typ) = runGenUnsafe (genLitString "hello")
      in do
        case op of
          IRGlobal _ (IRPtr IRChar) -> return ()
          _ -> assertBool "Expected IRGlobal with IRPtr IRChar" False
        typ @?= IRPtr IRChar

  , testCase "Handles empty string" $
      let (_, _, typ) = runGenUnsafe (genLitString "")
      in typ @?= IRPtr IRChar
  ]
