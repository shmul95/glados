{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module IR.Generator.Expression.LiteralsSpecs (literalsTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.IR.Generator.Expression.Literals
import Rune.IR.Nodes (IRType(..), IROperand(..))
import IR.TestUtils (runGen)

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
      let (instrs, op, typ) = runGen (genLitInt 42)
      in do
        instrs @?= []
        op @?= IRConstInt 42
        typ @?= IRI32

  , testCase "Handles negative int" $
      let (_, op, _) = runGen (genLitInt (-10))
      in op @?= IRConstInt (-10)
  ]

testGenLitFloat :: TestTree
testGenLitFloat = testGroup "genLitFloat"
  [ testCase "Generates float constant" $
      let (instrs, op, typ) = runGen (genLitFloat 3.14)
      in do
        instrs @?= []
        op @?= IRConstFloat 3.14
        typ @?= IRF32
  ]

testGenLitChar :: TestTree
testGenLitChar = testGroup "genLitChar"
  [ testCase "Generates char constant" $
      let (instrs, op, typ) = runGen (genLitChar 'a')
      in do
        instrs @?= []
        op @?= IRConstChar 'a'
        typ @?= IRChar
  ]

testGenLitBool :: TestTree
testGenLitBool = testGroup "genLitBool"
  [ testCase "Generates bool true" $
      let (instrs, op, typ) = runGen (genLitBool True)
      in do
        instrs @?= []
        op @?= IRConstBool True
        typ @?= IRBool

  , testCase "Generates bool false" $
      let (_, op, _) = runGen (genLitBool False)
      in op @?= IRConstBool False
  ]

testGenLitNull :: TestTree
testGenLitNull = testGroup "genLitNull"
  [ testCase "Generates null constant" $
      let (instrs, op, typ) = runGen genLitNull
      in do
        instrs @?= []
        op @?= IRConstNull
        typ @?= IRNull
  ]

testGenLitString :: TestTree
testGenLitString = testGroup "genLitString"
  [ testCase "Generates global string" $
      let (_, op, typ) = runGen (genLitString "hello")
      in do
        case op of
          IRGlobal _ (IRPtr IRChar) -> return ()
          _ -> assertBool "Expected IRGlobal with IRPtr IRChar" False
        typ @?= IRPtr IRChar

  , testCase "Handles empty string" $
      let (_, _, typ) = runGen (genLitString "")
      in typ @?= IRPtr IRChar
  ]
