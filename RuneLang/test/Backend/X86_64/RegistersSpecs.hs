{-# LANGUAGE CPP #-}
#define TESTING_EXPORT

module Backend.X86_64.RegistersSpecs (registersTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Rune.Backend.X86_64.Registers
import Rune.IR.Nodes (IRType(..))

--
-- public
--

registersTests :: TestTree
registersTests = testGroup "Rune.Backend.X86_64.Registers"
  [ testX86_64Registers
  , testX86_64ArgsRegisters
  , testX86_64CallerSavedRegisters
  , testX86_64CalleeSavedRegisters
  , testGetRegisterName
  , testGetSizeSpecifier
  , testGetMovType
  ]

--
-- private
--

testX86_64Registers :: TestTree
testX86_64Registers = testGroup "x86_64Registers"
  [ testCase "Contains all 16 registers" $
      length x86_64Registers @?= 16

  , testCase "Contains rax" $
      assertBool "Should contain rax" $ "rax" `elem` x86_64Registers

  , testCase "Contains rbp and rsp" $
      assertBool "Should contain rbp and rsp" $
        "rbp" `elem` x86_64Registers && "rsp" `elem` x86_64Registers

  , testCase "Contains r8-r15" $
      assertBool "Should contain extended registers" $
        all (`elem` x86_64Registers) ["r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
  ]

testX86_64ArgsRegisters :: TestTree
testX86_64ArgsRegisters = testGroup "x86_64ArgsRegisters"
  [ testCase "Contains 6 argument registers" $
      length x86_64ArgsRegisters @?= 6

  , testCase "Correct order" $
      x86_64ArgsRegisters @?= ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

  , testCase "First arg in rdi" $
      case x86_64ArgsRegisters of
        (first:_) -> first @?= "rdi"
        [] -> assertBool "Should not be empty" False
  ]

testX86_64CallerSavedRegisters :: TestTree
testX86_64CallerSavedRegisters = testGroup "x86_64CallerSavedRegisters"
  [ testCase "Contains caller-saved registers" $
      length x86_64CallerSavedRegisters @?= 9

  , testCase "Contains rax" $
      assertBool "Should contain rax" $ "rax" `elem` x86_64CallerSavedRegisters

  , testCase "Does not contain rbp" $
      assertBool "Should not contain rbp" $ "rbp" `notElem` x86_64CallerSavedRegisters
  ]

testX86_64CalleeSavedRegisters :: TestTree
testX86_64CalleeSavedRegisters = testGroup "x86_64CalleeSavedRegisters"
  [ testCase "Contains callee-saved registers" $
      length x86_64CalleeSavedRegisters @?= 6

  , testCase "Contains rbx and rbp" $
      assertBool "Should contain rbx and rbp" $
        "rbx" `elem` x86_64CalleeSavedRegisters && "rbp" `elem` x86_64CalleeSavedRegisters

  , testCase "Contains r12-r15" $
      assertBool "Should contain r12-r15" $
        all (`elem` x86_64CalleeSavedRegisters) ["r12", "r13", "r14", "r15"]
  ]

testGetRegisterName :: TestTree
testGetRegisterName = testGroup "getRegisterName"
  [ testCase "rax with 1 byte" $
      getRegisterName "rax" IRI8 @?= "al"

  , testCase "rax with 2 bytes" $
      getRegisterName "rax" IRI16 @?= "ax"

  , testCase "rax with 4 bytes" $
      getRegisterName "rax" IRI32 @?= "eax"

  , testCase "rax with 8 bytes" $
      getRegisterName "rax" IRI64 @?= "rax"

  , testCase "rbx with 1 byte" $
      getRegisterName "rbx" IRU8 @?= "bl"

  , testCase "rdi with different sizes" $ do
      getRegisterName "rdi" IRI8 @?= "dil"
      getRegisterName "rdi" IRI16 @?= "di"
      getRegisterName "rdi" IRI32 @?= "edi"
      getRegisterName "rdi" IRI64 @?= "rdi"

  , testCase "Pointers use full 64-bit register" $
      getRegisterName "rax" (IRPtr IRI32) @?= "rax"

  , testCase "r8 with different sizes" $ do
      getRegisterName "r8" IRI8 @?= "r8b"
      getRegisterName "r8" IRI16 @?= "r8w"
      getRegisterName "r8" IRI32 @?= "r8d"
      getRegisterName "r8" IRI64 @?= "r8"
  ]

testGetSizeSpecifier :: TestTree
testGetSizeSpecifier = testGroup "getSizeSpecifier"
  [ testCase "1-byte types" $ do
      getSizeSpecifier IRI8 @?= "byte"
      getSizeSpecifier IRU8 @?= "byte"
      getSizeSpecifier IRChar @?= "byte"
      getSizeSpecifier IRBool @?= "byte"

  , testCase "2-byte types" $ do
      getSizeSpecifier IRI16 @?= "word"
      getSizeSpecifier IRU16 @?= "word"

  , testCase "4-byte types" $ do
      getSizeSpecifier IRI32 @?= "dword"
      getSizeSpecifier IRU32 @?= "dword"
      getSizeSpecifier IRF32 @?= "dword"

  , testCase "8-byte types" $ do
      getSizeSpecifier IRI64 @?= "qword"
      getSizeSpecifier IRU64 @?= "qword"
      getSizeSpecifier IRF64 @?= "qword"

  , testCase "Pointers are qword" $
      getSizeSpecifier (IRPtr IRI8) @?= "qword"

  , testCase "Nested pointers are qword" $
      getSizeSpecifier (IRPtr (IRPtr IRI32)) @?= "qword"
  ]

testGetMovType :: TestTree
testGetMovType = testGroup "getMovType"
  [ testCase "1-byte mov" $
      getMovType IRI8 @?= "movzx rax, byte"

  , testCase "2-byte mov" $
      getMovType IRI16 @?= "movzx rax, word"

  , testCase "4-byte mov" $
      getMovType IRI32 @?= "mov eax, dword"

  , testCase "8-byte mov" $
      getMovType IRI64 @?= "mov rax, qword"

  , testCase "Pointer mov" $
      getMovType (IRPtr IRI32) @?= "mov rax, qword"
  ]
