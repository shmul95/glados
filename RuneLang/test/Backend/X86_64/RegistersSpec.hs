module Backend.X86_64.RegistersSpec (registersTests) where

import Rune.Backend.X86_64.Registers
import Rune.IR.Nodes (IRType (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

registersTests :: TestTree
registersTests =
  testGroup
    "Rune.Backend.X86_64.Registers Specs"
    [ testX86_64Registers,
      testX86_64ArgsRegisters,
      testX86_64CallerSaved,
      testX86_64CalleeSaved,
      testGetRegisterName,
      testGetSizeSpecifier,
      testGetMovType
    ]

testX86_64Registers :: TestTree
testX86_64Registers =
  testCase "x86_64Registers list" $
    do
      length x86_64Registers @?= 16
      case x86_64Registers of
        (first : _) -> first @?= "rax"
        [] -> error "Expected non-empty register list"
      last x86_64Registers @?= "r15"

testX86_64ArgsRegisters :: TestTree
testX86_64ArgsRegisters =
  testCase "x86_64ArgsRegisters list" $
    do
      length x86_64ArgsRegisters @?= 6
      x86_64ArgsRegisters @?= ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

testX86_64CallerSaved :: TestTree
testX86_64CallerSaved =
  testCase "x86_64CallerSavedRegisters list" $
    do
      length x86_64CallerSavedRegisters @?= 9
      "rax" `elem` x86_64CallerSavedRegisters @?= True
      "r11" `elem` x86_64CallerSavedRegisters @?= True

testX86_64CalleeSaved :: TestTree
testX86_64CalleeSaved =
  testCase "x86_64CalleeSavedRegisters list" $
    do
      length x86_64CalleeSavedRegisters @?= 6
      "rbx" `elem` x86_64CalleeSavedRegisters @?= True
      "rbp" `elem` x86_64CalleeSavedRegisters @?= True

testGetRegisterName :: TestTree
testGetRegisterName =
  testGroup
    "getRegisterName"
    [ testCase "rax with different sizes" $
        do
          getRegisterName "rax" IRI8 @?= "al"
          getRegisterName "rax" IRI16 @?= "ax"
          getRegisterName "rax" IRI32 @?= "eax"
          getRegisterName "rax" IRI64 @?= "rax",
      testCase "rbx with different sizes" $
        do
          getRegisterName "rbx" IRI8 @?= "bl"
          getRegisterName "rbx" IRI16 @?= "bx"
          getRegisterName "rbx" IRI32 @?= "ebx"
          getRegisterName "rbx" IRI64 @?= "rbx",
      testCase "rdi with different sizes" $
        do
          getRegisterName "rdi" IRI8 @?= "dil"
          getRegisterName "rdi" IRI16 @?= "di"
          getRegisterName "rdi" IRI32 @?= "edi"
          getRegisterName "rdi" IRI64 @?= "rdi",
      testCase "r8 with different sizes" $
        do
          getRegisterName "r8" IRI8 @?= "r8b"
          getRegisterName "r8" IRI16 @?= "r8w"
          getRegisterName "r8" IRI32 @?= "r8d"
          getRegisterName "r8" IRI64 @?= "r8",
      testCase "pointer types always use full register" $
        do
          getRegisterName "rax" (IRPtr IRI32) @?= "rax"
          getRegisterName "rbx" (IRPtr IRI8) @?= "rbx",
      testCase "unsigned types" $
        do
          getRegisterName "rax" IRU8 @?= "al"
          getRegisterName "rax" IRU32 @?= "eax"
          getRegisterName "rax" IRU64 @?= "rax",
      testCase "char type" $
        getRegisterName "rax" IRChar @?= "al",
      testCase "bool type" $
        getRegisterName "rax" IRBool @?= "al"
    ]

testGetSizeSpecifier :: TestTree
testGetSizeSpecifier =
  testGroup
    "getSizeSpecifier"
    [ testCase "8-bit types" $
        do
          getSizeSpecifier IRI8 @?= "byte"
          getSizeSpecifier IRU8 @?= "byte"
          getSizeSpecifier IRChar @?= "byte"
          getSizeSpecifier IRBool @?= "byte",
      testCase "16-bit types" $
        do
          getSizeSpecifier IRI16 @?= "word"
          getSizeSpecifier IRU16 @?= "word",
      testCase "32-bit types" $
        do
          getSizeSpecifier IRI32 @?= "dword"
          getSizeSpecifier IRU32 @?= "dword"
          getSizeSpecifier IRF32 @?= "dword",
      testCase "64-bit types" $
        do
          getSizeSpecifier IRI64 @?= "qword"
          getSizeSpecifier IRU64 @?= "qword"
          getSizeSpecifier IRF64 @?= "qword",
      testCase "pointer types always qword" $
        do
          getSizeSpecifier (IRPtr IRI8) @?= "qword"
          getSizeSpecifier (IRPtr IRI32) @?= "qword"
          getSizeSpecifier (IRPtr IRChar) @?= "qword",
      testCase "struct type" $
        getSizeSpecifier (IRStruct "Vec2") @?= "qword"
    ]

testGetMovType :: TestTree
testGetMovType =
  testGroup
    "getMovType"
    [ testCase "1-byte type" $
        getMovType IRI8 @?= "movzx rax, byte",
      testCase "2-byte type" $
        getMovType IRI16 @?= "movzx rax, word",
      testCase "4-byte type" $
        getMovType IRI32 @?= "mov eax, dword",
      testCase "8-byte type" $
        getMovType IRI64 @?= "mov rax, qword"
    ]
