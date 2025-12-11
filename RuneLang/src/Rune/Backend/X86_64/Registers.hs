{-# OPTIONS_GHC -cpp #-}

#if defined(TESTING_EXPORT)
module Rune.Backend.X86_64.Registers
  ( x86_64Registers,
    x86_64ArgsRegisters,
    x86_64CallerSavedRegisters,
    x86_64CalleeSavedRegisters,
    getRegisterName,
    getSizeSpecifier,
    getMovType,
  )
where
#else
module Rune.Backend.X86_64.Registers
  ( x86_64Registers,
    x86_64ArgsRegisters,
    x86_64CallerSavedRegisters,
    x86_64CalleeSavedRegisters,
    getRegisterName,
    getSizeSpecifier,
    getMovType,
  )
where
#endif

import Rune.IR.IRHelpers (sizeOfIRType)
import Rune.IR.Nodes (IRType (IRPtr))

--
-- public
--

x86_64Registers :: [String]
x86_64Registers =
  [ "rax",
    "rbx",
    "rcx",
    "rdx",
    "rsi",
    "rdi",
    "rbp",
    "rsp",
    "r8",
    "r9",
    "r10",
    "r11",
    "r12",
    "r13",
    "r14",
    "r15"
  ]

x86_64ArgsRegisters :: [String]
x86_64ArgsRegisters =
  [ "rdi",
    "rsi",
    "rdx",
    "rcx",
    "r8",
    "r9"
  ]

x86_64CallerSavedRegisters :: [String]
x86_64CallerSavedRegisters =
  [ "rax",
    "rcx",
    "rdx",
    "rsi",
    "rdi",
    "r8",
    "r9",
    "r10",
    "r11"
  ]

x86_64CalleeSavedRegisters :: [String]
x86_64CalleeSavedRegisters =
  [ "rbx",
    "rbp",
    "r12",
    "r13",
    "r14",
    "r15"
  ]

-- | get mov instruction based on type size
getMovType :: IRType -> String
getMovType typ =
  let size = sizeOfIRType typ
   in case sizeOfIRType typ of
        1 -> "movzx rax, byte"
        2 -> "movzx rax, word"
        4 -> "mov eax, dword"
        8 -> "mov rax, qword"
        _ -> error $ "Unsupported size for DEREF: " ++ show size

-- | get size specifier for x86_64 instructions (byte, word, dword, qword)
-- Pointers are always qword (8 bytes) regardless of what they point to
getSizeSpecifier :: IRType -> String
getSizeSpecifier (IRPtr _) = "qword"
getSizeSpecifier t = case sizeOfIRType t of
  1 -> "byte"
  2 -> "word"
  4 -> "dword"
  8 -> "qword"
  _ -> "qword"

-- | get register name based on size (al, ax, eax, rax for accumulator)
-- Pointers always use full 64-bit registers
getRegisterName :: String -> IRType -> String
getRegisterName baseReg (IRPtr _) = baseReg
getRegisterName baseReg t =
  let size = sizeOfIRType t
   in case (baseReg, size) of
        ("rax", 1) -> "al"
        ("rax", 2) -> "ax"
        ("rax", 4) -> "eax"
        ("rax", 8) -> "rax"
        ("rbx", 1) -> "bl"
        ("rbx", 2) -> "bx"
        ("rbx", 4) -> "ebx"
        ("rbx", 8) -> "rbx"
        ("rdi", 1) -> "dil"
        ("rdi", 2) -> "di"
        ("rdi", 4) -> "edi"
        ("rdi", 8) -> "rdi"
        ("rsi", 1) -> "sil"
        ("rsi", 2) -> "si"
        ("rsi", 4) -> "esi"
        ("rsi", 8) -> "rsi"
        ("rdx", 1) -> "dl"
        ("rdx", 2) -> "dx"
        ("rdx", 4) -> "edx"
        ("rdx", 8) -> "rdx"
        ("rcx", 1) -> "cl"
        ("rcx", 2) -> "cx"
        ("rcx", 4) -> "ecx"
        ("rcx", 8) -> "rcx"
        ("r8", 1) -> "r8b"
        ("r8", 2) -> "r8w"
        ("r8", 4) -> "r8d"
        ("r8", 8) -> "r8"
        ("r9", 1) -> "r9b"
        ("r9", 2) -> "r9w"
        ("r9", 4) -> "r9d"
        ("r9", 8) -> "r9"
        _ -> baseReg
