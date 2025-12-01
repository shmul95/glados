module Rune.Backend.X86_64.Registers
  ( x86_64Registers,
    x86_64ArgsRegisters,
    x86_64CallerSavedRegisters,
    x86_64CalleeSavedRegisters,
  )
where

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
