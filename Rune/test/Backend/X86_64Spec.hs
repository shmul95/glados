module Backend.X86_64Spec (x86_64Tests) where

import Rune.AST.Parser (parseRune)
import Rune.Backend.X86_64.Codegen (emitAssembly)
import Rune.IR.Generator (generateIR)
import Rune.Lexer.Lexer (lexer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Megaparsec (errorBundlePretty)

--
-- public
--

x86_64Tests :: TestTree
x86_64Tests =
  testGroup
    "Rune.Backend.x86_64Spec"
    [ testCase "x86_64 Test Programs" x86_64TestShowString,
      testCase "x86_64 If-Else Test" x86_64TestIfElse
    ]

--
-- private
--

runX86_64 :: String -> String
runX86_64 source =
  case lexer "test" source of
    Left err -> error $ errorBundlePretty err
    Right tokens -> case parseRune "test" tokens of
      Left err -> error err
      Right ast -> emitAssembly $ generateIR ast

--
-- test programs
--

x86_64TestShowString :: IO ()
x86_64TestShowString = do
  let program =
        unlines
          [ "def show_string(str: string) -> null",
            "{",
            "    for c in str {",
            "        show(c);",
            "    }",
            "}",
            "",
            "def get_string() -> string",
            "{",
            "    return \"Hello, Rune!\n\";",
            "}",
            "",
            "def main() -> null",
            "{",
            "    str: string = get_string();",
            "",
            "    show_string(str);",
            "}",
            ""
          ]

      expected =
        unlines
          [ "extern putchar",
            "section .data",
            "str_get_string0 db \"Hello, Rune!\",10, 0",
            "section .text",
            "global main",
            "main:",
            "    push rbp",
            "    mov rbp, rsp",
            "    sub rsp, 16",
            "    call get_string",
            "    mov qword [rbp-8], rax",
            "    mov rdi, qword [rbp-8]",
            "    call show_string",
            "    mov qword [rbp-4], rax",
            "    jmp .L.function_end_main",
            ".L.function_end_main:",
            "    mov rsp, rbp",
            "    pop rbp",
            "    ret",
            "",
            "global get_string",
            "get_string:",
            "    push rbp",
            "    mov rbp, rsp",
            "    sub rsp, 16",
            "    mov qword rax, str_get_string0",
            "    mov qword [rbp-8], rax",
            "    mov rax, qword [rbp-8]",
            "    jmp .L.function_end_get_string",
            ".L.function_end_get_string:",
            "    mov rsp, rbp",
            "    pop rbp",
            "    ret",
            "",
            "global show_string",
            "show_string:",
            "    push rbp",
            "    mov rbp, rsp",
            "    sub rsp, 32",
            "    mov qword [rbp-8], rdi",
            "    mov rax, qword [rbp-8]",
            "    mov qword [rbp-16], rax",
            ".L.loop_header0:",
            "    mov rax, qword [rbp-16]",
            "    movzx rax, byte [rax]",
            "    mov qword [rbp-24], rax",
            ".L.loop_check0:",
            "    mov rax, qword [rbp-24]",
            "    test rax, rax",
            "    je .L.loop_end0",
            ".L.body0:",
            "    mov rdi, qword [rbp-24]",
            "    call putchar",
            "    add qword [rbp-16], 1",
            "    jmp .L.loop_header0",
            ".L.loop_end0:",
            "    jmp .L.function_end_show_string",
            ".L.function_end_show_string:",
            "    mov rsp, rbp",
            "    pop rbp",
            "    ret",
            ""
          ]

  runX86_64 program @?= expected

x86_64TestIfElse :: IO ()
x86_64TestIfElse = do
  let program =
        unlines
          [ "def main() -> i32 {",
            "    a: i32 = 5;",
            "",
            "    if a < 10 {",
            "        show(\"a is less than 10\\n\");",
            "    } else {",
            "        show(\"a is 10 or greater\\n\");",
            "    }",
            "    return a;",
            "}"
          ]

      expected =
        unlines
          [ "extern printf",
            "section .data",
            "str_main1 db \"a is 10 or greater\",10, 0",
            "str_main0 db \"a is less than 10\",10, 0",
            "section .text",
            "global main",
            "main:",
            "    push rbp",
            "    mov rbp, rsp",
            "    sub rsp, 32",
            "    mov qword [rbp-28], 5",
            "    mov rax, qword [rbp-28]",
            "    mov rbx, 10",
            "    cmp rax, rbx",
            "    setl al",
            "    movzx rax, al",
            "    mov qword [rbp-4], rax",
            "    mov rax, qword [rbp-4]",
            "    test rax, rax",
            "    je .L.else0",
            "    mov qword rax, str_main0",
            "    mov qword [rbp-20], rax",
            "    mov rdi, qword [rbp-20]",
            "    call printf",
            "    jmp .L.end0",
            ".L.else0:",
            "    mov qword rax, str_main1",
            "    mov qword [rbp-12], rax",
            "    mov rdi, qword [rbp-12]",
            "    call printf",
            ".L.end0:",
            "    mov rax, qword [rbp-28]",
            "    jmp .L.function_end_main",
            ".L.function_end_main:",
            "    mov rsp, rbp",
            "    pop rbp",
            "    ret",
            ""
          ]
  runX86_64 program @?= expected
