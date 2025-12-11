extern printf
extern putchar
section .data
str_show_float0 db "%f", 0
section .rodata
main_float1: dd 23.0
main_float0: dd 42.0
section .text
global main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    movss xmm0, dword [rel main_float0]
    movss dword [rbp-12], xmm0
    movss xmm0, dword [rbp-12]
    call show_float
    mov dword [rbp-8], eax
    movss xmm0, dword [rel main_float1]
    call show_float
    mov dword [rbp-4], eax
    mov rax, 0
    jmp .L.function_end_main
.L.function_end_main:
    mov rsp, rbp
    pop rbp
    ret

global show_float
show_float:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    movss dword [rbp-4], xmm0
    mov rdi, str_show_float0
    movss xmm0, dword [rbp-4]
    cvtss2sd xmm0, xmm0
    call printf
    mov rdi, 10
    call putchar
    xor rax, rax
    jmp .L.function_end_show_float
.L.function_end_show_float:
    mov rsp, rbp
    pop rbp
    ret

