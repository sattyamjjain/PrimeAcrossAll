section .data
    prompt db 'Enter a number: ', 0
    newline db 0xA, 0
    format db '%d', 0
    result db '%d is a prime number.', 0

section .bss
    number resb 4

section .text
    global _start

_start:
    ; Prompt user for input
    mov eax, 4
    mov ebx, 1
    mov ecx, prompt
    mov edx, 15
    int 0x80

    ; Read input from user
    mov eax, 3
    mov ebx, 0
    mov ecx, number
    mov edx, 4
    int 0x80

    ; Prime number checking logic here...

    ; Exit
    mov eax, 1
    xor ebx, ebx
    int 0x80
