
    .linkrelax
    .section .text, "ax", @progbits
    .align 2
    .globl _start
    .ent _start

_start:
    balc fun
    .align 2
    li $a1, 1

    .end _start
    .size _start, .-_start
