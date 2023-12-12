    .linkrelax
    .section .text, "ax", @progbits
    .align 2
    .globl fun
    .ent fun

fun:
    li $a2, 1

    .end fun
    .size fun, .-fun
