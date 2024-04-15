
    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl fun
    .ent fun

fun:
    jrc $r31

    .end fun
    .size fun, .-fun

