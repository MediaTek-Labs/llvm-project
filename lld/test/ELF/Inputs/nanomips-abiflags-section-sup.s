    .section .text, "ax", @progbits
    .align 1
    .globl fun
    .ent fun

fun:
    balc _start

    .end fun
    .size fun, .-fun