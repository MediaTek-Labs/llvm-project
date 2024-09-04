    
    .section .text, "ax", @progbits
    .align 1
    .globl fun
    .ent fun

fun:
    bc _start

    .end fun
    .size fun, .-fun
