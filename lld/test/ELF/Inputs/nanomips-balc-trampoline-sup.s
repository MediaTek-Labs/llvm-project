    .linkrelax
    .module pcrel

    .section .text, "ax", @progbits
    .align 1

    .globl sup_fun
    .ent sup_fun

sup_fun:
    balc fun

    .end sup_fun
    .size sup_fun, .-sup_fun

    .section .before_text, "ax", @progbits
    .align 1

    .globl sup_before
    .ent sup_before

sup_before:
    balc fun

    .end sup_before
    .size sup_before, .-sup_before
