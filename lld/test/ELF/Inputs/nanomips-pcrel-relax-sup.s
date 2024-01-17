    .linkrelax
    .section .text, "ax", @progbits
    .globl y
    .ent y

y:
    beqc $a2, $a3, _start

    .end y
    .size y, .-y


