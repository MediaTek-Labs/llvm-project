    .linkrelax
    .section .text, "ax", @progbits
    .globl y
    .ent y

y:
    beqc $a2, $a3, _start
    addiu $a1, $a2, 1
    .end y
    .size y, .-y


