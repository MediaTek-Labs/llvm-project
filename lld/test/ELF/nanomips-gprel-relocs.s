# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -EL -march=32r6 -m32 %s -o %t.o
# RUN: ld.lld --script=%S/Inputs/nanomips-gprel-relocs.ld %t.o -o %t
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -d %t | FileCheck %s 

# CHECK: 1000: 4080 0006 lw a0,4(gp)
# CHECK: 1004: 44b0 0004 lh a1,4(gp)
# CHECK: 1008: 60a1 0008 addiu a1,a1,8
# CHECK: 100c: 0000
# CHECK: 100e: e0c0 1000 lui a2,%hi(0x1000)
# CHECK: 1012: 44e8 0008 lbu a3,8(gp)

    .section .text, "ax", @progbits
    .align 4
    .globl _start
    .ent _start

_start:
    lw $a0, %gprel(a)($gp)
#   sw $a0, %gprel(b)($gp)
    lh $a1, %gprel(a)($gp)
    addiu $a1, %gprel(b)
    lui $a2, %gprel_hi(c)
    lbu $a3, %gprel_lo(b)($gp)
#   sw $a1, %gprel(a)($gp)
    .end _start
    .size _start, .-_start

    .section .sdata, "aw", @progbits
    .align 4
    .skip 4
    .globl a
    .type a, @object
    .size a, 4
a:
    .long 1

    .globl b
    .type b, @object
    .size b, 2
b:
    .2byte 3

    .skip 0x1000
    .globl c
    .type c, @object
    .size c, 4
c:
    .long 6
