# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -m32 -EL -march=32r6 %s -o %t.o
# march=nms1 for non full nanoMIPS ISA
# RUN: ld.lld --relax %t.o -o %t
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -d %t | FileCheck %s

# CHECK: 8965{{.*}} bgec {{.*}} <fun>
# CHECK: dbe{{.*}} beqc {{.*}} <fun>
# CHECK: dbe{{.*}} beqc {{.*}} <fun>
# CHECK: 8905{{.*}} bgec {{.*}} <fun>

# Starting testcase of nanomips relaxations, needs to convert
# 32bit beqc to 16 bit beqc
# And for expansions 32 bit lapc to 48 bit lapc
    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    lapc $a3, fun
    bgec $a1, $a7, fun
    beqc $a2, $a3, fun
a:
    bnec $a1, $a2, fun
    bgec $a1, $a4, fun
    add $a1, $a1, $a1
    add $a2, $a3, $a1
    .end _start
    .size _start, .-_start

    .section .other_text, "ax", @progbits
    .align 1
    .globl fun
    .ent fun
fun:
    addiu $a3, $a2, 1
    beqc $zero, $a2, a
    bnec $t0, $a2, x
    bnec $zero, $t0, x
    beqc $a2, $a2, x
    bnec $a2, $a2, x 

    .end fun
    .size fun, .-fun

    .globl x
    .ent x
x:
    addiu $a2, $a2, 2
    bnec $zero, $a3, x2
    beqc $a2, $a3, x1
    .skip 0x20
x1:
    .skip 0x5c
x2:
    .end x
    .size x, .-x
