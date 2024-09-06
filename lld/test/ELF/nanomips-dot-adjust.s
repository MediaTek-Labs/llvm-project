# REQUIRES: nanomips


# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu=i7200 %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-dot-adjust.ld %t.o -o %t
# RUN: llvm-objdump -h --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s

# CHECK: .text {{[0-9a-fA-F]*}} 00001000
# CHECK-NEXT: .first {{[0-9a-fA-F]*}} 00001100

    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    addiu $a1, $a2, 1

    .end _start
    .size _start, .-_start

    .section .first, "ax", @progbits
    .align 1
    .globl first_fun
    .ent first_fun

first_fun:
    addiu $a1, $a2, 2

    .end first_fun
    .size first_fun, .-first_fun
