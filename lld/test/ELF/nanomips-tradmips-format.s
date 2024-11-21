# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 %s -o %t.o

# RUN: ld.lld -T %S/Inputs/nanomips-tradmips-format.ld %t.o -o %t

# RUN: llvm-objdump -f %t | FileCheck %s

# CHECK: elf32-nanomips

    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    addiu $a1, $a2, 3

    .end _start
    .size _start, .-_start