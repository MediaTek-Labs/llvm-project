# Testing composite relocations
# llvm-mc doesn't make same relocations
# as nanomips-elf-as they are needed for this
# to work
# REQUIRES: nanomips, nanomips-gnu


# RUN: %nanomips-elf-as -m32 -EL -march=32r6 %s -o %t.o
# RUN: ld.lld --section-start .text=0x1000 --section-start .sdata=0x2000 %t.o -o %t
# RUN: %nanomips-elf-objdump -s --section=.sdata %t | FileCheck %s

# CHECK: 2000 fbffffff 0800

    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    addiu $a1, $a2, 1
    addiu $a1, $a2, 1
_end_start:
    .end _start
    .size _start, .-_start

    .section .sdata, "aw", @progbits
    .align 1

a:
    .4byte (_start - (_end_start + 2)) >> 1
    .2byte _end_start - _start
