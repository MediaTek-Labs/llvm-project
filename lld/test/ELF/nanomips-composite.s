# REQUIRES: nanomips
# RUN: llvm-mc -triple nanomips-elf -filetype=obj -mcpu i7200 %s -o %t.o
# RUN: ld.lld %t.o -o %t
# RUN: llvm-objdump -s --section=.sdata --triple=nanomips-elf \
# RUN: --mcpu=i7200 %t | FileCheck %s
# RUN: llvm-objdump -s --section=.debug_dummy --triple=nanomips-elf \
# RUN: --mcpu=i7200 %t | FileCheck --check-prefix=CHECK-DEBUG %s

# CHECK: fdffffff 0400
# CHECK-DEBUG: 01

    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    addiu $a1, $a2, 1
_end_start:
    .end _start
    .size _start, .-_start

    .section .sdata, "aw", @progbits
    .align 1

    .4byte (_start - (_end_start + 2)) >> 1
    .2byte _end_start - _start

    # Testing in the debug sections as well
    .section .debug_dummy, "", @progbits
    .byte (_end_start - (_start + 2)) >> 1