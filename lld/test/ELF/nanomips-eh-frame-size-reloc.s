# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu=i7200 %s -o %t.o
# RUN: ld.lld %t.o -o %t
# RUN: llvm-objdump -h %t | FileCheck %s

# CHECK: .eh_frame 0000002c

    .linkrelax
    .cfi_sections .eh_frame
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    .cfi_startproc
    nop
    .cfi_endproc
    .end _start
    .size _start, .-_start

