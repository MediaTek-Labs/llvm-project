# REQUIRES: nanomips


# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu=i7200 %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-align-outsec.ld %t.o -o %t
# RUN: llvm-objdump -h --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s
# RUN: llvm-objdump -p --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s --check-prefix=CHECK-PHDR

# CHECK: .first 00000004 00002020 00001020
# CHECK-NEXT: .second 00000004 00002040 00001040


# CHECK-PHDR: 0x00002020 vaddr 0x00002020 paddr 0x00001020
# CHECK-PHDR:  0x00002040 vaddr 0x00002040 paddr 0x00001040

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

    .section .second, "ax", @progbits
    .align 1
    .globl second_fun
    .ent second_fun

second_fun:
    beqic $a1, 2, second_fun
    
    .end second_fun
    .size second_fun, .-second_fun