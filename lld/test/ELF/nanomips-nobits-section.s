# REQUIRES: nanomips

# Note that sections first and third are intentionally put as "ax",@progbits
# and lld will issue a warning about this. This happens in some nanoMIPS
# linker scripts.

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu=i7200 %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-nobits-section.ld %t.o -o %t
# RUN: llvm-objdump -h --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s
# RUN: llvm-objdump -p --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s --check-prefix=CHECK-PHDR

# CHECK: .text 00000004 00001000 00001000
# CHECK-NEXT: .first 00000004 00002000 00001004
# CHECK-NEXT: .second 00000004 00002004 00001004
# CHECK-NEXT: .third 00000004 00002008 00001008

# CHECK-PHDR: vaddr 0x00002000
# CHECK-PHDR-NEXT: filesz 0x00000000 memsz 0x00000004
# CHECK-PHDR: vaddr 0x00002008
# CHECK-PHDR-NEXT: filesz 0x00000000 memsz 0x00000004


    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    beqic $a1, 1, _start

    .end _start
    .size _start, .-_start

    .section .first, "ax", @progbits
    .align 1
    .globl a

a:
    .4byte 0
    .size a, 4

    .section .second, "ax", @progbits
    .align 1
    .globl second_fun
    .ent second_fun

second_fun:
    beqic $a1, 2, second_fun
    
    .end second_fun
    .size second_fun, .-second_fun

    .section .third, "ax", @progbits
    .align 1
    .globl b
b:
    .4byte 0
    .size b, 4