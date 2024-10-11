# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu=i7200 -mattr=+pcrel %s -o %t.o
# RUN: ld.lld --emit-relocs --section-start .text=0x1000 --defsym far=0x04000000 --defsym bbeqzc_far=0x4000 %t.o -o %t
# RUN: llvm-objdump -dr %t | FileCheck %s

# CHECK: beqic
# CHECK-NEXT: R_NANOMIPS_PC11_S1 label
# CHECK: lapc
# CHECK-NEXT: R_NANOMIPS_PC_I32 far
# CHECK: bbnezc
# CHECK-NEXT: R_NANOMIPS_PC11_S1 {{.*}}skip_bc
# CHECK-NEXT: bc
# CHECK-NEXT: R_NANOMIPS_PC25_S1 bbeqzc_far


    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start: 
    beqic $a2, 2, label
    addiu $a1, $a2, 3
    lapc $a1, far
label:
    bbeqzc $a1, 2, bbeqzc_far
    addiu $a1, $a2, 3

    .end _start
    .size _start, .-_start

