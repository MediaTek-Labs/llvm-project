# REQUIRES: nanomips
# TODO: testing align fill, max size and fill size

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 -mattr=+pcrel \
# RUN: %s -o %t.o

# RUN: ld.lld --section-start .text=0x1000 --section-start \
# RUN: .balc_fun_sec=0x1100 --section-start .lapc_fun_sec=0x20000000 %t.o -o %t

# RUN: llvm-objdump -d %t | FileCheck %s

# CHECK: lapc
# CHECK-NEXT: 08 90 nop
# CHECK-NEXT: 08 90 nop

# CHECK: balc
# CHECK-NEXT: 08 90 nop

    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    addiu $a1, $a2, 1
    li $a1, 1
    
    # lapc gets expanded so the alignment is changed a 32 bit nop should be cut
    # down
    lapc $a0, lapc_fun
    .align 3
    
    # balc gets relaxed so one 2-byte nop should be added
    balc balc_fun
    .align 2
    
    li $a1, 1
    .end _start
    .size _start, .-_start

    .section .balc_fun_sec, "ax", @progbits
    .align 1
    .globl balc_fun
    .ent balc_fun

balc_fun:
    addiu $a1, $a1, 3
    .end balc_fun
    .size balc_fun, .-balc_fun

    .section .lapc_fun_sec, "ax", @progbits
    .align 1
    .globl lapc_fun
    .ent lapc_fun

lapc_fun:
    addiu $a1, $a2, 2
    .end lapc_fun
    .size lapc_fun, .-lapc_fun

