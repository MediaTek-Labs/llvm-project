# llvm-objdump needs to be fixed doesn't recognize 2-byte beqc
# llvm-objdump addiu - immediate tried to be represented as a symbol
# llvm-mc doesn't generate good align symbols and doesn't generate
# R_NANOMIPS_FILL
# llvm-mc doesn't support adding size to the instruction (e.g. beqc32)
# but that is not that important now
# REQUIRES: nanomips, nanomips-gnu

# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpcrel %s -o %t.o
# RUN: ld.lld --section-start .text=0x1000 --section-start .fun_sec=0x1020 \
# RUN: --section-start .max_fill_sec=0x1100 --section-start .lapc_sec=0x2000000 --relax %t.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s

# CHECK: 6083{{.*}} lapc {{.*}} <lapc_fun>
# The next line is important only to skip it
# CHECK-NEXT: 1
# CHECK-NEXT: 9008 nop
# CHECK-NEXT: 9008 nop
# CHECK-NEXT: 00a6
# CHECK: dac{{.*}} beqc {{.*}} <fun>
# CHECK-NEXT: 9008 nop
# CHECK-NEXT: d282 li

# CHECK: 60e3{{.*}} lapc {{.*}} <lapc_fun>
# The next line is important only to skip it
# CHECK-NEXT: 12
# CHECK-NEXT: 0101 0101
# CHECK-NEXT: 0101 0101
# CHECK-NEXT: 0101 0101
# CHECK-EMPTY:

    
    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    addiu $a1, $a2, 1
    li $a1, 1
    # lapc gets expanded so the alignment
    # is changed a 32 bit nop should be cut down
    lapc $a0, lapc_fun
    .align 3
    addiu $a1, $a2, 1
    # beqc gets relaxed so one 2-byte nop
    # should be added (llvm-mc generates 2-byte immediately, should change then)
    beqc $a0, $a1, fun
    .align 2
    li $a1, 2
    .end _start
    .size _start, .-_start

    .section .fun_sec, "ax", @progbits
    .align 1
    .globl fun
    .ent fun

fun:
    addiu $a1, $a1, 3
    .end fun
    .size fun, .-fun

    .section .max_fill_sec, "ax", @progbits
    .align 1
    .globl max_fill_fun
    .ent max_fill_fun

max_fill_fun:
    .skip 0x8
    # Here lapcs get expanded
    # and we need to align
    # afterwards, 12 more bytes
    lapc $a2, lapc_fun
    lapc $a3, lapc_fun
    # Don't know how to specify max
    # and fill size as well
    .align 4, 1

    .end max_fill_fun
    .size max_fill_fun, .-max_fill_fun


    .section .lapc_sec, "ax", @progbits
    .align 1
    .globl lapc_fun
    .ent lapc_fun


lapc_fun:
    addiu $a2, $a3, 1
    .end lapc_fun
    .size lapc_fun, .-lapc_fun
