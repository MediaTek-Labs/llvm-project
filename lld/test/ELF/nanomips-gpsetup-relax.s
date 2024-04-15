# REQUIRES: nanomips
# Test in progress
# XFAIL: *
# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpcrel %s -o %t.o
# RUN: ld.lld %t.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s

    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    balc fun

    .end _start
    .size _start, .-_start

    .section .fun_sec, "ax", @progbits
    .align 1
    .globl fun
    .ent fun

    .reloc fun, R_NANOMIPS_SAVERESTORE, fun
fun:
    save 32, $ra,$s0-$s1,$gp
    addiu $a1, $a1, 1
    restore.jrc 32, $ra, $s0-$s1, $gp
    .end fun
    .size fun, .-fun
