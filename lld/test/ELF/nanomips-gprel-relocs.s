# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-gprel-relocs.ld %t.o -o %t
# RUN: llvm-objdump -d --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s

# CHECK: 01 56 lw $a0, 0x4($gp)
# CHECK-NEXT: b0 44 04 00 lh $a1, 0x4($gp)
# CHECK-NEXT: ac 44 04 00 addiu.b $a1, $gp, 0x4
# CHECK-NEXT: c0 40 04 00 addiu.w $a2, $gp, 0x4
# CHECK-NEXT: e2 60 04 00 00 00 addiu.b32 $a3, $gp, 0x4


    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start
# TODO: R_NANOMIPS_GPREL_LO12
# TODO: R_NANOMIPS_GPREL_HI20
# TODO: if generated somehow add R_NANOMIPS_GPREL_18_S3
# and R_NANOMIPS_GPREL_16_S2

_start:
    lw $a0, %gp_rel(a)($gp)
    lh $a1, %gp_rel(a)($gp)
    addiu.b $a1, $gp, %gp_rel(a)
    addiu.w $a2, $gp, %gp_rel(a)
    addiu.b32 $a3, $gp, %gp_rel(a)


    .end _start
    .size _start, .-_start

    .section .sdata, "aw", @progbits
    .align 1
    .skip 4
    .globl a
    .type a, @object
    .size a, 4
a:
    .4byte 1

    .globl b
    .type b, @object
    .size b, 2

b:
    .2byte 2

    .skip 0x1000
    .globl c
    .type c, @object
    .size c, 4
c:
    .4byte 3
