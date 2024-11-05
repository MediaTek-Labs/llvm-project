# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+pcrel %s -o %t.o

# RUN: ld.lld --relax --fix-nmips-hw113064 --section-start \
# RUN: .fix_neg_sec=0x20012 --section-start .text=0x2000000 \
# RUN: --section-start .fix_pos_sec=0x3fe0004 %t.o -o %t

    .linkrelax
    .section .fix_neg_sec, "ax", @progbits
    .align 1
    .globl fix_neg
    .ent fix_neg

fix_neg:

    addiu $a1, $a2, 1

    .end fix_neg
    .size fix_neg, .-fix_neg

    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    # CHECK: 23 60 fe ff fd 01 lapc.b $at, 0x3fe0004
    # CHECK-NEXT: 20 d8 jrc $at
    bc fix_pos
    # No expand
    # CHECK-NEXT: fd 29 f8 ff bc 0x3fe0004
    bc fix_pos
    # No expand
    # CHECK-NEXT: 02 2a 03 00 balc 0x20012
    balc fix_neg
    # CHECK-NEXT: 23 60 fc ff 01 fe lapc.b $at, 0x20012
    # CHECK-NEXT: 30 d8 jalrc $ra, $at
    balc fix_neg

    .ent _start
    .size _start, .-_start

    .section .fix_pos_sec, "ax", @progbits
    .align 1
    .globl fix_pos
    .ent fix_pos

fix_pos:

    addiu $a1, $a2, 1

    .end fix_pos
    .size fix_pos, .-fix_pos