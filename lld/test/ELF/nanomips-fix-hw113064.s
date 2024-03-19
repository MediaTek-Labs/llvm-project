# REQUIRES: nanomips

# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpcrel %s -o %t.o
# RUN: ld.lld --relax --fix-nmips-hw113064 --section-start .fix_neg_sec=0x20012 --section-start .text=0x2000000 --section-start .fix_pos_sec=0x3fe0004 %t.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s

    .linkrelax
    .section .fix_neg_sec, "ax", @progbits
    .align 1
    .globl fix_neg
    .ent fix_neg

# CHECK: 6023 {{.*}} lapc at{{.*}}<fix_pos>
# CHECK: d820 jrc at
# CHECK-NEXT: 29{{.*}} bc {{.*}}<fix_pos>
# CHECK-NEXT: 2a{{.*}} balc {{.*}}<fix_neg>
# CHECK-NEXT: 6023{{.*}} lapc {{.*}}
# CHECK: d830 jalrc at

fix_neg:

    addiu $a1, $a2, 1

    .end fix_neg
    .size fix_neg, .-fix_neg

    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start


_start:
    
    bc fix_pos
    # Won't expand
    bc fix_pos
    # Won't expand
    balc fix_neg
    balc fix_neg

    .end _start
    .size _start, .-_start

    .section .fix_pos_sec, "ax", @progbits
    .align 1
    .globl fix_pos
    .ent fix_pos

fix_pos:

    addiu $a1, $a2, 1

    .end fix_pos
    .size fix_pos, .-fix_pos
