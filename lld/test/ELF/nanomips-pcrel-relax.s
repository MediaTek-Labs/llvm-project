# REQUIRES: nanomips

# TODO: nanoMIPS Subset not supported yet (nms), test needs to be changed
# according

# TODO: insn32 as well, seems like not all is implemented



# RUN: llvm-mc -filetype=obj -mcpu=i7200 -triple=nanomips-elf \
# RUN: -mattr=+pcrel %s -o %t.o

# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t.o -o %t

# RUN: llvm-objdump -d %t | FileCheck %s -check-prefix=CHECK-NMF-PCREL

# RUN: llvm-mc -filetype=obj -mcpu=i7200 -triple=nanomips-elf \
# RUN: -mattr=-pcrel %s -o %t.o

# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t.o -o %t

# RUN: llvm-objdump -d %t | FileCheck %s -check-prefix=CHECK-NMF-ABS

    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start


_start:
    # 0x1000
    lapc.b $a1, start_lapc_relax - 0x2008 - 0x200000 + 0x1004
relaxed_lapc:

    # 0x1004

    .end _start
    .size _start, .-_start


    # Also includes some relocs which it relaxes, expands to
    .section .pc14_s1, "ax", @progbits
    .align 1
    .globl pc14_s1
    .ent pc14_s1

pc14_s1:
    # PC14_S1

    # CHECK-NMF-PCREL: c5 88 1e 00 beqc $a1, $a2, 0x2022
    # 0x2000
    beqc $a1, $a2, pc14_s1_pc5u_far + 0x2

    # CHECK-NMF-PCREL-NEXT: ef da bnec $a2, $a1, 0x2024
    # 0x2004
    # No expand, not far enough
    bnec $a2, $a1, pc14_s1_pc5u_far + 0x4

    # CHECK-NMF-PCREL-NEXT: a6 a8 f7 2f bnec $a2, $a1, 0x1000
    # 0x2006
    # Negative jump should be expanded to 4byte version
    # Registers should be already swapped
    bnec $a1, $a2, _start
start_lapc_relax:

    # CHECK-NMF-PCREL: c5 88 00 00 beqc $a1, $a2, 0x200e
    # 0x200a
    # Expanded as zero jumps are not allowed for 2byte
    # Note: Maybe optimize this to delete instruction?
    beqc $a1, $a2, zero_jump
zero_jump:

    # CHECK-NMF-PCREL: cc 88 0e 00 beqc $t0, $a2, 0x2020
    # 0x200e
    # Incompatible regs for 2 byte
    beqc $t0, $a2, pc14_s1_pc5u_far

    # CHECK-NMF-PCREL-NEXT: 5f db beqc $a1, $a2, 0x2032
    # 0x2012
    # Expand then relax
    beqc $a1, $a2, relaxed_lapc + 0x102e
    
    # CHECK-NMF-PCREL-NEXT: ef da bnec $a2, $a1, 0x2034
    # 0x2014
    # Expand then relax
    bnec $a1, $a2, relaxed_lapc + 0x1030

    # CHECK-NMF-PCREL-NEXT: a0 88 e7 2f beqzc $a1, 0x1000 <_start>
    # 0x2016
    # Expand, too negative
    beqzc $a1, _start

    # CHECK-NMF-PCREL-NEXT: a0 a8 7e 00 bnezc $a1, 0x209c
    # 0x201a
    # Expand, too positive
    bnezc $a1, pc14_s1_pc8_far + 0x1c

    # CHECK-NMF-PCREL-NEXT: fe ba bnezc $a1, 0x209e
    # 0x201e
    # Expand then relax
    bnezc $a1, relaxed_lapc + 0x109a

    # CHECK-NMF-PCREL-NEXT: 81 9a beqzc $a1, 0x1fa2
    # 0x2020
    # Expand then relax negative
    beqzc $a1, expanded_lapc - 0x0fe4 - 0x80

    # CHECK-NMF-PCREL: e2 da bnec $a2, $a1, 0x2028
    # CHECK-NMF-PCREL-NEXT: 00 28 d8 42 bc 0x6300
    # Opposite branches from now, expansions
    # 0x2022
    beqc $a1, $a2, pc14_s1_pc15_far + 0x300
    
    # CHECK-NMF-PCREL: 52 db beqc $a1, $a2, 0x202e
    # CHECK-NMF-PCREL-NEXT: 00 28 d2 42 bc 0x6300
    # 0x2028
    bnec $a1, $a2, pc14_s1_pc15_far + 0x300

    # CHECK-NMF-PCREL: c5 a8 04 80 bltc $a1, $a2, 0x2036
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 3f bc 0x6032
    # 0x202e
    bgec $a1, $a2, pc14_s1_pc15_far + 0x32

    # CHECK-NMF-PCREL: c5 88 04 80 bgec $a1, $a2, 0x203e
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 3f bc 0x603a
    # 0x2036
    bltc $a1, $a2, pc14_s1_pc15_far + 0x3a

    # 0x204e
    # CHECK-NMF-PCREL: c5 a8 04 c0 bltuc $a1, $a2, 0x2046
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 3f bc 0x6042
    bgeuc $a1, $a2, pc14_s1_pc15_far + 0x42

    # CHECK-NMF-PCREL: c5 88 04 c0 bgeuc $a1, $a2, 0x204e
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 3f bc 0x604a
    # 0x2056
    bltuc $a1, $a2, pc14_s1_pc15_far + 0x4a

    # 0x205e

    .end pc14_s1
    .size pc14_s1, .-pc14_s1


    .section .lapc_expands, "ax", @progbits
    .align 1
    .globl lapc_expands_fun
    .ent lapc_expands_fun

lapc_expands_fun:
    # 0x3000
    lapc.h $a1, lapc_far
expanded_lapc:

    # 0x3006
    .end lapc_expands_fun
    .size lapc_expands_fun, .-lapc_expands_fun

    .section .pc21_s1, "ax", @progbits
    .align 1
    .globl pc21_s1
    .ent pc21_s1

pc21_s1:
    # PC21_S1
    # CHECK-NMF-PCREL: a3 60 fa bf ff 09 lapc.b $a1, 0xa000000
    # CHECK-NMF-ABS: a0 60 00 00 00 0a li $a1, 0xa000000
    # 0x4000
    # Expand
    lapc.h $a1, lapc_far

    # CHECK-NMF-PCREL-NEXT: 85 10 move $a0, $a1
    # CHECK-NMF-PCREL-NEXT: 1f 2a fe ff balc 0x20400a
    # 0x4006
    # Expand
    move.balc $a0, $a1, pc21_s1_pc22_far + 0xa

    # CHECK-NMF-PCREL-NEXT: bf 08 fe ff move.balc $a0, $a1, 0x20400e
    # 0x400c
    # No expand
    move.balc $a0, $a1, pc21_s1_pc22_far + 0xe

    # CHECK-NMF-PCREL-NEXT: bf 04 fe ff lapc.h $a1, 0x204012
    # Not relaxed for abs
    # CHECK-NMF-ABS: a0 60 12 40 20 00 li $a1, 0x204012
    # 0x4010
    # Expand then relax
    lapc.h $a1, relaxed_lapc + 0x204012 - 0x1004

    # 0x4014
    .end pc21_s1
    .size pc21_s1, .-pc21_s1

    .section .pc25_s1, "ax", @progbits
    .align 1
    .globl pc25_s1
    .ent pc25_s1

pc25_s1:
    # PC25_S1

    # CHECK-NMF-PCREL: 00 28 fe 03 bc 0x5402
    # 0x5000
    # Expand
    bc pc25_s1_pc11_far + 0x2

    # CHECK-NMF-PCREL-NEXT: fe 3b balc[16] 0x5404
    # 0x5004
    # No expand, not far enough
    balc pc25_s1_pc11_far + 0x4

    # CHECK-NMF-PCREL-NEXT: 01 38 balc[16] 0x4c08
    # 0x5006
    # Expand then relax, negative
    balc lapc_relax_2 - 0x13fe

    # CHECK-NMF-PCREL-NEXT: ff 2b fd fb balc 0x4c08
    # 0x5008
    # Expand, negative
    balc lapc_relax_2 - 0x13fe

    # CHECK-NMF-PCREL-NEXT: 23 60 ee af ff 09 lapc.b $at, 0xa000000
    # CHECK-NMF-PCREL-NEXT: 20 d8 jrc $at
    # CHECK-NMF-ABS: 20 60 00 00 00 0a li $at, 0xa000000
    # CHECK-NMF-ABS-NEXT: 20 d8 jrc $at
    # 0x500c
    # Expand, then expand
    bc lapc_far

    # CHECK-NMF-PCREL-NEXT: 23 60 e6 ff ff fd lapc.b $at, 0xfe005000
    # CHECK-NMF-PCREL-NEXT: 30 d8 jalrc $ra, $at
    # CHECK-NMF-ABS-NEXT: 20 60 00 50 00 fe li $at, 0xfe005000
    # CHECK-NMF-ABS-NEXT: 30 d8 jalrc $ra, $at
    # 0x5014
    # Expand, then expand, negative
    balc pc25_s1 - 0x2000000


    .end pc25_s1
    .size pc25_s1, .-pc25_s1

    .section .lapc_expands_2, "ax", @progbits
    .align 1
    .globl lapc_expands_2
    .ent lapc_expands_2

lapc_expands_2:
    # 0x6000
    lapc.h $a1, lapc_far
lapc_relax_2:
    # 0x6006

    .end lapc_expands_2
    .size lapc_expands_2, .-lapc_expands_2

    .section .pc11_s1, "ax", @progbits
    .align 1
    .globl pc11_s1
    .ent pc11_s1

pc11_s1:
    # PC11_S1

    # CHECK-NMF-PCREL: a4 c8 fc 2f bbeqzc $a1, 0x5, 0x7800
    # 0x7000
    # No expansion, not too far
    bbeqzc $a1, 5, pc11_s1_pc12_far
    
    # CHECK-NMF-PCREL-NEXT: b4 c8 04 28 bbnezc $a1, 0x5, 0x700c
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 07 bc 0x7808
    # Expansion, opposite branches
    # 0x7004
    bbeqzc $a1, 5, pc11_s1_pc12_far + 0x8

    # CHECK-NMF-PCREL: a4 c8 04 28 bbeqzc $a1, 0x5, 0x7014
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 07 bc 0x7810
    # 0x700c
    bbnezc $a1, 5, pc11_s1_pc12_far + 0x10

    # CHECK-NMF-PCREL: b0 c8 04 28 bneic $a1, 0x5, 0x701c
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 07 bc 0x7818
    # 0x7014
    beqic $a1, 5, pc11_s1_pc12_far + 0x18

    # CHECK-NMF-PCREL: a0 c8 04 28 beqic $a1, 0x5, 0x7024
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 07 bc 0x7820
    # 0x701c
    bneic $a1, 5, pc11_s1_pc12_far + 0x20

    # CHECK-NMF-PCREL: b8 c8 04 28 bltic $a1, 0x5, 0x702c
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 07 bc 0x7828
    # 0x7024
    bgeic $a1, 5, pc11_s1_pc12_far + 0x28

    # CHECK-NMF-PCREL: a8 c8 04 28 bgeic $a1, 0x5, 0x7034
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 07 bc 0x7830
    # 0x702c
    bltic $a1, 5, pc11_s1_pc12_far + 0x30

    # CHECK-NMF-PCREL: bc c8 04 28 bltiuc $a1, 0x5, 0x703c
    # CHECK-NMF-PCREL-NEXT: 00 28 fc 07 bc 0x7838
    # 0x7034
    bgeiuc $a1, 5, pc11_s1_pc12_far + 0x38

    # CHECK-NMF-PCREL: ac c8 04 28 bgeiuc $a1, 0x5, 0x7044
    # CHECK-NMF-PCREL-NEXT: ff 29 bd f7 bc 0x6800
    # 0x703c
    # Negative
    bltiuc $a1, 5, pc11_s1 - 0x800

    .end pc11_s1
    .size pc11_s1, .-pc11_s1
