# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu=i7200 %s -o %t.o

# RUN: ld.lld --section-start .text=0x1000 --section-start .sdata=0x13000 \
# RUN: %t.o -o %t

# RUN: llvm-objdump --triple=nanomips-elf --mcpu=i7200  -d %t | FileCheck %s


    .section    .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start


_start:

    # CHECK: 1000: 380a balc[16]{{.*}} <label>
    balc[16] label
    # CHECK-NEXT: 1002: dbe4 beqc{{.*}} <label>
    beqc $a2, $a3, label
    # CHECK-NEXT: 1004: 04e0 0004 lapc.h{{.*}} <label>
    lapc $a3, label
    add $a2, $a2, $a3
label:
    add $a3, $a3, $a2

    # CHECK: 1010: 88ca 3ff9 beqc{{.*}} <label>
    beqc $a6, $a2, label
    # CHECK-NEXT: 1014: c8e0 17f5 beqic{{.*}} <label>
    beqic $a3, 2, label
    # CHECK-NEXT: 1018: 9bf3 beqzc{{.*}} <label>
    beqzc $a3, label
    # CHECK-NEXT: 101a: 60ab 0004 0001 lwpc{{.*}} <label_long>
    lwpc $a1, label_long
    # CHECK-NEXT: 1020: 2a01 0000 balc{{.*}} <label_long>
    balc label_long

    .skip 0x10000, 0

label_long:
    add $a4, $a4, $a3
    # CHECK: 11028: e0e0 2002 aluipc{{.*}}
    aluipc $a3, %pcrel_hi(a)

    .end _start
    .size _start, .-_start

    .section .sdata, "aw", @progbits
    .align 1
    .skip 4
    .globl a
    .type a, @object
    .size a, 4
a:
    .long 4
