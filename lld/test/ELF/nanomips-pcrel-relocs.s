# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu=i7200 %s -o %t.o
# RUN: ld.lld --section-start .text=0x1000 --section-start .sdata=0x13000 %t.o -o %t
# RUN: llvm-objdump --triple=nanomips-elf --mcpu=i7200  -d %t | FileCheck %s

# CHECK: 1000: 0a 38 balc[16]{{.*}} <label>
# CHECK-NEXT: 1002: e4 db beqc{{.*}} <label>
# CHECK-NEXT: 1004: e0 04 04 00 lapc.h{{.*}} <label>
# CHECK: 1010: ca 88 f9 3f beqc{{.*}} <label>
# CHECK-NEXT: 1014: e0 c8 f5 17 beqic{{.*}} <label>
# CHECK-NEXT: 1018: f3 9b beqzc{{.*}} <label>
# CHECK-NEXT: 101a: ab 60 04 00 01 00 lwpc{{.*}} <label_long>
# CHECK-NEXT: 1020: 01 2a 00 00 balc{{.*}} <label_long>
# CHECK: 11028: e0 e0 02 20 aluipc{{.*}}


    .section    .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start


_start:
    
    balc[16] label
    beqc $a2, $a3, label
    lapc $a3, label
    add $a2, $a2, $a3
label:
    add $a3, $a3, $a2

    beqc $a6, $a2, label
    beqic $a3, 2, label
    beqzc $a3, label
    lwpc $a1, label_long
    balc label_long

    .skip 0x10000, 0

label_long:
    add $a4, $a4, $a3

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
