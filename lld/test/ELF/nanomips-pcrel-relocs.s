# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -EL -march=32r6 -m32 %s -o %t.o
# RUN: ld.lld --section-start .text=0x1000 --section-start .eh_frame=0x12000 --section-start .sdata=0x13000 %t.o -o %t
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump -d %t | FileCheck %s
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump -s --section=.eh_frame %t | FileCheck %s --check-prefix=CHECK-EH

# CHECK: 1000: 380a balc{{.*}}
# CHECK: 1002: dbe4 beqc{{.*}}
# CHECK: 1004: 04e0 0004 lapc{{.*}}
# CHECK: 1010: 88e6 3ff9 beqc{{.*}}
# CHECK: 1014: c8e0 17f5 beqic{{.*}}
# CHECK: 1018: 9bf3 beqzc{{.*}}
# CHECK: 101a: 60ab 0004 lwpc{{.*}}
# CHECK: 101e: 0001
# CHECK: 1020: 2a01 0000 balc{{.*}}
# CHECK: 11028: e0e0 2002 aluipc{{.*}}

# CHECK-EH: 12010 {{[0-9a-f]+}} {{[0-9a-f]+}} {{[0-9a-f]+}} e4effeff

    .section    .text, "ax", @progbits
    .align 4
    .globl _start
    .cfi_startproc
    .ent _start


_start:
    
    balc label
    beqc $a2, $a3, label
    lapc $a3, label
    add $a2, $a2, $a3
label:
    add $a3, $a3, $a2

    beqc $a3, $a2, label
    beqic $a3, 2, label
    beqzc $a3, label
    lwpc $a1, label_long
    balc label_long

    .skip 0x10000, 0

label_long:
    add $a4, $a4, $a3

    aluipc $a3, %pcrel_hi(a)

    .end _start
    .cfi_endproc
    .size _start, .-_start

    .section .sdata, "aw", @progbits
    .align 4
    .skip 4
    .globl a
    .type a, @object
    .size a, 4
a:
    .long 4
