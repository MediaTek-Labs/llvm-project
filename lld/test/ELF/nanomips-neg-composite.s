# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -m32 -EL -march=32r6 %s -o %t.o
# RUN: ld.lld --section-start .text=0x80020000 --section-start .rodata=0x80021000 --section-start .eh_frame=0x80022000 %t.o -o %t
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -d %t | FileCheck %s
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -s --section=.rodata %t | FileCheck %s --check-prefix=CHECK-JUMP
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -s --section=.eh_frame %t | FileCheck %s --check-prefix=CHECK-EH

# CHECK: 80020004 <_start>
# CHECK: 8002000e: 4806 8000 brsc a2
# CHECK-JUMP: 80021000 f9
# CHECK-EH: 80022020 0e000000

    .linkrelax
    .module pcrel
    .module softfloat
    .section .text, "ax", @progbits
    .align 1
    .skip 4
    .globl _start
    .cfi_startproc
    .ent _start

_start:

    li $a3, 0
    lapc $a2, jump_table
    .reloc 1f, R_NANOMIPS_JUMPTABLE_LOAD, jump_table
1:
    lbx $a2, $a3($a2)
brsc_ins:
    brsc $a2

    .end _start
    .cfi_endproc
    .size _start, .-_start

    .section .rodata
    .align 2
    .jumptable 1, 1, 0
jump_table:
    .sbyte (_start - (brsc_ins + 4)) >> 1
