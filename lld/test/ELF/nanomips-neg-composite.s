# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -m32 -EL -march=32r6 %s -o %t.o
# RUN: ld.lld --section-start .text=0x80020000 --section-start .rodata=0x80021000 %t.o -o %t
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -d %t
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -s --section=.rodata %t

# CHECK: 80020000 <_start>
# CHECK: 8002000a : 4806 8000 brsc a2
# CHECK: 80021000 <jump_table>
# CHECK-NEXT: 80021000 f9 
    .linkrelax
    .module pcrel
    .module softfloat
    .section .text, "ax", @progbits
    .align 1
    .globl _start
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
    .size _start, .-_start

    .section .rodata
    .align 2
    .jumptable 1, 1, 0
jump_table:
    .sbyte (_start - (brsc_ins + 4)) >> 1
