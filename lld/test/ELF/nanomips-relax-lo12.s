# REQUIRES: nanomips

# RUN: %nanomips-elf-as -EL -march=32r6 -m32 %s -o %t.o
# RUN: ld.lld --relax --relax-lo12 --section-start .sym_sec=0x303c --section-start .text=0x2000 %t.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s
    
    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

# CHECK: 16e{{.*}} lw a1,{{.*}}(a2)
# CHECK-NEXT: 946{{.*}} sw zero,{{.*}}(a2)
# CHECK-NEXT: 8586{{.*}} lw t0,{{.*}}(a2)
# CHECK-NEXT: 8606{{.*}} sw s0,{{.*}}(a2)
# CHECK-NEXT: 84a6{{.*}} lw a1,{{.*}}(a2)
# CHECK-NEXT: 84a6{{.*}} lw a1,{{.*}}(a2)


_start:

    lw $a1, %lo(relaxable_lo12)($a2)
    sw $zero, %lo(relaxable_lo12)($a2)

    # Non valid regs
    lw $t0, %lo(relaxable_lo12)($a2)
    sw $s0, %lo(relaxable_lo12)($a2)

    # Non aligned offset
    lw $a1, %lo(relaxable_lo12 + 2)($a2)

    # Too large offset
    lw $a1, %lo(relaxable_lo12 + 4)($a2)


    .end _start
    .size _start, .-_start

    .section .sym_sec, "ax", @progbits
    .align 1
    .globl relaxable_lo12
    .ent relaxable_lo12

relaxable_lo12:
    
    addiu $a1, $a2, 1

    .end relaxable_lo12
    .size relaxable_lo12, .-relaxable_lo12


