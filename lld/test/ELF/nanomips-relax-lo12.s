# REQUIRES: nanomips

# Also tests expansions of LO4_S2

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+pcrel %s -o %t.o
# RUN: ld.lld --relax --relax-lo12 --section-start .lo12_relax_sec=0x303c --section-start .text=0x2000 \
# RUN: --section-start .lo4_relax_expand_sec=0x4038 --section-start .lapc_far_sec=0x204050 %t.o -o %t
# RUN: llvm-objdump --triple=nanomips-elf -d %t | FileCheck %s
    
    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

# CHECK: ef 16{{.*}} lw $a1,{{.*}}($a2)
# CHECK-NEXT: 6f 94{{.*}} sw $zero,{{.*}}($a2)
# CHECK-NEXT: 86 85{{.*}} lw $t0,{{.*}}($a2)
# CHECK-NEXT: 06 86{{.*}} sw $s0,{{.*}}($a2)
# CHECK-NEXT: a6 84{{.*}} lw $a1,{{.*}}($a2)
# CHECK-NEXT: a6 84{{.*}} lw $a1,{{.*}}($a2)
# CHECK-NEXT: a6 84{{.*}} lw $a1,{{.*}}($a2)
# CHECK-NEXT: a6 84{{.*}} lw $a1,{{.*}}($a2)
# CHECK-NEXT: a6 84{{.*}} sw $a1,{{.*}}($a2)
# CHECK: a3 60{{.*}} lapc


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
    lw $a1, %lo(relaxable_lo12 - 0x40)($a2)

    # Relax then expand
    lw $a1, %lo(relax_expand_sym)($a2)
    sw $a1, %lo(relax_expand_sym)($a2)

    .end _start
    .size _start, .-_start

    .section .lo12_relax_sec, "ax", @progbits
    .align 1
    .globl relaxable_lo12
    .ent relaxable_lo12

relaxable_lo12:
    
    addiu $a1, $a2, 1

    .end relaxable_lo12
    .size relaxable_lo12, .-relaxable_lo12

    .section .lo4_relax_expand_sec, "ax", @progbits
    .align 1
    .globl relax_expand
    .ent relax_expand

relax_expand:
    lapc $a1, lapc_far

relax_expand_sym:
    .end relax_expand
    .size relax_expand, .-relax_expand

    .section .lapc_far_sec, "ax", @progbits
    .align 1
    .globl lapc_far
    .ent lapc_far

lapc_far:

    addiu $a1, $a2, 1
    .end lapc_far
    .size lapc_far, .-lapc_far





