# REQUIRES: nanomips

# Tests R_NANOMIPS_RELAX, R_NANOMIPS_NORELAX, R_NANOMIPS_FIXED, R_NANOMIPS_INSN32

# A work in progress
# XFAIL: *

# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpcrel %s -o %t.o
# RUN: ld.lld --section-start .text=0x1000 --section-start .other_text=0x1040 --relax %t.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s

# CHECK: 88a0 {{.*}} beqzc a1,{{.*}} <target>
# CHECK: 9a{{.*}} beqzc a1,{{.*}} <target>
# CHECK-NEXT: 88c0 {{.*}} beqzc a2,{{.*}} <target>
# CHECK-NEXT: 88c0 {{.*}} beqzc a2,{{.*}} <target>

    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    .reloc 1f, R_NANOMIPS_NORELAX
1:    

    beqzc $a1, target
    
    .reloc 2f, R_NANOMIPS_RELAX
2:

    beqzc $a1, target

    .reloc 3f, R_NANOMIPS_INSN32
3: 
    beqzc $a2, target

    .reloc 4f, R_NANOMIPS_FIXED
4: 
    beqzc $a2, target



    .end _start
    .size _start, .-_start

    .section .other_text, "ax", @progbits
    .align 1
    .globl target
    .ent target

target:

    addiu $a1, $a1, 1
    .end target
    .size target, .-target

