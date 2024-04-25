# REQUIRES: nanomips
# TODO: When the define directive is availble, use it
# DEFINE: %{skip_bc_sym} = __skip_bc__
 
# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpcrel %s -o %t1.o
# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mpcrel %S/Inputs/nanomips-pcrel-relax-sup.s -o %t2.o
# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t2.o %t1.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMF-PCREL %s

# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --insn32 --relax %t2.o %t1.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMF-INSN32 %s

# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mno-pcrel %s -o %t1.o
# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t2.o %t1.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMF-ABS %s

# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mpcrel --defsym nms=1 %s -o %t1.o
# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t2.o %t1.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMS-PCREL %s

# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax --insn32 %t2.o %t1.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMS-PCREL-INSN32 %s

# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mno-pcrel --defsym nms=1 %s -o %t1.o
# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t2.o %t1.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMS-ABS %s

# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax --insn32 %t2.o %t1.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMS-ABS-INSN32 %s


# CHECK-NMF-PCREL: 88e6{{.*}} beqc {{.*}} <_start>
# CHECK-NMF-PCREL: a8a6{{.*}} bnec {{.*}} <zero_jump>
# CHECK-NMF-PCREL: 88a4{{.*}} beqc {{.*}} <a>
# CHECK-NMF-PCREL: 60a3{{.*}} lapc {{.*}} <x3>
# CHECK-NMF-PCREL: 10a6 move {{.*}}
# CHECK-NMF-PCREL: 2a{{.*}} balc {{.*}} <x3>
# CHECK-NMF-PCREL: 60e3{{.*}} lapc {{.*}} <fun+0x1>
# CHECK-NMF-PCREL: 048{{.*}} lapc {{.*}} <fun>
# CHECK-NMF-PCREL: dbe{{.*}} beqc {{.*}} <fun>

# CHECK-NMF-PCREL: dae{{.*}} bnec {{.*}} <fun>

# CHECK-NMF-PCREL: 9b6{{.*}} beqzc {{.*}} <a>
# CHECK-NMF-PCREL: a8cc{{.*}} bnec {{.*}} <x>
# CHECK-NMF-PCREL: a980{{.*}} bnezc {{.*}} <x>
# CHECK-NMF-PCREL: 88c6{{.*}} beqc {{.*}} <x>
# CHECK-NMF-PCREL: a8c6{{.*}} bnec {{.*}} <x>

# CHECK-NMF-PCREL: a8e0{{.*}} bnezc {{.*}} <x2>
# CHECK-NMF-PCREL: 88e6{{.*}} beqc {{.*}} <x1>

# CHECK-NMF-PCREL: dae{{.*}} bnec {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc14_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: db5{{.*}} beqc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc14_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: a8c5{{.*}} bltc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc14_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: 88c5{{.*}} bgec {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc14_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: a8c5{{.*}} bltuc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc14_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: 88c5{{.*}} bgeuc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>



# CHECK-NMF-PCREL: 2a{{.*}} balc {{.*}} <pc10_far
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc10_far_a>
# CHECK-NMF-PCREL-NEXT: 3{{.*}} balc {{.*}} <pc10_far_a>
# CHECK-NMF-PCREL-NEXT: 1{{.*}} bc {{.*}} <pc10_far_a>

# CHECK-NMF-PCREL: 88a0{{.*}} beqzc {{.*}}<pc7_far
# CHECK-NMF-PCREL-NEXT: a8a0{{.*}} bnezc {{.*}}<pc7_far_a>
# CHECK-NMF-PCREL-NEXT: 9af{{.*}} beqzc {{.*}} <pc7_far_a>
# CHECK-NMF-PCREL-NEXT: baf{{.*}} bnezc {{.*}} <pc7_far_a>

# CHECK-NMF-PCREL: 6023{{.*}} lapc at{{.*}}<pc25_far>
# CHECK-NMF-PCREL: d820 jrc at
# CHECK-NMF-PCREL-NEXT: 6023{{.*}} lapc at{{.*}}<pc25_far>
# CHECK-NMF-PCREL: d830 jalrc at
# CHECK-NMF-PCREL-NEXT: 29{{.*}} bc {{.*}}<pc25_far>

# CHECK-NMF-PCREL: c8{{.*}} bbnezc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc11_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: c8{{.*}} bbeqzc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc11_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: c8{{.*}} bneic {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc11_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: c8{{.*}} beqic {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc11_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: c8{{.*}} bltic {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc11_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: c8{{.*}} bgeic {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc11_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: c8{{.*}} bltiuc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc11_far>
# CHECK-NMF-PCREL: <__skip_bc__[[BC_NUM]]>
# CHECK-NMF-PCREL-NEXT: c8{{.*}} bgeiuc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>


# Will only check differences from others from now on

# CHECK-NMF-INSN32: 88e6{{.*}} beqc {{.*}} <fun>
# CHECK-NMF-INSN32: a8a6{{.*}} bnec {{.*}} <fun>
# CHECK-NMF-INSN32: 88c0{{.*}} beqzc {{.*}} <a>


# CHECK-NMF-ABS: 60a0{{.*}} li {{.*}}
# CHECK-NMF-ABS: 60e0{{.*}} li {{.*}}

# CHECK-NMF-ABS: 6020{{.*}} li at{{.*}}
# CHECK-NMF-ABS: d820 jrc at
# CHECK-NMF-ABS-NEXT: 6020{{.*}} li at{{.*}}
# CHECK-NMF-ABS: d830 jalrc at
# CHECK-NMF-ABS-NEXT: 29{{.*}} bc {{.*}}<pc25_far> 


# CHECK-NMS-PCREL: e0b{{.*}} aluipc {{.*}}
# CHECK-NMS-PCREL: 80a5{{.*}} ori {{.*}}
# CHECK-NMS-PCREL: e0e{{.*}} aluipc {{.*}}
# CHECK-NMS-PCREL: 80e7{{.*}} ori {{.*}}
# CHECK-NMS-PCREL: 88e6{{.*}} beqc {{.*}} <fun>
# CHECK-NMS-PCREL: a8c5{{.*}} bnec {{.*}} <fun>
# CHECK-NMS-PCREL: 88c0{{.*}} beqzc {{.*}} <a>

# CHECK-NMS-PCREL: e02{{.*}} aluipc at,
# CHECK-NMS-PCREL-NEXT: 8021{{.*}} ori at,at,
# CHECK-NMS-PCREL-NEXT: d820 jrc at
# CHECK-NMS-PCREL-NEXT: e02{{.*}} aluipc at,
# CHECK-NMS-PCREL-NEXT: 8021{{.*}} ori at,at
# CHECK-NMS-PCREL-NEXT: d830 jalrc at
# CHECK-NMS-PCREL-NEXT: 29{{.*}} bc {{.*}}<pc25_far>

# CHECK-NMS-PCREL-INSN32: e02{{.*}} aluipc at,
# CHECK-NMS-PCREL-INSN32-NEXT: 8021{{.*}} ori at,at,
# CHECK-NMS-PCREL-INSN32-NEXT: 4801 0000 jrc at
# CHECK-NMS-PCREL-INSN32-NEXT: e02{{.*}} aluipc at,
# CHECK-NMS-PCREL-INSN32-NEXT: 8021{{.*}} ori at,at
# CHECK-NMS-PCREL-INSN32-NEXT: 4be1 0000 jalrc at
# CHECK-NMS-PCREL-INSN32-NEXT: 29{{.*}} bc {{.*}}<pc25_far>

# CHECK-NMS-ABS: e0a{{.*}} lui {{.*}}%hi
# CHECK-NMS-ABS: 80a5{{.*}} ori {{.*}}
# CHECK-NMS-ABS: e0e{{.*}} lui {{.*}}%hi
# CHECK-NMS-ABS: 80e7{{.*}} ori {{.*}}

# CHECK-NMS-ABS: e02{{.*}} lui at,%hi
# CHECK-NMS-ABS-NEXT: 8021{{.*}} ori at,at
# CHECK-NMS-ABS-NEXT: d820 jrc at
# CHECK-NMS-ABS-NEXT: e02{{.*}} lui at,%hi
# CHECK-NMS-ABS-NEXT: 8021{{.*}} ori at,at
# CHECK-NMS-ABS-NEXT: d830 jalrc at
# CHECK-NMS-ABS-NEXT: 29{{.*}} bc {{.*}}<pc25_far>

# CHECK-NMS-ABS-INSN32: e02{{.*}} lui at,%hi
# CHECK-NMS-ABS-INSN32-NEXT: 8021{{.*}} ori at,at
# CHECK-NMS-ABS-INSN32-NEXT: 4801 0000 jrc at
# CHECK-NMS-ABS-INSN32-NEXT: e02{{.*}} lui at,%hi
# CHECK-NMS-ABS-INSN32-NEXT: 8021{{.*}} ori at,at
# CHECK-NMS-ABS-INSN32-NEXT: 4be1 0000 jalrc at
# CHECK-NMS-ABS-INSN32-NEXT: 29{{.*}} bc {{.*}}<pc25_far>

# Starting testcase of nanomips relaxations, needs to convert
# 32bit beqc to 16 bit beqc
# And for expansions 32 bit lapc to 48 bit lapc
    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

# relax R_NANOMIPS_PC14_S1
# expand R_NANOMIPS_PC4_S1 and R_NANOMIPS_PC21_S1
_start:
    # No relaxation bc of zero jump
    bnec $a1, $a2, zero_jump
zero_jump:
    # Should be relaxed then expanded as section changes size
    beqc $a0, $a1, a
    .skip 0x8
    lapc $a1, x3
    .ifndef nms
    move.balc $a1, $a2, x3
    .else
    .skip 0x4
    .endif
    # Expanded because of odd offset
    lapc $a3, fun + 1
    lapc $a0, fun
    bgec $a1, $a7, fun
    beqc $a2, $a3, fun
a:
    bnec $a1, $a2, fun
    bgec $a1, $a4, fun
    addiu $a1, $a2, 3
    addiu $a2, $a3, 1
    .end _start
    .size _start, .-_start

    .section .other_text, "ax", @progbits
    .align 1
    .globl fun
    .ent fun
fun:
    addiu $a3, $a2, 1
    beqc $zero, $a2, a
    # No relax because of invalid regs for relax
    bnec $t0, $a2, x
    bnec $zero, $t0, x
    # No relax because of sReg==tReg
    beqc $a2, $a2, x
    bnec $a2, $a2, x 

    .end fun
    .size fun, .-fun

    .globl x
    .ent x
x:
    addiu $a3, $a2, 2
    bnec $zero, $a3, x2
    beqc $a2, $a3, x1
    .skip 0x20
x1:
    .skip 0x5c
x2:
# Only used to test on big obj files as Inputs
# as there was an error before
    .skip 0x7e0000
x3:
    .end x
    .size x, .-x

# expand R_NANOMIPS_PC14_S1
    .section .expand_pc14_sec, "ax", @progbits
    .align 1
    .globl expand_pc14
    .ent expand_pc14
expand_pc14:

    beqc $a1, $a2, pc14_far
    bnec $a1, $a2, pc14_far
    bgec $a1, $a2, pc14_far
    bltc $a1, $a2, pc14_far
    bgeuc $a1, $a2, pc14_far
    bltuc $a1, $a2, pc14_far

    .end expand_pc14
    .size expand_pc14, .-expand_pc14

    .section .pc14_far_sec, "ax", @progbits
    .align 1
    .globl pc14_far
    .ent pc14_far

pc14_far:
    addiu $a1, $a2, 1
    .end pc14_far
    .size pc14_far, .-pc14_far


# expand R_NANOMIPS_PC10_S1 + relax R_NANOMIPS_PC25_S1
# First there is a relaxation to 16bit version then there is an expansion

    .section .expand_pc10_sec, "ax", @progbits
    .align 1
    .globl expand_pc10
    .ent expand_pc10

expand_pc10:
    balc pc10_far_a-4
    bc pc10_far_a
    # pc10_far_a is not far for next two
    # as pc has moved, if lapc expands
    # to 6byte lapc
    balc pc10_far_a
    bc pc10_far_a

    .end expand_pc10
    .size expand_pc10, .-expand_pc10

    .section .pc10_far_sec, "ax", @progbits
    .align 1
    .globl pc10_far
    .ent pc10_far

pc10_far:
    lapc $a1, pc10_far_sec_lapc_far
pc10_far_a:
    addiu $a1, $a2, 1
    .end pc10_far
    .size pc10_far, .-pc10_far

# expand R_NANOMIPS_PC7_S1, after relaxing R_NANOMIPS_PC_14_S1

    .section .expand_pc7_sec, "ax", @progbits
    .align 1
    .globl expand_pc7
    .ent expand_pc7
    
expand_pc7:
    beqzc $a1, pc7_far_a-4
    bnezc $a1, pc7_far_a
    # pc7_far is not far for next two
    # if lapc expands to 6byte lapc
    beqzc $a1, pc7_far_a
    bnezc $a1, pc7_far_a

    .end expand_pc7
    .size expand_pc7, .-expand_pc7

    .section .pc7_far_sec, "ax", @progbits
    .align 1
    .globl pc7_far
    .ent pc7_far

pc7_far:
    lapc $a1, pc7_far_sec_lapc_far
pc7_far_a:
    addiu $a1, $a2, 1
    .end pc7_far
    .size pc7_far, .-pc7_far

    .section .expand_pc25_sec, "ax", @progbits
    .align 1
    .globl expand_pc25
    .ent expand_pc25

expand_pc25:
    bc pc25_far
    balc pc25_far
    # Shouldn't expand
    bc pc25_far

    .end expand_pc25
    .size expand_pc25, .-expand_pc25

    .section .pc25_far_sec, "ax", @progbits
    .align 1
    .globl pc25_far
    .ent pc25_far

pc25_far:
    addiu $a1, $a2, 1
    .end pc25_far
    .size pc25_far, .-pc25_far


    .section .expand_pc11_sec, "ax", @progbits
    .align 1
    .globl expand_pc11
    .ent expand_pc11

expand_pc11:
    .ifndef nms
    bbeqzc $a1, 5, pc11_far
    bbnezc $a1, 5, pc11_far
    beqic $a1, 10, pc11_far
    bneic $a1, 10, pc11_far
    bgeic $a1, 15, pc11_far
    bltic $a1, 15, pc11_far
    bgeiuc $a1, 20, pc11_far
    bltiuc $a1, 20, pc11_far
    # Shouldn't expand as it is not too far
    bbeqzc $a1, 25, pc11_far
    .endif
    .end expand_pc11
    .size expand_pc11, .-expand_pc11

    .section .pc11_far_sec, "ax", @progbits
    .align 1
    .globl pc11_far
    .ent pc11_far

pc11_far:
    addiu $a1, $a2, 1
    .end pc11_far
    .size pc11_far, .-pc11_far
