# REQUIRES: nanomips
# TODO: When the define directive is availble, use it
# DEFINE: %{skip_bc_sym} = __skip_bc__
 
# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpcrel %s -o %t1.o
# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mpcrel %S/Inputs/nanomips-pcrel-relax-sup.s -o %t2.o
# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t2.o %t1.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMF-PCREL %s

# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --insn32 --relax %t2.o %t1.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-INSN32 %s

# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mno-pcrel %s -o %t1.o
# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t2.o %t1.o -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMF-ABS %s

# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mpcrel --defsym nms=1 %s -o %t1.o
# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t2.o %t1.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMS-PCREL %s

# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mno-pcrel --defsym nms=1 %s -o %t1.o
# RUN: ld.lld -T %S/Inputs/nanomips-pcrel-relax.ld --relax %t2.o %t1.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck -check-prefix=CHECK-NMS-ABS %s


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

# CHECK-NMF-PCREL: dae{{.*}} bnec {{.*}} <__skip_bc__{{[0-9]*}}>
# CHECK-NMF-PCREL-NEXT: 28{{.*}} bc {{.*}} <pc14_far>
# CHECK-NMF-PCREL: db5{{.*}} beqc {{.*}} <__skip_bc__{{[0-9]*}}>
# CHECK-NMF-PCREL: a8c5{{.*}} bltc {{.*}} <__skip_bc__{{[0-9]*}}>
# CHECK-NMF-PCREL: 88c5{{.*}} bgec {{.*}} <__skip_bc__{{[0-9]*}}>
# CHECK-NMF-PCREL: a8c5{{.*}} bltuc {{.*}} <__skip_bc__{{[0-9]*}}>
# CHECK-NMF-PCREL: 88c5{{.*}} bgeuc {{.*}} <__skip_bc__{{[0-9]*}}>

# Will only check differences from others from now on

# CHECK-INSN32: 88e6{{.*}} beqc {{.*}} <fun>
# CHECK-INSN32: a8a6{{.*}} bnec {{.*}} <fun>
# CHECK-INSN32: 88c0{{.*}} beqzc {{.*}} <a>


# CHECK-NMF-ABS: 60a0{{.*}} li {{.*}}
# CHECK-NMF-ABS: 60e0{{.*}} li {{.*}}


# CHECK-NMS-PCREL: e0b{{.*}} aluipc {{.*}}
# CHECK-NMS-PCREL: 80a5{{.*}} ori {{.*}}
# CHECK-NMS-PCREL: e0e{{.*}} aluipc {{.*}}
# CHECK-NMS-PCREL: 80e7{{.*}} ori {{.*}}
# CHECK-NMS-PCREL: 88e6{{.*}} beqc {{.*}} <fun>
# CHECK-NMS-PCREL: a8c5{{.*}} bnec {{.*}} <fun>
# CHECK-NMS-PCREL: 88c0{{.*}} beqzc {{.*}} <a>

# CHECK-NMS-ABS: e0a{{.*}} lui {{.*}}%hi
# CHECK-NMS-ABS: 80a5{{.*}} ori {{.*}}
# CHECK-NMS-ABS: e0e{{.*}} lui {{.*}}%hi
# CHECK-NMS-ABS: 80e7{{.*}} ori {{.*}}


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
