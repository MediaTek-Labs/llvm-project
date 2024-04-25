# REQUIRES: nanomips
# Test initially copied from gold
# RUN: %nanomips-elf-as -EL -march=32r6 -m32 %s -o %t1
# RUN: %nanomips-elf-as -EL -march=32r6 -m32 %S/Inputs/nanomips-balc-trampoline-sup.s -o %t2
# RUN: ld.lld -T %S/Inputs/nanomips-balc-trampoline.ld %t2 %t1 -o %t
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s


# TODO: Change exact opcode with a symbol later

# CHECK: <sup_before>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM:[0-9]*]]>

# CHECK: <before_start>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>

# CHECK: <with_start>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>

# CHECK: <sup_fun>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>

# CHECK: <_start>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 60a3 {{.*}} lapc {{.*}} <a>
# CHECK: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 1{{.*}} bc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK: <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 28{{.*}} bc {{.*}} <fun>
# CHECK: <__skip_bc__[[BC_NUM]]>

# CHECK: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>

# CHECK: <with_start_after>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>

# CHECK: <expand>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM:[0-9]*]]>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 1{{.*}} bc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK: <__balc_tramp__[[BC_NUM]]>
# CHECK-NEXT: 29{{.*}} bc {{.*}} <fun>
# CHECK: <__skip_bc__[[BC_NUM]]>
# CHECK: 2b{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>

# CHECK: <align_fun>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM:[0-9]*]]>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]
# CHECK-NEXT: 8000 c000 nop
# CHECK-NEXT: 8000 c000 nop
# CHECK-NEXT: 9008 nop
# CHECK-NEXT: 9008 nop
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 1{{.*}} bc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK: <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 29{{.*}} bc {{.*}} <fun>
# CHECK: <__skip_bc__[[BC_NUM]]>

# CHECK: <transform_bc_fun>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM:[0-9]*]]>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK: 3{{.*}} balc {{.*}} <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 1{{.*}} bc {{.*}} <__skip_bc__[[BC_NUM:[0-9]*]]>
# CHECK: <__balc_tramp__[[TRAMP_NUM]]>
# CHECK-NEXT: 6023 {{.*}} lapc
# CHECK: d820 jrc at


    .linkrelax
    .module pcrel

    .section .before_text, "ax", @progbits
    .align 1
    .globl before_start
    .ent before_start

before_start:
    balc fun
    .end before_start
    .size before_start, .-before_start

    .section .with_text, "ax", @progbits
    .align 1
    .globl with_start
    .ent with_start

with_start:
    balc fun
    .end with_start
    .size with_start, .-with_start

    .section .text, "ax", @progbits
    .align 1

    .globl _start
    .ent _start

_start:
    balc fun
    balc fun
    balc fun
    lapc $a1, a
    balc fun
    .skip 0x3f8
    balc fun
    .end _start
    .size _start, .-_start

    .section .with_text_after, "ax", @progbits
    .align 1

    .globl with_start_after
    .ent with_start_after

with_start_after:
    balc fun
    .end with_start_after
    .size with_start_after,.-with_start_after

    .section .other_text, "ax", @progbits
    .align 1

    .globl fun
    .ent fun

fun:
    addiu $a2, $a2, 1
    .end fun
    .size fun, .-fun


    .section .tramp_expansion, "ax", @progbits
    .align 1

    .globl expand
    .ent expand

expand: 
    balc fun
    balc fun
    balc fun
    balc fun
    .skip 0x3f4
    addiu $a2, $a2, 1
    lapc $a1, long_dist_fun
    balc fun

    .end expand
    .size expand, .-expand

    .section .align_section, "ax", @progbits
    .align 1

    .globl align_fun
    .ent align_fun

align_fun:
    
    balc fun
    balc fun
    .align 4
    balc fun
    balc fun

    .end align_fun
    .size align_fun, .-align_fun

    .section .a_section, "ax", @progbits
    .align 1

    .globl a
    .ent a
a:
    addiu $a2, $a2, 1
b:
    .end a
    .size a, .-a

    .section .long_distance, "ax", @progbits
    .align 1

    .globl long_dist_fun
    .ent long_dist_fun

long_dist_fun:
    addiu $a1, $a2, 3

    .end long_dist_fun
    .size long_dist_fun, .-long_dist_fun

    .section .transform_bc_section, "ax", @progbits
    .align 1

    .globl transform_bc_fun
    .ent transform_bc_fun

transform_bc_fun:
    balc c
    balc c
    balc c
    lapc $a2, d
    lapc $a2, d + 4
    balc c

    .end transform_bc_fun
    .size transform_bc_fun, .-transform_bc_fun

.equ c, 0x000018
.equ d, 0x220000e
