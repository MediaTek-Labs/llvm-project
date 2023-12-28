# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -m32 -EL -march=32r6 %s -o %t.o
# RUN: ld.lld --relax %t.o -o %t
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -d %t | FileCheck %s

# CHECK: 8965{{.*}} bgec {{.*}} <fun>
# CHECK: dbe{{.*}} beqc {{.*}} <fun>
# CHECK: dbe{{.*}} beqc {{.*}} <fun>
# CHECK: 8905{{.*}} bgec {{.*}} <fun>

# Starting testcase of nanomips relaxations, needs to convert
# 32bit beqc to 16 bit beqc
    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    lapc $a3, fun
#    bgec $a1, $a7, fun
#    beqc $a2, $a3, fun
a:
#    beqc $a3, $a2, fun
#    bgec $a1, $a4, fun
#    add $a1, $a1, $a1
#    add $a2, $a3, $a1
    .end _start
    .size _start, .-_start

    .section .other_text, "ax", @progbits
    .align 1
    .globl fun
    .ent fun
fun:
    add $a3, $a2, $a1
#    beqc $zero, $a2, a

    .end fun
    .size fun, .-fun
