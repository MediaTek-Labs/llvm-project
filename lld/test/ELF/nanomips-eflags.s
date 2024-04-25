# REQUIRES: nanomips
# Only 32bit little endian is tested, as it is the only one supported for now
# RUN: %nanomips-elf-as -m32 -EL -march=32r6 %s -o %t1.o
# RUN: %nanomips-elf-as -m32 -EL -march=32r6 %S/Inputs/nanomips-eflags-sup.s -o %t2.o
# RUN: ld.lld %t1.o %t2.o -o %t
# RUN: %nanomips-elf-objdump -p %t | FileCheck %s --check-prefix=CHECK-REGULAR

# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpic -mpcrel %s -o %t1.o
# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpic -mpid -mpcrel %S/Inputs/nanomips-eflags-sup.s -o %t2.o
# RUN: ld.lld -r %t1.o %t2.o -o %t
# RUN: %nanomips-elf-objdump -p %t | FileCheck %s --check-prefix=CHECK-RELOCATABLE

# RUN: %nanomips-elf-as -m32 -EL -march=64r6 -mpic -mpcrel %s -o %t1.o
# RUN: ld.lld -r %t1.o %t2.o -o %t
# RUN: %nanomips-elf-objdump -p %t | FileCheck %s --check-prefix=CHECK-32BITMODE

# CHECK-REGULAR: private flags = 00001000
# CHECK-RELOCATABLE: private flags = 00001013
# CHECK-32BITMODE: private flags = 10001017

    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    addiu $a1, $a2, 2
    balc fun

    .end _start
    .size _start, .-_start

