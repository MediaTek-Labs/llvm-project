# REQUIRES: nanomips

# Only 32bit little endian is tested, as it is the only one supported for now
# Need to test options like pic and pid as well
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 %s -o %t1.o

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 \
# RUN: %S/Inputs/nanomips-eflags-sup.s -o %t2.o

# RUN: ld.lld %t1.o %t2.o -o %t

# RUN: llvm-readelf --file-header %t | FileCheck %s --check-prefix=CHECK-REGULAR

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 -mattr=+pcrel \
# RUN: %s -o %t1.o

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 \
# RUN: -mattr=+pcrel %S/Inputs/nanomips-eflags-sup.s -o %t2.o

# RUN: llvm-readelf --file-header %t1.o | FileCheck %s --check-prefix=CHECK-PCREL

# RUN: ld.lld -r %t1.o %t2.o -o %t
# RUN llvm-readelf --file-header %t | FileCheck %s --check-prefix=CHECK-RELOCATABLE


# CHECK-REGULAR: Flags: 0x1000
# CHECK-PCREL: Flags: 0x1010
# CHECK-RELOCATABLE: Flags: 0x1000



    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    addiu $a1, $a2, 1
    balc fun

    .end _start
    .size _start, .-_start

