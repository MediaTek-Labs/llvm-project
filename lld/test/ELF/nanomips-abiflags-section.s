# REQUIRES: nanomips

# Note: This test may need some changing in the future, as not all flags are
# available for llvm-mc. Other types than soft-float and mcu options are an 
# example. 

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 \
# RUN: -mattr=+soft-float,-tlb,+crc %s -o %t1.o

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 \
# RUN: -mattr=+soft-float,-tlb,+crc %S/Inputs/nanomips-abiflags-section-sup.s \
# RUN: -o %t2.o

# RUN: ld.lld %t1.o %t2.o -o %t

# RUN: llvm-objdump --triple=nanomips-elf --mcpu=i7200 -s \
# RUN: --section=.nanoMIPS.abiflags %t | FileCheck \
# RUN: --check-prefix=CHECK-SAME-FLAGS %s

# CHECK-SAME-FLAGS: 00002006 01000003 00000000 40800400
# Note: Next line differs from gnu's as (it generates all zeroes)
# CHECK-SAME-FLAGS-NEXT: 01000000 00000000

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 \
# RUN: -mattr=+tlb,+virt %s -o %t1.o

# RUN: ld.lld %t1.o %t2.o -o %t

# RUN: llvm-objdump --triple=nanomips-elf --mcpu=i7200 -s \
# RUN: --section=.nanoMIPS.abiflags %t | FileCheck \
# RUN: --check-prefix=CHECK-DIFFERENT-FLAGS %s

# CHECK-DIFFERENT-FLAGS: 00002006 01010003 00000000 41810400
# CHECK-DIFFERENT-FLAGS-NEXT: 01000000 00000000

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu i7200 \
# RUN: %s -o %t1.o

# RUN: llvm-objcopy --remove-section=.nanoMIPS.abiflags %t1.o

# RUN: ld.lld %t1.o %t2.o -o %t 2>&1 | FileCheck %s \
# RUN: --check-prefix=EFLAGS

# EFLAGS: Inherited abiflags from e_flags instead of .nanoMIPS.abiflags

# RUN: llvm-objdump --triple=nanomips-elf --mcpu=i7200 -s \
# RUN: --section=.nanoMIPS.abiflags %t | FileCheck \
# RUN: --check-prefix=CHECK-EFLAGS %s

# CHECK-EFLAGS: 00002006 01000003 00000000 40800400
# CHECK-EFLAGS-NEXT: 01000000 00000000



    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    balc fun

    .end _start
    .size _start, .-_start
