# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -mcpu=i7200 -triple=nanomips-elf %s -o %t.o

# RUN: ld.lld --nanomips-custom-linker-script-type \
# RUN: -T %S/Inputs/nanomips-overlay-section.ld %t.o -o %t

# RUN: llvm-objdump -h %t | FileCheck %s

# FIXME: For some reason LMAs seem to be the same among sections in same
# overlay, this isn't the case when inspecting with gnu's objdump.
# Update the tests with LMAs when fixed


# CHECK: .overlay1_sec1 0000000c 00010000
# CHECK: .overlay1_sec2 00000004 00010000
# CHECK: .overlay2_sec1 00000004 0001000c
# CHECK: .overlay2_sec2 00000008 0001000c
# CHECK: .overlay3_sec1 00000004 00010014
# CHECK: .overlay3_sec2 00000004 00010014
# CHECK: .text 00000004 00010018

    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start
_start:
    addiu $a1, $a2, 1
    
    .end _start
    .size _start, .-_start

    .section .overlay1_sec1, "ax", @progbits
    .align 1
    .globl overlay1_sec1
    .ent overlay1_sec1
overlay1_sec1:
    addiu $a1, $a2, 1
    addiu $a1, $a2, 1

    .end overlay1_sec1
    .size overlay1_sec1, .-overlay1_sec1

    .section .overlay1_sec2, "ax", @progbits
    .align 1
    .globl overlay1_sec2
    .ent overlay1_sec2

overlay1_sec2:
    addiu $a1, $a2, 1

    .end overlay1_sec2
    .size overlay1_sec2, .-overlay1_sec2

    .section .overlay2_sec1, "ax", @progbits
    .align 1
    .globl overlay2_sec1
    .ent overlay2_sec1

overlay2_sec1:
    addiu $a1, $a2, 1

    .end overlay2_sec1
    .size overlay2_sec1, .-overlay2_sec1

    .section .overlay2_sec2, "ax", @progbits
    .align 1
    .globl overlay2_sec2
    .ent overlay2_sec2

overlay2_sec2:
    addiu $a1, $a2, 1
    addiu $a1, $a2, 1

    .end overlay2_sec2
    .size overlay2_sec2, .-overlay2_sec2


    .section .overlay3_sec1, "ax", @progbits
    .align 1
    .globl overlay3_sec1
    .ent overlay3_sec1
overlay3_sec1:
    addiu $a1, $a2, 1

    .end overlay3_sec1
    .size overlay3_sec1, .-overlay3_sec1

    .section .overlay3_sec2, "ax", @progbits
    .align 1
    .globl overlay3_sec2
    .ent overlay3_sec2
overlay3_sec2:
    addiu $a1, $a2, 1

    .end overlay3_sec2
    .size overlay3_sec2, .-overlay3_sec2