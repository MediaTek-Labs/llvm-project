# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mcpu=i7200 -mattr=+pcrel \
# RUN: %s -o %t.o

# RUN: ld.lld --relax --section-start .text=0x1000 --section-start \
# RUN: .fix_abs=0x7fff2600 --section-start .fix=0x7fff3600 \
# RUN: --fix-nmips-hw110880 --expand-reg 6 %t.o -o %t

# RUN: llvm-objdump -d --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s 

# RUN: ld.lld --relax --section-start .text=0x1000 --section-start \
# RUN: .fix_abs=0x79ff2600 --section-start .fix=0x79ff3600 \
# RUN: --fix-nmips-hw110880 --expand-reg 6 %t.o -o %t

# RUN: llvm-objdump -d --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s

# RUN: ld.lld --relax --section-start .text=0x1000 --section-start \
# RUN: .fix_abs=0x78ff2600 --section-start .fix=0x78ff3600 \
# RUN: --fix-nmips-hw110880 --expand-reg 6 %t.o -o %t

# RUN: llvm-objdump -d --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s \
# RUN: --check-prefix=CHECK-NO-FIX

# RUN: ld.lld --relax --section-start .text=0x1000 --section-start \
# RUN: .fix_abs=0x7fff2900 --section-start .fix=0x7fff3900 \
# RUN: --fix-nmips-hw110880 --expand-reg 6 %t.o -o %t

# RUN: llvm-objdump -d --triple=nanomips-elf --mcpu=i7200 %t | FileCheck %s \
# RUN: --check-prefix=CHECK-NO-FIX

# CHECK: bf e0 {{.*}} aluipc $a1, %pcrel_hi(0x7{{.}}ff3
# CHECK-NEXT: a5 80 00 06 ori $a1, $a1, 0x600
# CHECK-NEXT: df e0 {{.*}} aluipc $a2, %pcrel_hi(0x7{{.}}ff3
# CHECK-NEXT: a6 84 06 96 sw $a1, 0x606($a2)
# CHECK-NEXT: bf 04 fe ff lapc.h $a1, 0x201012
# CHECK-NEXT: c0 04 01 00 lapc.h $a2, 0xffe01018
# CHECK-NEXT: a3 60 {{.*}} lapc.b $a1, 0x7{{.}}ff2600
# CHECK-NEXT: c3 60 {{.*}} lapc.b $a2, 0x7{{.}}ff2600
# CHECK-NEXT: c5 20 50 29 addu $a1, $a1, $a2
# CHECK: e3 60 {{.*}} lapc.b $a3, 0x7{{.}}ff2600

# CHECK-NO-FIX: a3 60 {{.*}} lapc.b $a1, {{.*}} <addr_fix>
# CHECK-NO-FIX-NEXT: af 60 {{.*}} swpc $a1, {{.*}} <addr_fix+0x6>
# CHECK-NO-FIX-NEXT: a3 60 00 00 20 00 lapc.b $a1, 0x201012
# CHECK-NO-FIX-NEXT: c0 04 03 00 lapc.h $a2, 0xffe01018
# CHECK-NO-FIX-NEXT: a0 60 {{.*}} li $a1,
# CHECK-NO-FIX-NEXT: a1 60 {{.*}} addiu $a1, $a1,
# CHECK-NO-FIX: e0 60 {{.*}} li $a3,

    
    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    lapc $a1, addr_fix
    swpc $a1, addr_fix+6
    lapc $a1, far_positive
    lapc $a2, negative

    li $a1, addr_fix_abs
    addiu $a1, addr_fix_abs

    .end _start
    .size _start, .-_start

    .section .fix_abs, "ax", @progbits
    .align 1

addr_fix_abs:

    .section .fix, "ax", @progbits
    .align 1

addr_fix:

    .section first_transform_no_byte_count_change, "ax", @progbits
    .align 1

    .globl fun
    .ent fun

fun:
    # Test for big obj files
    .skip 0x7e0000
    li $a3, addr_fix_abs
    .end fun
    .size fun, .-fun

.equ far_positive, 0x201012
.equ negative, 0xffe01018
.equ first_transform, 0x20000001
