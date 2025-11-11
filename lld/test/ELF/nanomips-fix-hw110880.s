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


    
    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    # CHECK: e0bf {{.*}} aluipc $a1, %pcrel_hi(0x7{{.}}ff3
    # CHECK-NEXT: 80a5 0600 ori $a1, $a1, 0x600
    # CHECK-NO-FIX: 60a3 {{.*}} lapc.b $a1, {{.*}} <addr_fix>
    lapc $a1, addr_fix
    # CHECK-NEXT: e0df {{.*}} aluipc $a2, %pcrel_hi(0x7{{.}}ff3
    # CHECK-NEXT: 84a6 9606 sw $a1, 0x606($a2)
    # CHECK-NO-FIX-NEXT: 60af {{.*}} swpc $a1, {{.*}} <addr_fix+0x6>
    swpc $a1, addr_fix+6
    # CHECK-NEXT: 04bf fffe lapc.h $a1, 0x201012
    # CHECK-NO-FIX-NEXT: 60a3 0000 0020 lapc.b $a1, 0x201012
    lapc $a1, positive
    # CHECK-NEXT: 60c3 fffc ffdf lapc.b $a2, 0xffe01016
    # CHECK-NO-FIX-NEXT: 04c0 0001 lapc.h $a2, 0xffe01016
    lapc $a2, negative

    # CHECK-NEXT: 60a3 {{.*}} lapc.b $a1, 0x7{{.}}ff2600
    # CHECK-NO-FIX-NEXT: 60a0 {{.*}} li $a1,
    li $a1, addr_fix_abs
    # CHECK-NEXT: 60c3 {{.*}} lapc.b $a2, 0x7{{.}}ff2600
    # CHECK-NEXT: 20c5 2950 addu $a1, $a1, $a2
    # CHECK-NO-FIX-NEXT: 60a1 {{.*}} addiu $a1, $a1,
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
    # CHECK: 60e3 {{.*}} lapc.b $a3, 0x7{{.}}ff2600
    # CHECK-NO-FIX: 60e0 {{.*}} li $a3,
    li $a3, addr_fix_abs
    .end fun
    .size fun, .-fun

.equ positive, 0x201012
.equ negative, 0xffe01016
.equ first_transform, 0x20000001
