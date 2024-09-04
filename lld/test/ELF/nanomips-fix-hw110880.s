# llvm-objdump not good symbol shown
# at disassembly for lapc (instead of far_positive,
# _start + imm)
# REQUIRES: nanomips, nanomips-gnu

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+pcrel %s -o %t.o
# RUN: ld.lld --relax --section-start .text=0x1000 --section-start \
# RUN: .fix_abs=0x7fff2600 --section-start .fix=0x7fff3600 \
# RUN: --fix-nmips-hw110880 --expand-reg 6 %t.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck %s

# RUN: ld.lld --relax --section-start .text=0x1000 --section-start \
# RUN: .fix_abs=0x79ff2600 --section-start .fix=0x79ff3600 \
# RUN: --fix-nmips-hw110880 --expand-reg 6 %t.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck %s

# RUN: ld.lld --relax --section-start .text=0x1000 --section-start \
# RUN: .fix_abs=0x78ff2600 --section-start .fix=0x78ff3600 \
# RUN: --fix-nmips-hw110880 --expand-reg 6 %t.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck --check-prefix=CHECK-NO-FIX %s

# RUN: ld.lld --relax --section-start .text=0x1000 --section-start \
# RUN: .fix_abs=0x7fff2900 --section-start .fix=0x7fff3900 \
# RUN: --fix-nmips-hw110880 --expand-reg 6 %t.o -o %t

# RUN: %nanomips-elf-objdump -d %t | FileCheck --check-prefix=CHECK-NO-FIX %s

# CHECK: e0b{{.*}} aluipc a1,0x7{{.}}ff2
# CHECK-NEXT: 80a5{{.*}} ori a1,a1,0x600
# CHECK-NEXT: e0d{{.*}} aluipc a2,0x7{{.}}ff2
# CHECK-NEXT: 84a6{{.*}} sw a1,1542(a2)
# CHECK-NEXT: 04b{{.*}} lapc {{.*}} <far_positive>
# CHECK-NEXT: 04c0 0001 lapc {{.*}} <far_negative>
# CHECK-NEXT: 60a3{{.*}} lapc a1,{{.*}} <addr_fix_abs>
# CHECK: 60c3{{.*}} lapc a2,{{.*}} <addr_fix_abs>
# CHECK: 20{{.*}} addu a1,a1,a2
# CHECK: 60e3{{.*}} lapc a3,{{.*}}

# CHECK-NO-FIX: 60a3{{.*}} lapc {{.*}} <addr_fix>
# CHECK-NO-FIX: 60af{{.*}} swpc {{.*}} <addr_fix+0x6>
# CHECK-NO-FIX: 60a3{{.*}} lapc {{.*}} <far_positive>
# CHECK-NO-FIX: 04c{{.*}} lapc {{.*}} <far_negative>
# CHECK-NO-FIX: 60a0{{.*}} li a1,{{.*}}
# CHECK-NO-FIX: 60a1{{.*}} addiu a1,a1,{{.*}}
# CHECK-NO-FIX: 60e0{{.*}} li a3,{{.*}}

# expand R_NANOMIPS_PC_I32
# also tests relax R_NANOMIPS_PC_I32


    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    lapc $a1, addr_fix 
    swpc $a1, addr_fix+6
    lapc $a1, far_positive
    lapc $a2, far_negative

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

    .section .first_transform_no_byte_count_change, "ax", @progbits
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
.equ far_negative, 0xffe01018
.equ first_transform, 0x20000001

