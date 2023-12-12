# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -EL -m32 -march=32r6 -msingle-float -mno-tlb -mcrc %s -o %t1.o
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -EL -m32 -march=32r6 -msingle-float -mno-tlb -mcrc %S/Inputs/nanomips-abiflags-section-sup.s -o %t2.o

# RUN: ld.lld --section-start .text=0x1000 --section-start .sdata=0x2000 \
# RUN: --section-start .nanoMIPS.abiflags=0x3000 %t1.o %t2.o -o %t

# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -s --section=.nanoMIPS.abiflags %t | FileCheck %s --check-prefix=CHECK-SAME

# CHECK-SAME: 3000 00002006 01020002 00000000 00800400
# CHECK-SAME-NEXT: 3010 00000000 00000000

# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -EL -m32 -march=32r6 -msingle-float -mtlb -mvirt -mmcu %S/Inputs/nanomips-abiflags-section-sup.s -o %t2.o

# RUN: ld.lld --section-start .text=0x1000 --section-start .sdata=0x2000 \
# RUN: --section-start .nanoMIPS.abiflags=0x3000 %t1.o %t2.o -o %t

# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump \
# RUN: -s --section=.nanoMIPS.abiflags %t | FileCheck %s --check-prefix=CHECK-DIFFERENT

# CHECK-DIFFERENT: 3000 00002006 01020002 00000000 09810400
# CHECK-DIFFERENT-NEXT: 3010 00000000 00000000

# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -EL -m32 -march=32r6 -mhard-float %S/Inputs/nanomips-abiflags-section-sup.s -o %t2.o

# RUN: ld.lld --section-start .text=0x1000 --section-start .sdata=0x2000 \
# RUN: --section-start .nanoMIPS.abiflags=0x3000 %t1.o %t2.o -o /dev/null 2>&1 | FileCheck %s --check-prefix=CHECK-FLOAT

# CHECK-FLOAT: warning: {{.*}} is incompatible with {{.*}}

    .section .text, "ax", @progbits
    .align 2
    .globl _start
    .ent _start

_start:
    li $a0, _start
    lw $a1, a
    jrc $a0
    .end _start
    .size _start, .-_start
