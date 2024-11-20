# REQUIRES: nanomips

# RUN: llvm-mc -triple nanomips-elf -filetype=obj -mcpu i7200 %s -o %t.o
# RUN: ld.lld %t.o -o %t
# RUN: llvm-objdump -t %t | FileCheck %s --check-prefix=CHECK-NOSYMS
    

# RUN: llvm-mc -triple nanomips-elf --defsym eh=1 -filetype=obj \
# RUN: -mcpu i7200 %s -o %t.o

# RUN: ld.lld --section-start .text=0x1000 --section-start .eh_frame=0x2000 \
# RUN: %t.o -o %t
# RUN: llvm-objdump -ht %t | FileCheck %s

# RUN: llvm-mc -triple nanomips-elf --defsym eh=1 --defsym hdr=1 \
# RUN: -filetype=obj -mcpu i7200 %s -o %t.o

# RUN: ld.lld --section-start .text=0x1000 --section-start .eh_frame=0x2000 \
# RUN: --eh-frame-hdr --section-start .eh_frame_hdr=0x3000 %t.o -o %t

# RUN: llvm-objdump -ht %t | FileCheck %s --check-prefix=CHECK-HDR

    .section .text, "ax", @progbits
    # Dummy function to generate eh frame
    .cfi_startproc
    .align 1
    .globl _start
    .ent _start
_start:
    addiu $a1, $a2, 1

    .end _start
    .cfi_endproc
    .size _start, .-_start

    # If not referenced, eh frame start/end symbols are not added
    .ifdef eh
    .section .reference_start_end, "aw", @progbits
    # CHECK-NOSYMS-NOT: __eh_frame_start
    # CHECK-NOSYMS-NOT: __eh_frame_end
    # CHECK: .eh_frame            0000002c 00002000
    # CHECK: 00002000 {{.*}} __eh_frame_start
    # CHECK: 0000202c {{.*}} __eh_frame_end
    .4byte __eh_frame_start
    .4byte __eh_frame_end

    .ifdef hdr
    # CHECK-NOSYMS-NOT: __eh_frame_hdr_start
    # CHECK-NOSYMS-NOT: __eh_frame_hdr_end
    # CHECK-NOT: __eh_frame_hdr_start
    # CHECK-NOT: __eh_frame_hdr_end
    # CHECK-HDR: .eh_frame_hdr        00000014 00003000
    # CHECK-HDR: 00003000 {{.*}} __eh_frame_hdr_start
    # CHECK-HDR: 00003014 {{.*}} __eh_frame_hdr_end
    .4byte __eh_frame_hdr_start
    .4byte __eh_frame_hdr_end
    .endif
    .endif


