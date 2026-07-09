# Check OS/ABI field in ELF header

# RUN: llvm-mc -filetype=obj -triple mipsel < %s \
# RUN:     | llvm-readelf -h - | FileCheck -check-prefix=CHECK-MIPS %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf  < %s \
# RUN:     | llvm-readelf -h - | FileCheck -check-prefix=CHECK-NMIPS %s

.text
	.globl __start
	.ent __start
__start:
	jr $ra
	.end __start
# CHECK-MIPS: OS/ABI: UNIX - System V
# CHECK-NMIPS: OS/ABI: Standalone App
