# The file testing ELF ABI versions for 64-bit time_t

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+nmips-64bit-time_t < %s \
# RUN:     | llvm-readelf -h - | FileCheck -check-prefix=CHECK-V1 %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=-nmips-64bit-time_t < %s \
# RUN:     | llvm-readelf -h - | FileCheck -check-prefix=CHECK-V0 %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf < %s \
# RUN:     | llvm-readelf -h - | FileCheck -check-prefix=CHECK-V0 %s

.text
	.globl __start
	.ent __start
__start:
	jr $ra
	.end __start
# CHECK-V0: ABI Version: 0
# CHECK-V1: ABI Version: 1
