# Testing instructions mis-aligned due to embedded odd-sized data in
# instruction stream

# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s -o - \
# RUN:     | llvm-objdump --triple nanomips-elf -dr - | FileCheck %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf --defsym=RELAX=1  %s -o - \
# RUN:     | llvm-objdump --triple nanomips-elf -dr - | FileCheck %s

.ifdef RELAX
	.linkrelax
.endif

.text
	.globl __start
	.ent __start
__start:
	.asciz "a"
	.align 1
a:
	addiu $a1, 0x12345678
	.asciz "aa"
	.align 1
b:
	li $a1, 0x87654321
	.asciz "aaa"
	.align 1
c:
	addiupc $a1,12345678
	.asciz "aaaa"
	.align 1
d:
	nop32
	.end __start

# CHECK: 2: 60a1 5678 1234    	addiu	$a1, $a1, 0x12345678
# CHECK: c: 60a0 4321 8765    	li	$a1, 0x87654321
# CHECK: 16: 60a3 6148 00bc    	lapc.b	$a1, 0xbc6164
# CHECK: 22: 8000 c000  	nop32
