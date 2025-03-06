# Testing instructions mis-aligned due to embedded odd-sized data in
# instruction stream

# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s -o - \
# RUN:     | llvm-objdump --triple nanomips-elf -dr - | FileCheck %s

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

# CHECK: 2: a1 60 78 56 34 12    	addiu	$a1, $a1, 0x12345678
# CHECK: c: a0 60 21 43 65 87    	li	$a1, 0x87654321
# CHECK: 16: a3 60 48 61 bc 00    	lapc.b	$a1, 0xbc6164
# CHECK: 22: 00 80 00 c0  	nop32
