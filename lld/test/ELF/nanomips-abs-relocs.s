# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=-pcrel %s -o %t1.o
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=-pcrel %S/Inputs/nanomips-abs-relocs-sup.s -o %t2.o
# RUN:  ld.lld --section-start .text=0x1000 --section-start .sdata=0x2001020 %t1.o %t2.o -o %t
# RUN: llvm-objdump --triple nanomips-elf -td %t | FileCheck %s
# RUN: llvm-objdump --triple nanomips-elf -s --section=.sdata %t | FileCheck %s --check-prefix CHECK-DATA-REL 

# CHECK: 0200102a {{.*}} b
# CHECK: 00000004 {{.*}} d_sup
# CHECK: 1000: e0 60 2a 10 00 02 li $a3, 0x200102a
# CHECK-NEXT: 1006: c0 e0 40 10 lui $a2, %hi(0x2001000)
# CHECK-NEXT: 100a: c6 84 2a 80 lw $a2, 0x2a($a2)
# CHECK-DATA-REL: 2001020 01000000 2a100002 04000300

	.section .text, "ax", @progbits
	.align 1
	.globl _start
	.ent _start
	

_start:
	# R_NANOMIPS_I32
	li $a3, b
	# R_NANOMIPS_HI20
	lui $a2, %hi(b)
	# R_NANOMIPS_LO12
	lw $a2, %lo(b)($a2)
	li $a4, -2
	.end _start
	.size _start, .-_start

	.section	.sdata,"aw",@progbits
	.align	1

	.long 1
	# R_NANOMIPS_32
	.equ a, b
c:
	.long b
d:
	# R_NANOMIPS_UNSIGNED_16
	.2byte d_sup
