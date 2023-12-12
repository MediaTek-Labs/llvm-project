# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -EL -march=32r6 -m32 -mno-pcrel %s -o %t1.o
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-as \
# RUN: -EL -march=32r6 -m32 -mno-pcrel %S/Inputs/nanomips-abs-relocs-sup.s -o %t2.o
# RUN:  ld.lld --section-start .text=0x1000 --section-start .sdata=0x2001020 %t1.o %t2.o -o %t
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump -td %t | FileCheck %s
# RUN: /home/syrmia/Desktop/nanomips-gnu/nanomips-elf/2021.07-01/bin/nanomips-elf-objdump -d --section=.sdata %t | FileCheck %s --check-prefix CHECK-DATA-REL 

# CHECK: 0200102c {{.*}} c
# CHECK: 00000004 {{.*}} e_sup
# CHECK: 1000: 60e0 102c li a3,0x200102c
# CHECK-NEXT: 1004: 0200
# CHECK-NEXT: 1006: e0c0 1040 lui a2,%hi(0x2001000)
# CHECK-NEXT: 100a: 84c6 802c lw a2,44(a2)
# CHECK-DATA-REL:2001024 <d>:
# CHECK-DATA-REL-NEXT: 2001024: 2c 10 00 02
# CHECK-DATA-REL:2001028 <e>:
# CHECK-DATA-REL-NEXT: 2001028: 04 00

	.section .text, "ax", @progbits
	.align 2
	.globl _start
	.ent _start
	

_start:
	li $a3, b
	lui $a2, %hi(c)
	lw $a2, %lo(c)($a2)
	li $a4, -2
	.end _start
	.size _start, .-_start

	.section	.sdata,"aw",@progbits
	.align	2
	.globl a
	.type a, @object
	.size a, 4
	.globl b
	.type b, @object
	.size b, 4
	.globl d
	.size d, 4
	.type d, @object

	.globl e
	.size e, 2
	.type e, @object
a:
	.long 1
	.equ b, c
d:
	.long c
e:
	.2byte e_sup
