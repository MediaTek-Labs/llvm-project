# RUN: llvm-mc %s -triple=nanomips-elf -show-encoding -show-inst 2> %t0 | \
# RUN:	FileCheck --check-prefixes CHECK,NOPCREL %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s -o - | \
# RUN:   llvm-objdump --no-print-imm-hex --triple nanomips-elf -dr - | \
# RUN:	FileCheck --check-prefixes DISAS,DISAS-NOPCREL %s
# RUN: llvm-mc %s --defsym=PCREL=1 -triple=nanomips-elf -show-encoding -show-inst 2> %t0 | \
# RUN:	FileCheck --check-prefixes CHECK,PCREL %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s --defsym=PCREL=1 -o - | \
# RUN:   llvm-objdump --no-print-imm-hex --triple nanomips-elf -dr - | \
# RUN:	FileCheck --check-prefixes DISAS,DISAS-PCREL %s
	.text
	# CHECK: .text
.ifdef PCREL
	.module pcrel
.endif
	
	la	$a0, test	# NOPCREL: li $a0, test	# encoding: [0x80,0x60,A,A,A,A]
				# NOPCREL-NEXT: fixup A - offset: 2, value: test, kind: fixup_NANOMIPS_I32
				# NOPCREL-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS-NOPCREL: {{.*}}  80 60 00 00 00 00    	li	$a0, 0
				# DISAS-NOPCREL-NEXT: {{.*}}  R_NANOMIPS_I32	test
				# PCREL: lapc.b $a0, test # encoding: [0x83,0x60,A,A,A,A]
				# PCREL-NEXT: fixup A - offset: 2, value: test, kind: fixup_NANOMIPS_PC_I32
				# PCREL-NEXT: <MCInst #{{.*}} LAPC48_NM
				# DISAS-PCREL: {{.*}}  83 60 00 00 00 00    	lapc.b	$a0, 0x{{.*}}
				# DISAS-PCREL-NEXT: {{.*}}  R_NANOMIPS_PC_I32	test

	la	$a0, test+4	# NOPCREL: li $a0, test+4 # encoding: [0x80,0x60,A,A,A,A]
				# NOPCREL-NEXT: fixup A - offset: 2, value: test+4, kind: fixup_NANOMIPS_I32
				# NOPCREL-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS-NOPCREL: {{.*}}  80 60 00 00 00 00    	li	$a0, 0
				# DISAS-NOPCREL-NEXT: {{.*}}  R_NANOMIPS_I32	test
				# PCREL: lapc.b $a0, test+4 # encoding: [0x83,0x60,A,A,A,A]
				# PCREL-NEXT: fixup A - offset: 2, value: test+4, kind: fixup_NANOMIPS_PC_I32
				# PCREL-NEXT: <MCInst #{{.*}} LAPC48_NM
				# DISAS-PCREL: {{.*}}  83 60 00 00 00 00    	lapc.b	$a0, 0x{{.*}}
				# DISAS-PCREL-NEXT: {{.*}}  R_NANOMIPS_PC_I32	test

	la	$t3, 65536	# CHECK: lui $t3, %hi(0x10000) # encoding: [0xe1,0xe1,0x00,0x00]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
				# DISAS: {{.*}}  e1 e1 00 00  	lui	$t3, %hi(0x10000)
	la	$a1, 64		# CHECK: li $a1, 64	# encoding: [0xc0,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  c0 d2        	li	$a1, 64
	la	$a5, 65535	# CHECK: li $a5, 65535	# encoding: [0x20,0x01,0xff,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  20 01 ff ff  	li	$a5, 65535
	la	$s7, -4095	# CHECK: li $s7, -4095	# encoding: [0xe0,0x82,0xff,0x8f]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  e0 82 ff 8f  	li	$s7, -4095
	la	$t4, -4097	# CHECK: li $t4, 0xffffefff	# encoding: [0x40,0x60,0xff,0xef,0xff,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS: {{.*}}  40 60 ff ef ff ff    	li	$t4, 0xffffefff
	jrc $ra
	.type   g_8,@object
	.comm   g_8,16,16
