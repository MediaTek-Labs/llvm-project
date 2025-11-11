# Instructions that are valid using with architectural suffixes
#
# RUN: llvm-mc %s -triple=nanomips-elf -show-encoding -show-inst 2> %t0 | FileCheck %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s -o - \
# RUN:   | llvm-objdump --no-print-imm-hex --triple nanomips-elf --nmips-little-endian-order -dr - \
# RUN:   | FileCheck --check-prefixes DISAS %s
	.text
	# CHECK: .text
	.set noat
	# reg3-reg3 arithmetic, 16-bit
	addu[16] $a1, $s2, $a3	# CHECK: addu	$a1, $s2, $a3	# encoding: [0xaa,0xb3]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu16_NM
				# DISAS: {{.*}}: aa b3        addu $a1, $s2, $a3
	subu[16] $a0, $a2, $a3	# CHECK: subu	$a0, $a2, $a3	# encoding: [0xe9,0xb3]
				# CHECK-NEXT: # <MCInst #{{.*}} SUBu16_NM
				# DISAS: {{.*}}: e9 b3	subu $a0, $a2, $a3

	# reg4-reg4 arithmetic, 16-bit
	addu[4x4] $a1, $a1, $a7	# CHECK: addu $a1, $a1, $a7	# encoding: [0xa3,0x3c]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu4x4_NM
				# DISAS: {{.*}}: a3 3c	addu $a1, $a1, $a7
	mul[4x4] $s2, $s2, $a3	# CHECK: mul $s2, $s2, $a3	# encoding: [0x4f,0x3e]
				# CHECK-NEXT: # <MCInst #{{.*}} MUL4x4_NM
				# DISAS: {{.*}}: 4f 3e	mul $s2, $s2, $a3

	# reg-reg arithmetic, 32-bit
	addu[32] $a1, $s2, $t3	# CHECK: addu $a1, $s2, $t3	# encoding: [0xf2,0x21,0x50,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu_NM
				# DISAS: {{.*}}: f2 21 50 29  	addu	$a1, $s2, $t3
	subu[32] $a0, $a2, $t3	# CHECK: subu $a0, $a2, $t3	# encoding: [0xe6,0x21,0xd0,0x21]
				# CHECK-NEXT: # <MCInst #{{.*}} SUBu_NM
				# DISAS: {{.*}}: e6 21 d0 21  	subu	$a0, $a2, $t
	mul[32]	$t1, $s2, $a3	# CHECK: mul $t1, $s2, $a3	# encoding: [0xf2,0x20,0x18,0x68]
				# CHECK-NEXT: # <MCInst #{{.*}} MUL_NM
				# DISAS: {{.*}}: f2 20 18 68  	mul	$t1, $s2, $a3

	# reg-reg logic, 16-bit
	and[16]	$a1, $a3, $a1	# CHECK: and $a1, $a3, $a1	# encoding: [0xf8,0x52]
				# CHECK-NEXT: # <MCInst #{{.*}} AND16_NM
				# DISAS: {{.*}}  f8 52        	and	$a3, $a1, $a3
	or[16]	$s2, $a3, $s2	# CHECK: or $s2, $a3, $s2	# encoding: [0x7c,0x51]
				# CHECK-NEXT: # <MCInst #{{.*}} OR16_NM
				# DISAS: {{.*}}  7c 51        	or	$a3, $s2, $a3
	xor[16]	$a3, $s3, $a3	# CHECK: xor $a3, $s3, $a3	# encoding: [0xb4,0x53]
				# CHECK-NEXT: # <MCInst #{{.*}} XOR16_NM
				# DISAS: {{.*}}  b4 53        	xor	$s3, $a3, $s3
	not[16]	$a3, $s3	# CHECK: not $a3, $s3		# encoding: [0xb0,0x53]
				# CHECK-NEXT: # <MCInst #{{.*}} NOT16_NM
				# DISAS: {{.*}}  b0 53        	not	$a3, $s3
	
	# reg-reg logic, 32-bit
	and[32]	$a1, $s4, $a3	# CHECK: and $a1, $s4, $a3	# encoding: [0xf4,0x20,0x50,0x2a]
				# CHECK-NEXT: # <MCInst #{{.*}} AND_NM
				# DISAS: {{.*}}  f4 20 50 2a  	and	$a1, $s4, $a3
	or[32]	$a1, $t2, $a3	# CHECK: or $a1, $t2, $a3	# encoding: [0xee,0x20,0x90,0x2a]
				# CHECK-NEXT: # <MCInst #{{.*}} OR_NM
				# DISAS: {{.*}}  ee 20 90 2a  	or	$a1, $t2, $a3
	xor[32]	$t0, $s2, $a3	# CHECK: xor $t0, $s2, $a3	# encoding: [0xf2,0x20,0x10,0x63]
				# CHECK-NEXT: # <MCInst #{{.*}} XOR_NM
				# DISAS: {{.*}}  f2 20 10 63  	xor	$t0, $s2, $a3
	
	# reg-imm logic, 16-bit
	sll[16]	$a1, $s2, 1	# CHECK: sll $a1, $s2, 1	# encoding: [0xa1,0x32]
				# CHECK-NEXT: # <MCInst #{{.*}} SLL16_NM
				# DISAS: {{.*}}  a1 32        	sll	$a1, $s2, 1
	srl[16]	$s1, $a3, 8	# CHECK: srl $s1, $a3, 8	# encoding: [0xf8,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SRL16_NM
				# DISAS: {{.*}}  f8 30        	srl	$s1, $a3, 8
	andi[16] $a0, $a1, 0	# CHECK: andi $a0, $a1, 0x0	# encoding: [0x50,0xf2]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  50 f2        	andi	$a0, $a1, 0x0
	andi[16] $a1, $a2, 1	# CHECK: andi $a1, $a2, 0x1	# encoding: [0xe1,0xf2]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  e1 f2        	andi	$a1, $a2, 0x1
	andi[16] $a2, $a3, 6	# CHECK: andi $a2, $a3, 0x6	# encoding: [0x76,0xf3]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  76 f3        	andi	$a2, $a3, 0x6
	andi[16] $a3, $s0, 0xff	# CHECK: andi $a3, $s0, 0xff	# encoding: [0x8c,0xf3]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  8c f3        	andi	$a3, $s0, 0xff
	andi[16] $s1, $s3, 0xffff	# CHECK: andi $s1, $s3, 0xffff	# encoding: [0xbd,0xf0]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  bd f0        	andi	$s1, $s3, 0xffff
	andi[16] $s3, $a3, 0xe	# CHECK: andi $s3, $a3, 0xe	# encoding: [0xfe,0xf1]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  fe f1        	andi	$s3, $a3, 0xe
	andi[16] $s2, $a1, 0xf	# CHECK: andi $s2, $a1, 0xf	# encoding: [0x5f,0xf1]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  5f f1        	andi	$s2, $a1, 0xf
	
	# reg-imm logic, 32-bit
	andi[32] $a4, $a1, 0	# CHECK: andi $a4, $a1, 0x0	# encoding: [0x05,0x81,0x00,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  05 81 00 20  	andi	$a4, $a1, 0x0
	andi[32] $a1, $t2, 1	# CHECK: andi $a1, $t2, 0x1	# encoding: [0xae,0x80,0x01,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  ae 80 01 20  	andi	$a1, $t2, 0x1
	andi[32] $a2, $t3, 6	# CHECK: andi $a2, $t3, 0x6	# encoding: [0xcf,0x80,0x06,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  cf 80 06 20  	andi	$a2, $t3, 0x6
	andi[32] $a6, $s0, 0xff	# CHECK: andi $a6, $s0, 0xff	# encoding: [0x50,0x81,0xff,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  50 81 ff 20  	andi	$a6, $s0, 0xff
	andi[32] $s1, $s4, 0xfff	# CHECK: andi $s1, $s4, 0xfff	# encoding: [0x34,0x82,0xff,0x2f]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  34 82 ff 2f  	andi	$s1, $s4, 0xfff
	andi[32] $s3, $a4, 0xe	# CHECK: andi $s3, $a4, 0xe	# encoding: [0x68,0x82,0x0e,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  68 82 0e 20  	andi	$s3, $a4, 0xe
	andi[32] $s2, $t0, 0xf	# CHECK: andi $s2, $t0, 0xf	# encoding: [0x4c,0x82,0x0f,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  4c 82 0f 20  	andi	$s2, $t0, 0xf
	andi[32] $a3, $s3, 12	# CHECK: andi $a3, $s3, 0xc	# encoding: [0xf3,0x80,0x0c,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  f3 80 0c 20  	andi	$a3, $s3, 0xc
	andi[32] $a1, $s2, 13	# CHECK: andi $a1, $s2, 0xd	# encoding: [0xb2,0x80,0x0d,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  b2 80 0d 20  	andi	$a1, $s2, 0xd
	andi[32] $a2, $s1, 16	# CHECK: andi $a2, $s1, 0x10	# encoding: [0xd1,0x80,0x10,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  d1 80 10 20  	andi	$a2, $s1, 0x10
	
	# reg-imm bit-wise, 32-bit
	sll[32]	$a1, $s2, 9	# CHECK: sll $a1, $s2, 9	# encoding: [0xb2,0x80,0x09,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SLL_NM
				# DISAS: {{.*}}  b2 80 09 c0  	sll	$a1, $s2, 9
	sll[32]	$a1, $t2, 1	# CHECK: sll $a1, $t2, 1	# encoding: [0xae,0x80,0x01,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SLL_NM
				# DISAS: {{.*}}  ae 80 01 c0  	sll	$a1, $t2, 1
	srl[32]	$a1, $a2, 31	# CHECK: srl $a1, $a2, 31	# encoding: [0xa6,0x80,0x5f,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SRL_NM
				# DISAS: {{.*}}  a6 80 5f c0  	srl	$a1, $a2, 31
	srl[32]	$a1, $a7, 8	# CHECK: srl $a1, $a7, 8	# encoding: [0xab,0x80,0x48,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SRL_NM
				# DISAS: {{.*}}  ab 80 48 c0  	srl	$a1, $a7, 8

	# paired moves
	movep[rev] $s0, $s1, $a0, $a1	# CHECK: movep $s0, $s1, $a0, $a1	# encoding: [0x30,0xfe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
				# DISAS: {{.*}}  30 fe        	movep	$s0, $s1, $a0, $a1
	movep[rev] $a7, $s7, $a1, $a2	# CHECK: movep $a7, $s7, $a1, $a2	# encoding: [0xe3,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
				# DISAS: {{.*}}  e3 ff        	movep	$a7, $s7, $a1, $a2
	movep[rev] $a1, $s3, $a2, $a3	# CHECK: movep $a1, $s3, $a2, $a3	# encoding: [0x6d,0xfe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
				# DISAS: {{.*}}  6d fe        	movep	$a1, $s3, $a2, $a3
	movep[rev] $a6, $s0, $a3, $a4	# CHECK: movep $a6, $s0, $a3, $a4	# encoding: [0x0a,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
				# DISAS: {{.*}}  0a ff        	movep	$a6, $s0, $a3, $a4
	
	li[16]	$a0, 1		# CHECK: li $a0, 1	# encoding: [0x01,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  01 d2        	li	$a0, 1
	li[16]	$a1, 64		# CHECK: li $a1, 64	# encoding: [0xc0,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  c0 d2        	li	$a1, 64
	li[16]	$a3, 0		# CHECK: li $a3, 0	# encoding: [0x80,0xd3]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  80 d3        	li	$a3, 0
	li[16]	$s0, 126	# CHECK: li $s0, 126	# encoding: [0x7e,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  7e d0        	li	$s0, 126
	li[16]	$s1, -1		# CHECK: li $s1, -1	# encoding: [0xff,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  ff d0        	li	$s1, -1
	li[48]	$t0, 65537	# CHECK: li $t0, 0x10001	# encoding: [0x80,0x61,0x01,0x00,0x01,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS: {{.*}}  80 61 01 00 01 00    	li	$t0, 0x10001
	li[48]	$t1, 2147483647	# CHECK: li $t1,  0x7fffffff	# encoding: [0xa0,0x61,0xff,0xff,0xff,0x7f]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS: {{.*}}  a0 61 ff ff ff 7f    	li	$t1, 0x7fffffff
	li[48]	$t4, -4097	# CHECK: li $t4, 0xffffefff	# encoding: [0x40,0x60,0xff,0xef,0xff,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS: {{.*}}  40 60 ff ef ff ff    	li	$t4, 0xffffefff
	li[48]	$t5, -2147483647 # CHECK: li $t5, 0x80000001	# encoding: [0x60,0x60,0x01,0x00,0x00,0x80]
				 # CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				 # DISAS: {{.*}}  60 60 01 00 00 80    	li	$t5, 0x80000001
	
	nop[32]		# CHECK: nop32	# encoding: [0x00,0x80,0x00,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} NOP32_NM
			# DISAS: {{.*}}  00 80 00 c0  	nop32
	sdbbp[16] 0	# CHECK: sdbbp 0x0 # encoding: [0x18,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP16_NM
			# DISAS: {{.*}}  18 10        	sdbbp 0x0
	sdbbp[16] 7	# CHECK: sdbbp	0x7 # encoding: [0x1f,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP16_NM
			# DISAS: {{.*}}  1f 10        	sdbbp	0x7
	sdbbp[32] 8	# CHECK: sdbbp	0x8 # encoding: [0x18,0x00,0x08,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP_NM
			# DISAS: {{.*}}  18 00 08 00  	sdbbp	0x8
	sdbbp[32] 0x7ffff	# CHECK: sdbbp	0x7ffff # encoding: [0x1f,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP_NM
			# DISAS: {{.*}}  1f 00 ff ff  	sdbbp	0x7ffff
	break[16] 0	# CHECK: break	0x0 # encoding: [0x10,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK16_NM
			# DISAS: {{.*}}  10 10        	break	0x0
	break[16] 7	# CHECK: break	0x7 # encoding: [0x17,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK16_NM
			# DISAS: {{.*}}  17 10        	break	0x7
	break[32]	# CHECK: break	0x0 # encoding: [0x10,0x00,0x00,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK_NM
			# DISAS: {{.*}}  10 00 00 00  	break	0x0
	break[32] 8	# CHECK: break	0x8 # encoding: [0x10,0x00,0x08,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK_NM
			# DISAS: {{.*}}  10 00 08 00  	break	0x8
	break[32] 0x7ffff	# CHECK: break	0x7ffff # encoding: [0x17,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK_NM
			# DISAS: {{.*}}  17 00 ff ff  	break	0x7ffff
	syscall[16] 0	# CHECK: syscall 0x0 # encoding: [0x08,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL16_NM
			# DISAS: {{.*}}  08 10        	syscall	0x0
	syscall[16] 3	# CHECK: syscall 0x3 # encoding: [0x0b,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL16_NM
			# DISAS: {{.*}}  0b 10        	syscall	0x3
	syscall[32]	# CHECK: syscall 0x0 # encoding: [0x08,0x00,0x00,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL_NM
			# DISAS: {{.*}}  08 00 00 00  	syscall	0x0
	syscall[32] 4	# CHECK: syscall 0x4 # encoding: [0x08,0x00,0x04,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL_NM
			# DISAS: {{.*}}  08 00 04 00  	syscall	0x4
	syscall[32] 0x3ffff	# CHECK: syscall 0x3ffff # encoding: [0x0b,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL_NM
			# DISAS: {{.*}}  0b 00 ff ff  	syscall	0x3ffff

	# 16-bit SAVE/RESTORE[.JRC]
	save[16] 16		# CHECK: save 16, 0 # encoding: [0x10,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  10 1c        	save	16, $fp
	save[16] 32, $ra		# CHECK: save 32, $ra # encoding: [0x21,0x1e]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  21 1e        	save	32, $ra
	save[16] 32, $fp, $ra	# CHECK: save 32, $fp, $ra # encoding: [0x22,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  22 1c        	save	32, $fp, $ra
	save[16] 32, $ra, $s0	# CHECK: save 32, $ra, $s0  # encoding: [0x22,0x1e]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  22 1e        	save	32, $ra, $s0
	save[16] 192, $ra, $s0-$gp	# CHECK: save 192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xce,0x1e]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  ce 1e        	save	192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
	save[16] 192, $fp, $ra, $s0-$gp	# CHECK: save 192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x1c]
					# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  cf 1c        	save	192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
	save[16] 128, $fp, $ra, $s0-$k1, $gp	# CHECK: save 128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0x8f,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  8f 1c        	save	128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp

	restore.jrc[16]	16	# CHECK: restore.jrc 16, 0	# encoding: [0x10,0x1d]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
				# DISAS: {{.*}}  10 1d        	restore.jrc	16, $fp
	restore.jrc[16]	32, $ra	# CHECK: restore.jrc 32, $ra	# encoding: [0x21,0x1f]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
				# DISAS: {{.*}}  21 1f        	restore.jrc	32, $ra
	restore.jrc[16]	32, $fp, $ra	# CHECK: restore.jrc 32, $fp, $ra	# encoding: [0x22,0x1d]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
					# DISAS: {{.*}}  22 1d        	restore.jrc	32, $fp, $ra
	restore.jrc[16]	32, $ra, $s0	# CHECK: restore.jrc 32, $ra, $s0	# encoding: [0x22,0x1f]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  22 1f        	restore.jrc	32, $ra, $s0
	restore.jrc[16]	32, $ra, $s0-$s3	# CHECK: restore.jrc 32, $ra, $s0, $s1, $s2, $s3 # encoding: [0x25,0x1f]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  25 1f        	restore.jrc	32, $ra, $s0, $s1, $s2, $s3
	restore.jrc[16]	64, $fp, $ra, $s0-$s7	# CHECK: restore.jrc 64, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7 # encoding: [0x4a,0x1d]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  4a 1d        	restore.jrc	64, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7
	restore.jrc[16]	192, $ra, $s0-$gp	# CHECK: restore.jrc 192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xce,0x1f]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  ce 1f        	restore.jrc	192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
	restore.jrc[16]	192, $fp, $ra, $s0-$gp	# CHECK: restore.jrc 192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x1d]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  cf 1d        	restore.jrc	192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
	restore.jrc[16]	128, $fp, $ra, $s0-$k1, $gp	# CHECK: restore.jrc 128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0x8f,0x1d]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  8f 1d        	restore.jrc	128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp

	# 32-bit SAVE/RESTORE[.JRC]
	save[32] 40		# CHECK: save 40, 0	# encoding: [0x00,0x80,0x28,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  00 80 28 30  	save	40
	save[32] 40, $ra		# CHECK: save 40, $ra	# encoding: [0xe1,0x83,0x28,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  e1 83 28 30  	save	40, $ra
	save[32] 256, $fp, $ra	# CHECK: save 256, $fp, $ra # encoding: [0xc2,0x83,0x00,0x31]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  c2 83 00 31  	save	256, $fp, $ra
	save[32] 1024, $ra, $s0	# CHECK: save 1024, $ra, $s0 # encoding: [0xe2,0x83,0x00,0x34]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  e2 83 00 34  	save	1024, $ra, $s0
	save[32] 160, $zero, $gp	# CHECK: save 160, $zero, $gp # encoding: [0x02,0x80,0xa4,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  02 80 a4 30  	save	160, $zero, $gp
	save[32] 64, $s0-$s3, $gp # CHECK: save 64, $s0, $s1, $s2, $s3, $gp # encoding: [0x05,0x82,0x44,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  05 82 44 30  	save	64, $s0, $s1, $s2, $s3, $gp
	save[32] 64, $a0-$s0, $gp # CHECK: save 64, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $gp # encoding: [0x8e,0x80,0x44,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  8e 80 44 30  	save	64, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $gp
	save[32] 128, $t3, $s0, $s1, $gp # CHECK: save 128, $t3, $s0, $s1, $gp # encoding: [0xe4,0x81,0x84,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  e4 81 84 30  	save	128, $t3, $s0, $s1, $gp
	save[32] 128, $a4-$t3, $s0-$s3 # CHECK: save 128, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $s1, $s2, $s3 # encoding: [0x0c,0x81,0x80,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  0c 81 80 30  	save	128, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $s1, $s2, $s3
	restore[32] 48		# CHECK: restore 48, 0	# encoding: [0x00,0x80,0x32,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
				# DISAS: {{.*}}  00 80 32 30  	restore	48
	restore[32] 48, $ra		# CHECK: restore 48, $ra	# encoding: [0xe1,0x83,0x32,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
				# DISAS: {{.*}}  e1 83 32 30  	restore	48, $ra
	restore[32] 256, $fp, $ra	# CHECK: restore 256, $fp, $ra	# encoding: [0xc2,0x83,0x02,0x31]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
				# DISAS: {{.*}}  c2 83 02 31  	restore	256, $fp, $ra
	restore[32] 1024, $ra, $s0	# CHECK: restore 1024, $ra, $s0	# encoding: [0xe2,0x83,0x02,0x34]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM	
			# DISAS: {{.*}}  e2 83 02 34  	restore	1024, $ra, $s0
	restore[32] 160, $zero, $gp	# CHECK: restore 160, $zero, $gp # encoding: [0x02,0x80,0xa6,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
				# DISAS: {{.*}}  02 80 a6 30  	restore	160, $zero, $gp
	restore[32] 64, $s0, $s1, $s2, $s3, $gp	# CHECK: restore 64,  $s0, $s1, $s2, $s3, $gp # encoding: [0x05,0x82,0x46,0x30]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
						# DISAS: {{.*}}  05 82 46 30  	restore	64, $s0, $s1, $s2, $s3, $gp
	restore[32] 64, $t4, $t5, $gp	# CHECK: restore 64, $t4, $t5, $gp # encoding: [0x43,0x80,0x46,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
					# DISAS: {{.*}}  43 80 46 30  	restore	64, $t4, $t5, $gp
	restore[32] 128, $fp, $ra, $s0, $s1, $gp	# CHECK: restore 128, $fp, $ra, $s0, $s1, $gp # encoding: [0xc5,0x83,0x86,0x30]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
						# DISAS: {{.*}}  c5 83 86 30  	restore	128, $fp, $ra, $s0, $s1, $gp
	restore[32] 120, $fp, $ra, $s0-$k1, $gp	# CHECK: restore 120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x83,0x7e,0x30]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
						# DISAS: {{.*}}  cf 83 7e 30  	restore	120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp

	restore.jrc[32] 56	# CHECK: restore.jrc 56, 0	# encoding: [0x00,0x80,0x3b,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
				# DISAS: {{.*}}  00 80 3b 30  	restore.jrc	56
	restore.jrc[32] 56, $ra	# CHECK: restore.jrc 56, $ra	# encoding: [0xe1,0x83,0x3b,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
				# DISAS: {{.*}}  e1 83 3b 30  	restore.jrc	56, $ra
	restore.jrc[32] 256, $fp, $ra	# CHECK: restore.jrc 256, $fp, $ra	# encoding: [0xc2,0x83,0x03,0x31]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  c2 83 03 31  	restore.jrc	256, $fp, $ra
	restore.jrc[32] 1024, $ra, $s0	# CHECK: restore.jrc 1024, $ra, $s0	# encoding: [0xe2,0x83,0x03,0x34]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  e2 83 03 34  	restore.jrc	1024, $ra, $s0
	restore.jrc[32] 160, $zero, $gp	# CHECK: restore.jrc 160, $zero, $gp	# encoding: [0x02,0x80,0xa7,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  02 80 a7 30  	restore.jrc	160, $zero, $gp
	restore.jrc[32] 64, $s0, $s1, $s2, $s3, $gp	# CHECK: restore.jrc 64, $s0, $s1, $s2, $s3, $gp # encoding: [0x05,0x82,0x47,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  05 82 47 30  	restore.jrc	64, $s0, $s1, $s2, $s3, $gp
	restore.jrc[32] 64, $t4, $t5, $gp	# CHECK: restore.jrc 64, $t4, $t5, $gp # encoding: [0x43,0x80,0x47,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  43 80 47 30  	restore.jrc	64, $t4, $t5, $gp
	restore.jrc[32] 128, $fp, $ra, $s0, $s1, $gp	# CHECK: restore.jrc 128, $fp, $ra, $s0, $s1, $gp # encoding: [0xc5,0x83,0x87,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  c5 83 87 30  	restore.jrc	128, $fp, $ra, $s0, $s1, $gp
	restore.jrc[32] 120, $fp, $ra, $s0-$k1, $gp	# CHECK: restore.jrc 120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x83,0x7f,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  cf 83 7f 30  	restore.jrc	120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp

	addiu[r2] $a1,$a1,8	# CHECK: addiu	$a1, $a1, 8 # encoding: [0xd2,0x92]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR2_NM
				# DISAS: {{.*}}  d2 92        	addiu	$a1, $a1, 8
	addiu[r2] $a1,$a3,0	# CHECK: addiu	$a1, $a3, 0 # encoding: [0xf0,0x92]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR2_NM
				# DISAS: {{.*}}  f0 92        	addiu	$a1, $a3, 0
	addiu[r2] $s1,$a1,28	# CHECK: addiu	$s1, $a1, 28 # encoding: [0xd7,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR2_NM
				# DISAS: {{.*}}  d7 90        	addiu	$s1, $a1, 28
	addiu[32] $s1,$a1,32	# CHECK: addiu	$s1, $a1, 32 # encoding: [0x25,0x02,0x20,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  25 02 20 00  	addiu	$s1, $a1, 32
	addiu[32] $a1,$a1,65535	# CHECK: addiu	$a1, $a1, 65535 # encoding: [0xa5,0x00,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  a5 00 ff ff  	addiu	$a1, $a1, 65535
	addiu[48] $s2,$s2,65536	# CHECK: addiu	$s2, $s2, 65536 # encoding: [0x41,0x62,0x00,0x00,0x01,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
				# DISAS: {{.*}}  41 62 00 00 01 00    	addiu	$s2, $s2, 65536
	addiu[48] $t4,$t4,0x7fffffff	# CHECK: addiu	$t4, $t4, 2147483647 # encoding: [0x41,0x60,0xff,0xff,0xff,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
				# DISAS: {{.*}}  41 60 ff ff ff 7f    	addiu	$t4, $t4, 2147483647
	addiu[rs5] $a4,$a4,0	# CHECK: addiu	$a4, $a4, 0 # encoding: [0x08,0x91]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIURS5_NM
				# DISAS: {{.*}}  08 91        	addiu	$a4, $a4, 0
	addiu[rs5] $t8,$t8,7	# CHECK: addiu	$t8, $t8, 7 # encoding: [0x0f,0x93]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIURS5_NM
				# DISAS: {{.*}}  0f 93        	addiu	$t8, $t8, 7
	addiu[rs5] $sp,$sp,-8	# CHECK: addiu	$sp, $sp, -8 # encoding: [0xb8,0x93]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIURS5_NM
				# DISAS: {{.*}}  b8 93        	addiu	$sp, $sp, -8
	addiu[32] $k1,$k1,8	# CHECK: addiu	$k1, $k1, 8 # encoding: [0x7b,0x03,0x08,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  7b 03 08 00  	addiu	$k1, $k1, 8
	addiu[neg] $k1,$k1,-9	# CHECK: addiu	$k1, $k1, -9 # encoding: [0x7b,0x83,0x09,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  7b 83 09 80  	addiu	$k1, $k1, -9
	addiu[r1.sp] $a1,$sp,0	# CHECK: addiu	$a1, $sp, 0 # encoding: [0xc0,0x72]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR1SP_NM
				# DISAS: {{.*}}  c0 72        	addiu	$a1, $sp, 0
	addiu[r1.sp] $s0,$sp,128	# CHECK: addiu	$s0, $sp, 128 # encoding: [0x60,0x70]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR1SP_NM
				# DISAS: {{.*}}  60 70        	addiu	$s0, $sp, 128
	addiu[r1.sp] $s3,$sp,252	# CHECK: addiu	$s3, $sp, 252 # encoding: [0xff,0x71]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR1SP_NM
				# DISAS: {{.*}}  ff 71        	addiu	$s3, $sp, 252
	addiu[32] $s3,$sp,256	# CHECK: addiu	$s3, $sp, 256 # encoding: [0x7d,0x02,0x00,0x01]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  7d 02 00 01  	addiu	$s3, $sp, 256
	addiu[neg] $a1,$a2,0	# CHECK: addiu	$a1, $a2, 0 # encoding: [0xa6,0x80,0x00,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  a6 80 00 80  	addiu	$a1, $a2, 0
	addiu[neg] $a1,$a2,-1	# CHECK: addiu	$a1, $a2, -1 # encoding: [0xa6,0x80,0x01,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  a6 80 01 80  	addiu	$a1, $a2, -1
	addiu[neg] $a1,$a2,-4095	# CHECK: addiu	$a1, $a2, -4095 # encoding: [0xa6,0x80,0xff,0x8f]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  a6 80 ff 8f  	addiu	$a1, $a2, -4095
	addiu[48] $s3,$s3,-4096	# CHECK: addiu	$s3, $s3, -4096 # encoding: [0x61,0x62,0x00,0xf0,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
				# DISAS: {{.*}}  61 62 00 f0 ff ff    	addiu	$s3, $s3, 4294963200
	addiu[48] $a3,$a3,-2147483648	# CHECK: addiu	$a3, $a3, -2147483648 # encoding: [0xe1,0x60,0x00,0x00,0x00,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
				# DISAS: {{.*}}  e1 60 00 00 00 80    	addiu	$a3, $a3, 2147483648
	addiu[gp.b] $a1, $gp, 0	# CHECK: addiu.b $a1, $gp, 0 # encoding: [0xac,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPB_NM
				# DISAS: {{.*}}  ac 44 00 00  	addiu.b	$a1, $gp, 0
	addiu[gp.b] $s0,$gp,131701	# CHECK: addiu.b $s0, $gp, 131701 # encoding: [0x0e,0x46,0x75,0x02]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPB_NM
				# DISAS: {{.*}}  0e 46 75 02  	addiu.b	$s0, $gp, 131701
	addiu[gp.b] $s3,$gp,262143	# CHECK: addiu.b $s3, $gp, 262143 # encoding: [0x6f,0x46,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPB_NM
				# DISAS: {{.*}}  6f 46 ff ff  	addiu.b	$s3, $gp, 262143
	addiu[gp.w] $k0,$gp,262144	# CHECK: addiu.w $k0, $gp, 262144 # encoding: [0x44,0x43,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPW_NM
				# DISAS: {{.*}}  44 43 00 00  	addiu.w	$k0, $gp, 262144
	addiu[gp.w] $s7,$gp,2097148	# CHECK: addiu.w $s7, $gp, 2097148 # encoding: [0xff,0x42,0xfc,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPW_NM
				# DISAS: {{.*}}  ff 42 fc ff  	addiu.w	$s7, $gp, 2097148
	addiu[gp48] $a4,$gp,2097152	# CHECK: addiu.b32 $a4, $gp, 2097152 # encoding: [0x02,0x61,0x00,0x00,0x20,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGP48_NM
				# DISAS: {{.*}}  02 61 00 00 20 00    	addiu.b32	$a4, $gp, 2097152
	addiu[gp48] $s4,$gp,262146	# CHECK: addiu.b32 $s4, $gp, 262146 # encoding: [0x82,0x62,0x02,0x00,0x04,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGP48_NM
				# DISAS: {{.*}}  82 62 02 00 04 00    	addiu.b32	$s4, $gp, 262146


	lw[16]	$a0, 0($s3)	# CHECK: lw $a0, 0($s3) # encoding: [0x30,0x16]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
				# DISAS: {{.*}}  30 16        	lw	$a0, 0($s3)
	lw[16]	$a1, 4($s2)	# CHECK: lw $a1, 4($s2) # encoding: [0xa1,0x16]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
				# DISAS: {{.*}}  a1 16        	lw	$a1, 4($s2)
	lw[16]	$a2, 32($s1)	# CHECK: lw $a2, 32($s1) # encoding: [0x18,0x17]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
				# DISAS: {{.*}}  18 17        	lw	$a2, 32($s1)
	lw[16]	$a3, 60($s0)	# CHECK: lw $a3, 60($s0) # encoding: [0x8f,0x17]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
				# DISAS: {{.*}}  8f 17        	lw	$a3, 60($s0)

	lw[4x4]	$a7, 0($s7)	# CHECK: lw $a7, 0($s7) # encoding: [0x77,0x74]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
				# DISAS: {{.*}}  77 74        	lw	$a7, 0($s7)
	lw[4x4] $s3, 4($a5)	# CHECK: lw $s3, 4($a5) # encoding: [0x61,0x77]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
				# DISAS: {{.*}}  61 77        	lw	$s3, 4($at)
	lw[4x4] $s4, 8($a3)	# CHECK: lw $s4, 8($a3) # encoding: [0x8f,0x76]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
				# DISAS: {{.*}}  8f 76        	lw	$s4, 8($a3)
	lw[4x4] $s6, 12($a0)	# CHECK: lw $s6, 12($a0) # encoding: [0xcc,0x77]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
				# DISAS: {{.*}}  cc 77        	lw	$s6, 12($a0)

	lw[s9]	$a2, -4($s4)	# CHECK: lw $a2, -4($s4) # encoding: [0xd4,0xa4,0xfc,0xc0]
				# CHECK-NEXT: <MCInst  #{{.*}} LWs9_NM
				# DISAS: {{.*}}  d4 a4 fc c0  	lw	$a2, -4($s4)
	lw[s9]	$a3, -256($s0)	# CHECK: lw $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xc0]
				# CHECK-NEXT: <MCInst  #{{.*}} LWs9_NM
				# DISAS: {{.*}}  f0 a4 00 c0  	lw	$a3, -256($s0)
	lw[s9]	$a4, 64($s3)	# CHECK: lw $a4, 64($s3) # encoding: [0x13,0xa5,0x40,0x40]
				# CHECK-NEXT: <MCInst  #{{.*}} LWs9_NM
				# DISAS: {{.*}}  13 a5 40 40  	lw	$a4, 64($s3)
	lw[s9]	$a1, 252($s2)	# CHECK: lw $a1, 252($s2) # encoding: [0xb2,0xa4,0xfc,0x40]
				# CHECK-NEXT: <MCInst  #{{.*}} LWs9_NM
				# DISAS: {{.*}}  b2 a4 fc 40  	lw	$a1, 252($s2)
	lw[u12]	$a4, 64($s3)	# CHECK: lw $a4, 64($s3) # encoding: [0x13,0x85,0x40,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LW_NM
				# DISAS: {{.*}}  13 85 40 80  	lw	$a4, 64($s3)
	lw[u12]	$a1, 252($s2)	# CHECK: lw $a1, 252($s2) # encoding: [0xb2,0x84,0xfc,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LW_NM
				# DISAS: {{.*}}  b2 84 fc 80  	lw	$a1, 252($s2)
	lw[u12]	$s3, 4092($t5)	# CHECK: lw $s3, 4092($t5) # encoding: [0x63,0x86,0xfc,0x8f]
				# CHECK-NEXT: <MCInst  #{{.*}} LW_NM
				# DISAS: {{.*}}  63 86 fc 8f  	lw	$s3, 4092($t5)

	lw[sp]	$a0, 0($sp)	# CHECK: lw $a0, 0($sp) # encoding: [0x80,0x34]
				# CHECK-NEXT: <MCInst  #{{.*}} LWSP16_NM
				# DISAS: {{.*}}  80 34        	lw	$a0, 0($sp)
	lw[sp]	$t0, 64($sp)	# CHECK: lw $t0, 64($sp) # encoding: [0x90,0x35]
				# CHECK-NEXT: <MCInst  #{{.*}} LWSP16_NM
				# DISAS: {{.*}}  90 35        	lw	$t0, 64($sp)
	lw[sp]	$k0, 124($sp)	# CHECK: lw $k0, 124($sp) # encoding: [0x5f,0x37]
				# CHECK-NEXT: <MCInst  #{{.*}} LWSP16_NM
				# DISAS: {{.*}}  5f 37        	lw	$k0, 124($sp)

	lw[gp16] $a0, 0($gp)	# CHECK: lw $a0, 0($gp) # encoding: [0x00,0x56]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP16_NM
				# DISAS: {{.*}}  00 56        	lw	$a0, 0($gp)
	lw[gp16] $a3, 256($gp)	# CHECK: lw $a3, 256($gp) # encoding: [0xc0,0x57]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP16_NM
				# DISAS: {{.*}}  c0 57        	lw	$a3, 256($gp)
	lw[gp16] $s0, 508($gp)	# CHECK: lw $s0, 508($gp) # encoding: [0x7f,0x54]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP16_NM
				# DISAS: {{.*}}  7f 54        	lw	$s0, 508($gp)
	lw[gp]	$a3, 4096($gp)	# CHECK: lw $a3, 4096($gp) # encoding: [0xe0,0x40,0x02,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP_NM
				# DISAS: {{.*}}  e0 40 02 10  	lw	$a3, 4096($gp)
	lw[gp]	$s3, 65536($gp)	# CHECK: lw $s3, 65536($gp) # encoding: [0x61,0x42,0x02,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP_NM
				# DISAS: {{.*}}  61 42 02 00  	lw	$s3, 65536($gp)
	lw[gp]	$s0, 2097148($gp)	# CHECK: lw $s0, 2097148($gp) # encoding: [0x1f,0x42,0xfe,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LWGP_NM
				# DISAS: {{.*}}  1f 42 fe ff  	lw	$s0, 2097148($gp)

	sw[16]	$a0, 4($a1)	# CHECK: sw $a0, 4($a1) # encoding: [0x51,0x96]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
				# DISAS: {{.*}}  51 96        	sw	$a0, 4($a1)
	sw[16]	$a1, 4($s2)	# CHECK: sw $a1, 4($s2) # encoding: [0xa1,0x96]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
				# DISAS: {{.*}}  a1 96        	sw	$a1, 4($s2)
	sw[16]	$a2, 32($s1)	# CHECK: sw $a2, 32($s1) # encoding: [0x18,0x97]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
				# DISAS: {{.*}}  18 97        	sw	$a2, 32($s1)
	sw[16]	$zero, 60($a3)	# CHECK: sw $zero, 60($a3) # encoding: [0x7f,0x94]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
				# DISAS: {{.*}}  7f 94        	sw	$zero, 60($a3)

	sw[4x4] $zero, 0($s7)	# CHECK: sw $zero, 0($s7) # encoding: [0x77,0xf4]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
				# DISAS: {{.*}}  77 f4        	sw	$zero, 0($s7)
	sw[4x4] $s3, 4($a5)	# CHECK: sw $s3, 4($a5) # encoding: [0x61,0xf7]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
				# DISAS: {{.*}}  61 f7        	sw	$s3, 4($at)
	sw[4x4] $s4, 8($a3)	# CHECK: sw $s4, 8($a3) # encoding: [0x8f,0xf6]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
				# DISAS: {{.*}}  8f f6        	sw	$s4, 8($a3)
	sw[4x4] $s6, 12($a0)	# CHECK: sw $s6, 12($a0) # encoding: [0xcc,0xf7]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
				# DISAS: {{.*}}  cc f7        	sw	$s6, 12($a0)

	sw[s9]	$a2, -4($s6)	# CHECK: sw $a2, -4($s6) # encoding: [0xd6,0xa4,0xfc,0xc8]
				# CHECK-NEXT: <MCInst  #{{.*}} SWs9_NM
				# DISAS: {{.*}}  d6 a4 fc c8  	sw	$a2, -4($s6)
	sw[s9]	$a3, -256($s0)	# CHECK: sw $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xc8]
				# CHECK-NEXT: <MCInst  #{{.*}} SWs9_NM
				# DISAS: {{.*}}  f0 a4 00 c8  	sw	$a3, -256($s0)
	sw[s9]	$a5, 64($s3)	# CHECK: sw $a5, 64($s3) # encoding: [0x33,0xa5,0x40,0x48]
				# CHECK-NEXT: <MCInst  #{{.*}} SWs9_NM
				# DISAS: {{.*}}  33 a5 40 48  	sw	$a5, 64($s3)
	sw[s9]	$a1, 252($s2)	# CHECK: sw $a1, 252($s2) # encoding: [0xb2,0xa4,0xfc,0x48]
				# CHECK-NEXT: <MCInst  #{{.*}} SWs9_NM
				# DISAS: {{.*}}  b2 a4 fc 48  	sw	$a1, 252($s2)
	sw[u12]	$a5, 64($s3)	# CHECK: sw $a5, 64($s3) # encoding: [0x33,0x85,0x40,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
				# DISAS: {{.*}}  33 85 40 90  	sw	$a5, 64($s3)
	sw[u12]	$a1, 252($s2)	# CHECK: sw $a1, 252($s2) # encoding: [0xb2,0x84,0xfc,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
				# DISAS: {{.*}}  b2 84 fc 90  	sw	$a1, 252($s2)
	sw[u12]	$s3, 4092($t5)	# CHECK: sw $s3, 4092($t5) # encoding: [0x63,0x86,0xfc,0x9f]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
				# DISAS: {{.*}}  63 86 fc 9f  	sw	$s3, 4092($t5)

	sw[sp]	$a0, 0($sp)	# CHECK: sw $a0, 0($sp) # encoding: [0x80,0xb4]
				# CHECK-NEXT: <MCInst  #{{.*}} SWSP16_NM
				# DISAS: {{.*}}  80 b4        	sw	$a0, 0($sp)
	sw[sp]	$t0, 64($sp)	# CHECK: sw $t0, 64($sp) # encoding: [0x90,0xb5]
				# CHECK-NEXT: <MCInst  #{{.*}} SWSP16_NM
				# DISAS: {{.*}}  90 b5        	sw	$t0, 64($sp)
	sw[sp]	$k0, 124($sp)	# CHECK: sw $k0, 124($sp) # encoding: [0x5f,0xb7]
				# CHECK-NEXT: <MCInst  #{{.*}} SWSP16_NM
				# DISAS: {{.*}}  5f b7        	sw	$k0, 124($sp)
	sw[u12]	$a1, 4092($sp)	# CHECK: sw $a1, 4092($sp) # encoding: [0xbd,0x84,0xfc,0x9f]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
				# DISAS: {{.*}}  bd 84 fc 9f  	sw	$a1, 4092($sp)
	 
	sw[gp16] $a0, 0($gp)	# CHECK: sw $a0, 0($gp) # encoding: [0x00,0xd6]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP16_NM
				# DISAS: {{.*}}  00 d6        	sw	$a0, 0($gp)
	sw[gp16] $a3, 256($gp)	# CHECK: sw $a3, 256($gp) # encoding: [0xc0,0xd7]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP16_NM
				# DISAS: {{.*}}  c0 d7        	sw	$a3, 256($gp)
	sw[gp16] $zero, 508($gp)	# CHECK: sw $zero, 508($gp) # encoding: [0x7f,0xd4]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP16_NM
				# DISAS: {{.*}}  7f d4        	sw	$zero, 508($gp)
	sw[gp]	$a2, 512($gp)	# CHECK: sw $a2, 512($gp) # encoding: [0xc0,0x40,0x03,0x02]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP_NM
				# DISAS: {{.*}}  c0 40 03 02  	sw	$a2, 512($gp)
	sw[gp]	$s2, 65536($gp)	# CHECK: sw $s2, 65536($gp) # encoding: [0x41,0x42,0x03,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP_NM
				# DISAS: {{.*}}  41 42 03 00  	sw	$s2, 65536($gp)
	sw[gp]	$zero, 2097148($gp)	# CHECK: sw $zero, 2097148($gp) # encoding: [0x1f,0x40,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP_NM
				# DISAS: {{.*}}  1f 40 ff ff  	sw	$zero, 2097148($gp)

	lb[16]	$a0, 0($s3)	# CHECK: lb $a0, 0($s3) # encoding:  [0x30,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
				# DISAS: {{.*}}  30 5e        	lb	$a0, 0($s3)
	lb[16]	$a1, 1($s2)	# CHECK: lb $a1, 1($s2) # encoding: [0xa1,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
				# DISAS: {{.*}}  a1 5e        	lb	$a1, 1($s2)
	lb[16]	$a2, 2($s1)	# CHECK: lb $a2, 2($s1) # encoding: [0x12,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
				# DISAS: {{.*}}  12 5f        	lb	$a2, 2($s1)
	lb[16]	$s0, 3($s0)	# CHECK: lb $s0, 3($s0) # encoding: [0x03,0x5c]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
				# DISAS: {{.*}}  03 5c        	lb	$s0, 3($s0)

	lb[s9]	$a2, -4($s1)	# CHECK: lb $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LBs9_NM
				# DISAS: {{.*}}  d1 a4 fc 80  	lb	$a2, -4($s1)
	lb[s9]	$a3, -256($s5)	# CHECK: lb $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LBs9_NM
				# DISAS: {{.*}}  f5 a4 00 80  	lb	$a3, -256($s5)
	lb[s9]	$a3, 4($s0)	# CHECK: lb $a3, 4($s0) # encoding: [0xf0,0xa4,0x04,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LBs9_NM
				# DISAS: {{.*}}  f0 a4 04 00  	lb	$a3, 4($s0)
	lb[s9]	$a5, 255($s2)	# CHECK: lb $a5, 255($s2) # encoding: [0x32,0xa5,0xff,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LBs9_NM
				# DISAS: {{.*}}  32 a5 ff 00  	lb	$a5, 255($s2)
	lb[u12]	$a3, 4($s0)	# CHECK: lb $a3, 4($s0) # encoding: [0xf0,0x84,0x04,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LB_NM
				# DISAS: {{.*}}  f0 84 04 00  	lb	$a3, 4($s0)
	lb[u12]	$a5, 255($s2)	# CHECK: lb $a5, 255($s2) # encoding: [0x32,0x85,0xff,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LB_NM
				# DISAS: {{.*}}  32 85 ff 00  	lb	$a5, 255($s2)
	lb[u12]	$s3, 4095($t5)	# CHECK: lb $s3, 4095($t5) # encoding: [0x63,0x86,0xff,0x0f]
				# CHECK-NEXT: <MCInst  #{{.*}} LB_NM
				# DISAS: {{.*}}  63 86 ff 0f  	lb	$s3, 4095($t5)

	lb[gp]	$a0, 256($gp)	# CHECK: lb $a0, 256($gp) # encoding: [0x80,0x44,0x00,0x01]
				# CHECK-NEXT: <MCInst  #{{.*}} LBGP_NM
				# DISAS: {{.*}}  80 44 00 01  	lb	$a0, 256($gp)
	lb[gp]	$s3, 4096($gp)	# CHECK: lb $s3, 4096($gp) # encoding: [0x60,0x46,0x00,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} LBGP_NM
				# DISAS: {{.*}}  60 46 00 10  	lb	$s3, 4096($gp)
	lb[gp]	$s0, 262143($gp)	# CHECK: lb $s0, 262143($gp) # encoding: [0x03,0x46,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} LBGP_NM
				# DISAS: {{.*}}  03 46 ff ff  	lb	$s0, 262143($gp)

	lbu[16] $a0, 0($s3)	# CHECK: lbu $a0, 0($s3) # encoding: [0x38,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
				# DISAS: {{.*}}  38 5e        	lbu	$a0, 0($s3)
	lbu[16] $a1, 1($s2)	# CHECK: lbu $a1, 1($s2) # encoding: [0xa9,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
				# DISAS: {{.*}}  a9 5e        	lbu	$a1, 1($s2)
	lbu[16] $a2, 2($s1)	# CHECK: lbu $a2, 2($s1) # encoding: [0x1a,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
				# DISAS: {{.*}}  1a 5f        	lbu	$a2, 2($s1)
	lbu[16] $s0, 3($s0)	# CHECK: lbu $s0, 3($s0) # encoding: [0x0b,0x5c]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
				# DISAS: {{.*}}  0b 5c        	lbu	$s0, 3($s0)

	lbu[s9] $a2, -4($s1)	# CHECK: lbu $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUs9_NM
				# DISAS: {{.*}}  d1 a4 fc 90  	lbu	$a2, -4($s1)
	lbu[s9] $a3, -256($s5)	# CHECK: lbu $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUs9_NM
				# DISAS: {{.*}}  f5 a4 00 90  	lbu	$a3, -256($s5)
	lbu[s9] $a3, 4($s0)	# CHECK: lbu $a3, 4($s0) # encoding: [0xf0,0xa4,0x04,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUs9_NM
				# DISAS: {{.*}}  f0 a4 04 10  	lbu	$a3, 4($s0)
	lbu[s9] $a5, 255($s2)	# CHECK: lbu $a5, 255($s2) # encoding: [0x32,0xa5,0xff,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUs9_NM
				# DISAS: {{.*}}  32 a5 ff 10  	lbu	$a5, 255($s2)
	lbu[u12] $a3, 4($s0)	# CHECK: lbu $a3, 4($s0) # encoding: [0xf0,0x84,0x04,0x20]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU_NM
				# DISAS: {{.*}}  f0 84 04 20  	lbu	$a3, 4($s0)
	lbu[u12] $a5, 255($s2)	# CHECK: lbu $a5, 255($s2) # encoding: [0x32,0x85,0xff,0x20]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU_NM
				# DISAS: {{.*}}  32 85 ff 20  	lbu	$a5, 255($s2)
	lbu[u12] $s3, 4095($t5)	# CHECK: lbu $s3, 4095($t5) # encoding: [0x63,0x86,0xff,0x2f]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU_NM
				# DISAS: {{.*}}  63 86 ff 2f  	lbu	$s3, 4095($t5)

	lbu[gp] $a0, 0($gp)	# CHECK: lbu $a0, 0($gp) # encoding: [0x88,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUGP_NM
				# DISAS: {{.*}}  88 44 00 00  	lbu	$a0, 0($gp)
	lbu[gp] $s3, 65536($gp)	# CHECK: lbu $s3, 65536($gp) # encoding: [0x69,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUGP_NM
				# DISAS: {{.*}}  69 46 00 00  	lbu	$s3, 65536($gp)
	lbu[gp] $s0, 262143($gp)	# CHECK: lbu $s0, 262143($gp) # encoding: [0x0b,0x46,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUGP_NM
				# DISAS: {{.*}}  0b 46 ff ff  	lbu	$s0, 262143($gp)

	sb[16]	$a0, 0($s3)	# CHECK: sb $a0, 0($s3) # encoding: [0x34,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
				# DISAS: {{.*}}  34 5e        	sb	$a0, 0($s3)
	sb[16]	$a1, 1($s2)	# CHECK: sb $a1, 1($s2) # encoding: [0xa5,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
				# DISAS: {{.*}}  a5 5e        	sb	$a1, 1($s2)
	sb[16]	$a2, 2($s1)	# CHECK: sb $a2, 2($s1) # encoding: [0x16,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
				# DISAS: {{.*}}  16 5f        	sb	$a2, 2($s1)
	sb[16]	$zero, 3($s0)	# CHECK: sb $zero, 3($s0) # encoding: [0x07,0x5c]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
				# DISAS: {{.*}}  07 5c        	sb	$zero, 3($s0)

	sb[s9]	$a2, -4($s1)	# CHECK: sb $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0x88]
				# CHECK-NEXT: <MCInst  #{{.*}} SBs9_NM
				# DISAS: {{.*}}  d1 a4 fc 88  	sb	$a2, -4($s1)
	sb[s9]	$a3, -256($s5)	# CHECK: sb $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x88]
				# CHECK-NEXT: <MCInst  #{{.*}} SBs9_NM
				# DISAS: {{.*}}  f5 a4 00 88  	sb	$a3, -256($s5)
	sb[s9]	$a3, 4($s0)	# CHECK: sb $a3, 4($s0) # encoding: [0xf0,0xa4,0x04,0x08]
				# CHECK-NEXT: <MCInst  #{{.*}} SBs9_NM
				# DISAS: {{.*}}  f0 a4 04 08  	sb	$a3, 4($s0)
	sb[s9]	$a5, 255($s2)	# CHECK: sb $a5, 255($s2) # encoding: [0x32,0xa5,0xff,0x08]
				# CHECK-NEXT: <MCInst  #{{.*}} SBs9_NM
				# DISAS: {{.*}}  32 a5 ff 08  	sb	$a5, 255($s2)
	sb[u12]	$a3, 4($s0)	# CHECK: sb $a3, 4($s0) # encoding: [0xf0,0x84,0x04,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} SB_NM
				# DISAS: {{.*}}  f0 84 04 10  	sb	$a3, 4($s0)
	sb[u12]	$a5, 255($s2)	# CHECK: sb $a5, 255($s2) # encoding: [0x32,0x85,0xff,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} SB_NM
				# DISAS: {{.*}}  32 85 ff 10  	sb	$a5, 255($s2)
	sb[u12]	$s3, 4095($t5)	# CHECK: sb $s3, 4095($t5) # encoding: [0x63,0x86,0xff,0x1f]
				# CHECK-NEXT: <MCInst  #{{.*}} SB_NM
				# DISAS: {{.*}}  63 86 ff 1f  	sb	$s3, 4095($t5)

	sb[gp]	$a0, 0($gp)	# CHECK: sb $a0, 0($gp) # encoding: [0x84,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SBGP_NM
				# DISAS: {{.*}}  84 44 00 00  	sb	$a0, 0($gp)
	sb[gp]	$s3, 65536($gp)	# CHECK: sb $s3, 65536($gp) # encoding: [0x65,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SBGP_NM
				# DISAS: {{.*}}  65 46 00 00  	sb	$s3, 65536($gp)
	sb[gp]	$s0, 262143($gp)	# CHECK: sb $s0, 262143($gp) # encoding: [0x07,0x46,0xff,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} SBGP_NM
					# DISAS: {{.*}}  07 46 ff ff  	sb	$s0, 262143($gp)

	lh[16]	$a0, 0($s3)	# CHECK: lh $a0, 0($s3) # encoding: [0x30,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
				# DISAS: {{.*}}  30 7e        	lh	$a0, 0($s3)
	lh[16]	$a1, 2($s2)	# CHECK: lh $a1, 2($s2) # encoding: [0xa2,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
				# DISAS: {{.*}}  a2 7e        	lh	$a1, 2($s2)
	lh[16]	$a2, 4($s1)	# CHECK: lh $a2, 4($s1) # encoding: [0x14,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
				# DISAS: {{.*}}  14 7f        	lh	$a2, 4($s1)
	lh[16]	$s0, 6($s0)	# CHECK: lh $s0, 6($s0) # encoding: [0x06,0x7c]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
				# DISAS: {{.*}}  06 7c        	lh	$s0, 6($s0)

	lh[s9]	$a2, -4($s1)	# CHECK: lh $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0xa0]
				# CHECK-NEXT: <MCInst  #{{.*}} LHs9_NM
				# DISAS: {{.*}}  d1 a4 fc a0  	lh	$a2, -4($s1)
	lh[s9]	$a3, -256($s5)	# CHECK: lh $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xa0]
				# CHECK-NEXT: <MCInst  #{{.*}} LHs9_NM
				# DISAS: {{.*}}  f5 a4 00 a0  	lh	$a3, -256($s5)
	lh[s9]	$a4, 8($s0)	# CHECK: lh $a4, 8($s0) # encoding: [0x10,0xa5,0x08,0x20]
				# CHECK-NEXT: <MCInst  #{{.*}} LHs9_NM
				# DISAS: {{.*}}  10 a5 08 20  	lh	$a4, 8($s0)
	lh[s9]	$a5, 254($s2)	# CHECK: lh $a5, 254($s2) # encoding: [0x32,0xa5,0xfe,0x20]
				# CHECK-NEXT: <MCInst  #{{.*}} LHs9_NM
				# DISAS: {{.*}}  32 a5 fe 20  	lh	$a5, 254($s2)
	lh[u12] $a4, 8($s0)	# CHECK: lh $a4, 8($s0) # encoding: [0x10,0x85,0x08,0x40]
				# CHECK-NEXT: <MCInst  #{{.*}} LH_NM
				# DISAS: {{.*}}  10 85 08 40  	lh	$a4, 8($s0)
	lh[u12] $a5, 254($s2)	# CHECK: lh $a5, 254($s2) # encoding: [0x32,0x85,0xfe,0x40]
				# CHECK-NEXT: <MCInst  #{{.*}} LH_NM
				# DISAS: {{.*}}  32 85 fe 40  	lh	$a5, 254($s2)
	lh[u12] $s3, 4094($t5)	# CHECK: lh $s3, 4094($t5) # encoding: [0x63,0x86,0xfe,0x4f]
				# CHECK-NEXT: <MCInst  #{{.*}} LH_NM
				# DISAS: {{.*}}  63 86 fe 4f  	lh	$s3, 4094($t5)

	lh[gp]	$a0, 0($gp)	# CHECK: lh $a0, 0($gp) # encoding: [0x90,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHGP_NM
				# DISAS: {{.*}}  90 44 00 00  	lh	$a0, 0($gp)
	lh[gp]	$s3, 65536($gp)	# CHECK: lh $s3, 65536($gp) # encoding: [0x71,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHGP_NM
				# DISAS: {{.*}}  71 46 00 00  	lh	$s3, 65536($gp)
	lh[gp]	$s0, 262142($gp)	# CHECK: lh $s0, 262142($gp) # encoding: [0x13,0x46,0xfe,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LHGP_NM
				# DISAS: {{.*}}  13 46 fe ff  	lh	$s0, 262142($gp)
	lhu[16] $a0, 0($s3)	# CHECK: lhu $a0, 0($s3) # encoding: [0x38,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
				# DISAS: {{.*}}  38 7e        	lhu	$a0, 0($s3)
	lhu[16] $a1, 2($s2)	# CHECK: lhu $a1, 2($s2) # encoding: [0xaa,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
				# DISAS: {{.*}}  aa 7e        	lhu	$a1, 2($s2)
	lhu[16]	$a2, 4($s1)	# CHECK: lhu $a2, 4($s1) # encoding: [0x1c,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
				# DISAS: {{.*}}  1c 7f        	lhu	$a2, 4($s1)
	lhu[16] $s0, 6($s0)	# CHECK: lhu $s0, 6($s0) # encoding: [0x0e,0x7c]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
				# DISAS: {{.*}}  0e 7c        	lhu	$s0, 6($s0)
	lhu[s9] $a2, -4($s1)	# CHECK: lhu $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0xb0]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUs9_NM
				# DISAS: {{.*}}  d1 a4 fc b0  	lhu	$a2, -4($s1)
	lhu[s9] $a3, -256($s5)		# CHECK: lhu $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xb0]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUs9_NM
				# DISAS: {{.*}}  f5 a4 00 b0  	lhu	$a3, -256($s5)
	lhu[s9]	$a4, 8($s0)		# CHECK: lhu $a4, 8($s0) # encoding: [0x10,0xa5,0x08,0x30]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUs9_NM
					# DISAS: {{.*}}  10 a5 08 30  	lhu	$a4, 8($s0)
	lhu[s9]	$a5, 254($s2)		# CHECK: lhu $a5, 254($s2) # encoding: [0x32,0xa5,0xfe,0x30]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUs9_NM
					# DISAS: {{.*}}  32 a5 fe 30  	lhu	$a5, 254($s2)
	lhu[u12] $a4, 8($s0)		# CHECK: lhu $a4, 8($s0) # encoding: [0x10,0x85,0x08,0x60]
					# CHECK-NEXT: <MCInst  #{{.*}} LHU_NM
					# DISAS: {{.*}}  10 85 08 60  	lhu	$a4, 8($s0)
	lhu[u12] $a5, 254($s2)		# CHECK: lhu $a5, 254($s2) # encoding: [0x32,0x85,0xfe,0x60]
					# CHECK-NEXT: <MCInst  #{{.*}} LHU_NM
					# DISAS: {{.*}}  32 85 fe 60  	lhu	$a5, 254($s2)
	lhu[u12] $s3, 4094($t5)		# CHECK: lhu $s3, 4094($t5) # encoding: [0x63,0x86,0xfe,0x6f]
					# CHECK-NEXT: <MCInst  #{{.*}} LHU_NM
					# DISAS: {{.*}}  63 86 fe 6f  	lhu	$s3, 4094($t5)
	lhu[gp] $a0, 0($gp)	# CHECK: lhu $a0, 0($gp) # encoding: [0x90,0x44,0x01,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHUGP_NM
				# DISAS: {{.*}}  90 44 01 00  	lhu	$a0, 0($gp)
	lhu[gp] $s3, 65536($gp)	# CHECK: lhu $s3, 65536($gp) # encoding: [0x71,0x46,0x01,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHUGP_NM
				# DISAS: {{.*}}  71 46 01 00  	lhu	$s3, 65536($gp)
	lhu[gp] $s0, 262142($gp)	# CHECK: lhu $s0, 262142($gp) # encoding: [0x13,0x46,0xff,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUGP_NM
					# DISAS: {{.*}}  13 46 ff ff  	lhu	$s0, 262142($gp)

	sh[16]	$a0, 0($s3)	# CHECK: sh $a0, 0($s3) # encoding: [0x31,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
				# DISAS: {{.*}}  31 7e        	sh	$a0, 0($s3)
	sh[16]	$a1, 2($s2)	# CHECK: sh $a1, 2($s2) # encoding: [0xa3,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
				# DISAS: {{.*}}  a3 7e        	sh	$a1, 2($s2)
	sh[16]	$a2, 4($s1)	# CHECK: sh $a2, 4($s1) # encoding: [0x15,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
				# DISAS: {{.*}}  15 7f        	sh	$a2, 4($s1)
	sh[16]	$zero, 6($s0)	# CHECK: sh $zero, 6($s0) # encoding: [0x07,0x7c]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
				# DISAS: {{.*}}  07 7c        	sh	$zero, 6($s0)

	sh[s9]	$a2, -4($s1)    # CHECK: sh $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0xa8]
				# CHECK-NEXT: <MCInst  #{{.*}} SHs9_NM
				# DISAS: {{.*}}  d1 a4 fc a8  	sh	$a2, -4($s1)
	sh[s9]	$a3, -256($s5)  # CHECK: sh $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xa8]
				# CHECK-NEXT: <MCInst  #{{.*}} SHs9_NM
				# DISAS: {{.*}}  f5 a4 00 a8  	sh	$a3, -256($s5)
	sh[s9]	$a4, 8($s0)     # CHECK: sh $a4, 8($s0) # encoding: [0x10,0xa5,0x08,0x28]
				# CHECK-NEXT: <MCInst  #{{.*}} SHs9_NM
				# DISAS: {{.*}}  10 a5 08 28  	sh	$a4, 8($s0)
	sh[s9]	$a5, 254($s2)   # CHECK: sh $a5, 254($s2) # encoding: [0x32,0xa5,0xfe,0x28]
				# CHECK-NEXT: <MCInst  #{{.*}} SHs9_NM
				# DISAS: {{.*}}  32 a5 fe 28  	sh	$a5, 254($s2)
	sh[u12]	$a4, 8($s0)     # CHECK: sh $a4, 8($s0) # encoding: [0x10,0x85,0x08,0x50]
				# CHECK-NEXT: <MCInst  #{{.*}} SH_NM
				# DISAS: {{.*}}  10 85 08 50  	sh	$a4, 8($s0)
	sh[u12] $a5, 254($s2)   # CHECK: sh $a5, 254($s2) # encoding: [0x32,0x85,0xfe,0x50]
				# CHECK-NEXT: <MCInst  #{{.*}} SH_NM
				# DISAS: {{.*}}  32 85 fe 50  	sh	$a5, 254($s2)
	sh[u12] $s3, 4094($t5)  # CHECK: sh $s3, 4094($t5) # encoding: [0x63,0x86,0xfe,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} SH_NM
				# DISAS: {{.*}}  63 86 fe 5f  	sh	$s3, 4094($t5)
	sh[gp]	$a0, 0($gp)	# CHECK: sh $a0, 0($gp) # encoding: [0x94,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SHGP_NM
				# DISAS: {{.*}}  94 44 00 00  	sh	$a0, 0($gp)
	sh[gp]	$s3, 65536($gp)	# CHECK: sh $s3, 65536($gp) # encoding: [0x75,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SHGP_NM
				# DISAS: {{.*}}  75 46 00 00  	sh	$s3, 65536($gp)
	sh[gp]	$s0, 262142($gp)	# CHECK: sh $s0, 262142($gp) # encoding: [0x17,0x46,0xfe,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} SHGP_NM
					# DISAS: {{.*}}  17 46 fe ff  	sh	$s0, 262142($gp)

	swpc[48] $t0, test	# CHECK: swpc $t0, test	# encoding: [0x8f,0x61,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst  #{{.*}} SWPC_NM
				# DISAS: {{.*}}  8f 61 00 00 00 00    	swpc	$t0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC_I32	test
	swpc[48] $t0, test-1048576	# CHECK: swpc $t0, test-1048576	# encoding: [0x8f,0x61,A,A,A,A]
					# CHECK-NEXT: fixup A - offset: 2, value: test-1048576, kind: fixup_NANOMIPS_PC_I32
					# CHECK-NEXT: <MCInst  #{{.*}} SWPC_NM
					# DISAS: {{.*}}  8f 61 00 00 00 00    	swpc	$t0, 0x{{.*}}
					# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC_I32	test-0x100000
	lwpc[48] $t5, test	# CHECK: lwpc $t5, test	# encoding: [0x6b,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst  #{{.*}} LWPC_NM
				# DISAS: {{.*}}  6b 60 00 00 00 00    	lwpc	$t5, 0x{{.*}}
				# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC_I32	test
	lwpc[48] $t5, test+2147483647	# CHECK: lwpc $t5, test+2147483647	# encoding: [0x6b,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test+2147483647, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst  #{{.*}} LWPC_NM
				# DISAS: {{.*}}  6b 60 00 00 00 00    	lwpc	$t5, 0{{.*}}
				# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC_I32	test+0x7fffffff

	addiu[gp.b] $t0, $gp, %gp_rel(test)	# CHECK: addiu.b	$t0, $gp, %gp_rel(test) # encoding: [0b100011AA,0x45,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGPB_NM
					# DISAS: {{.*}}  8c 45 00 00  	addiu.b	$t0, $gp, 0
	addiu[gp.b] $t1, $gp, %gp_rel(test)	# CHECK: addiu.b	$t1, $gp, %gp_rel(test) # encoding: [0b101011AA,0x45,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGPB_NM
					# DISAS: {{.*}}  ac 45 00 00  	addiu.b	$t1, $gp, 0
	addiu[gp.w] $t2, $gp, %gp_rel(test)	# CHECK: addiu.w	$t2, $gp, %gp_rel(test) # encoding: [0b11000AAA,0x41,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL19_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGPW_NM
					# DISAS: {{.*}}  c0 41 00 00  	addiu.w	$t2, $gp, 0
	addiu[gp48] $t3, $gp, %gp_rel(test)	# CHECK: addiu.b32	$t3, $gp, %gp_rel(test) # encoding: [0xe2,0x61,A,A,A,A]
                                        # CHECK-NEXT: fixup A - offset: 2, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL_I32
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGP48_NM
					# DISAS: {{.*}}  e2 61 00 00 00 00    	addiu.b32	$t3, $gp, 0

	lw[gp16] $a0, %gp_rel(test)($gp)	# CHECK: lw	$a0, %gp_rel(test)($gp) # encoding: [0b0AAAAAAA,0x56]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL7_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} LWGP16_NM
					# DISAS: {{.*}}  00 56        	lw	$a0, 0($gp)
	sw[gp16] $s1, %gp_rel(test)($gp)	# CHECK: sw	$s1, %gp_rel(test)($gp) # encoding: [0b1AAAAAAA,0xd4]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL7_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} SWGP16_NM
					# DISAS: {{.*}}  80 d4        	sw	$s1, 0($gp)
	lw[gp]	$t0, %gp_rel(test)($gp)	# CHECK: lw	$t0, %gp_rel(test)($gp) # encoding: [0b10000AAA,0x41,0x02'A',A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL19_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} LWGP_NM
					# DISAS: {{.*}}  80 41 02 00  	lw	$t0, 0($gp)
	lh[gp]	$a1, %gp_rel(test)($gp)	# CHECK: lh	$a1, %gp_rel(test)($gp) # encoding: [0b1011000A,0x44,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL17_S1
                                        # CHECK-NEXT: <MCInst #{{.*}} LHGP_NM
					# DISAS: {{.*}}  b0 44 00 00  	lh	$a1, 0($gp)
	lhu[gp] $a2, %gp_rel(test)($gp)	# CHECK: lhu	$a2, %gp_rel(test)($gp) # encoding: [0b1101000A,0x44,0x01'A',A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL17_S1
                                        # CHECK-NEXT: <MCInst #{{.*}} LHUGP_NM
					# DISAS: {{.*}}  d0 44 01 00  	lhu	$a2, 0($gp)
	lb[gp]	$a3, %gp_rel(test)($gp)	# CHECK: lb	$a3, %gp_rel(test)($gp) # encoding: [0b111000AA,0x44,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} LBGP_NM
					# DISAS: {{.*}}  e0 44 00 00  	lb	$a3, 0($gp)
	lbu[gp]	$a4, %gp_rel(test)($gp)	# CHECK: lbu	$a4, %gp_rel(test)($gp) # encoding: [0b000010AA,0x45,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} LBUGP_NM
					# DISAS: {{.*}}  08 45 00 00  	lbu	$a4, 0($gp)
	sw[gp]	$t1, %gp_rel(test)($gp)	# CHECK: sw	$t1, %gp_rel(test)($gp) # encoding: [0b10100AAA,0x41,0x03'A',A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL19_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} SWGP_NM
					# DISAS: {{.*}}  a0 41 03 00  	sw	$t1, 0($gp)
	sh[gp]	$s1, %gp_rel(test)($gp)	# CHECK: sh	$s1, %gp_rel(test)($gp) # encoding: [0b0011010A,0x46,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL17_S1
                                        # CHECK-NEXT: <MCInst #{{.*}} SHGP_NM
					# DISAS: {{.*}}  34 46 00 00  	sh	$s1, 0($gp)
	sb[gp]	$s3, %gp_rel(test)($gp)	# CHECK: sb	$s3, %gp_rel(test)($gp) # encoding: [0b011001AA,0x46,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} SBGP_NM
					# DISAS: {{.*}}  64 46 00 00  	sb	$s3, 0($gp)

	lapc[32] $a0, test	# CHECK: lapc.h $a0, test # encoding: [0b100AAAAA,0x04,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: test, kind: fixup_NANOMIPS_PC21_S1
				# CHECK-NEXT: <MCInst #{{.*}} LAPC32_NM
				# DISAS: {{.*}}  80 04 00 00  	lapc.h	$a0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC21_S1	test
	lapc[32] $a1, test-16	# CHECK: lapc.h $a1, test-16 # encoding: [0b101AAAAA,0x04,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: test-16, kind: fixup_NANOMIPS_PC21_S1
				# CHECK-NEXT: <MCInst #{{.*}} LAPC32_NM
				# DISAS: {{.*}}  a0 04 00 00  	lapc.h	$a1, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC21_S1	test-0x10
	lapc[48] $a3, test-16	# CHECK: lapc.b $a3, test-16 # encoding: [0xe3,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test-16, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst #{{.*}} LAPC48_NM
				# DISAS: {{.*}}  e3 60 00 00 00 00    	lapc.b	$a3, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC_I32	test-0x10

1:
	balc[16] test	# CHECK: balc[16] test # encoding: [A,0b001110AA]
			# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC10_S1
			# CHECK-NEXT: <MCInst #{{.*}} BALC16_NM
			# DISAS: {{.*}}  00 38        	balc[16]	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC10_S1	test
	balc[32] test	# CHECK: balc test # encoding: [A,0b0010101A,A,A]
			# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC25_S1
			# CHECK-NEXT: <MCInst #{{.*}} BALC_NM
			# DISAS: {{.*}}  00 2a 00 00  	balc	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC25_S1	test
	balc[32] 0x1ff0001	# CHECK: balc 0x1ff0001 # encoding: [A,0b0010101A,A,A]
			# CHECK-NEXT: fixup A - offset: 0, value: 33488897, kind: fixup_NANOMIPS_PC25_S1
			# CHECK-NEXT: <MCInst #{{.*}} BALC_NM
			# DISAS: {{.*}}  00 2a 00 00  	balc	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC25_S1	*ABS*+0x1ff0001

	bc[16]	1b	# CHECK: bc .Ltmp0 # encoding: [A,0b000110AA]
			# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC10_S1
			# CHECK-NEXT: <MCInst #{{.*}} BC16_NM
			# DISAS: {{.*}}  00 18        	bc	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC10_S1	.Ltmp0
	bc[16]	1f	# CHECK: bc .Ltmp1 # encoding: [A,0b000110AA]
			# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC10_S1
			# CHECK-NEXT: <MCInst #{{.*}} BC16_NM
			# DISAS: {{.*}}  00 18        	bc	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC10_S1	.Ltmp1
	beqc[16] $a1, $a2, 2f	# CHECK: beqc $a2, $a1, .Ltmp2 # encoding: [0b0101AAAA,0xdb]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC16_NM
				# DISAS: {{.*}}  5f db        	beqc	$a1, $a2, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC4_S1	.Ltmp2
	beqc[32] $a4, $a1, 1f	# CHECK: beqc $a4, $a1, .Ltmp1 # encoding: [0xa8,0x88,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC_NM
				# DISAS: {{.*}}  a8 88 00 00  	beqc	$a4, $a1, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp1
	bnec[16] $a2, $a0, 2f	# CHECK: bnec $a0, $a2, .Ltmp2 # encoding: [0b0110AAAA,0xda]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC16_NM
				# DISAS: {{.*}}  6f da        	bnec	$a2, $a0, {{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC4_S1	.Ltmp2
	bnec[32] $a0, $s5, 1f	# CHECK: bnec $a0, $s5, .Ltmp1 # encoding: [0xa4,0xaa,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC_NM
				# DISAS: {{.*}}  a4 aa 00 00  	bnec	$a0, $s5, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp1
2:
	beqzc[16] $a0, 1f	# CHECK: beqzc $a0, .Ltmp1 # encoding: [0b0AAAAAAA,0x9a]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQZC16_NM
				# DISAS: {{.*}}  00 9a        	beqzc	$a0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC7_S1	.Ltmp1
	beqzc[32] $a4, 1f	# CHECK: beqzc $a4, .Ltmp1 # encoding: [0x00,0x89,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQZC_NM
				# DISAS: {{.*}}  00 89 00 00  	beqzc	$a4, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp1
	bnezc[16] $s0, 1b	# CHECK: bnezc $s0, .Ltmp0 # encoding: [0b0AAAAAAA,0xb8]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEZC16_NM
				# DISAS: {{.*}}  00 b8        	bnezc	$s0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC7_S1	.Ltmp0
	bnezc[32] $s4, 1b	# CHECK: bnezc $s4, .Ltmp0 # encoding: [0x80,0xaa,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEZC_NM
				# DISAS: {{.*}}  80 aa 00 00  	bnezc	$s4, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp0

	jalrc[16] $ra, $a1	# CHECK: jalrc $ra, $a1	# encoding: [0xb0,0xd8]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC16_NM
				# DISAS: {{.*}}  b0 d8        	jalrc	$ra, $a1
	jalrc[16] $ra, $s1	# CHECK: jalrc $ra, $s1	# encoding: [0x30,0xda]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC16_NM
				# DISAS: {{.*}}  30 da        	jalrc	$ra, $s1
	jalrc[32] $t2, $s2	# CHECK: jalrc $t2, $s2	# encoding: [0xd2,0x49,0x00,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC_NM
				# DISAS: {{.*}}  d2 49 00 00  	jalrc	$t2, $s2
	jalrc[32] $a5, $s5	# CHECK: jalrc $a5, $s5	# encoding: [0x35,0x49,0x00,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC_NM
				# DISAS: {{.*}}  35 49 00 00  	jalrc	$a5, $s5

1:
	pref[s9] 0, -4($s4)	# CHECK: pref 0, -4($s4) # encoding: [0x14,0xa4,0xfc,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFs9_NM
				# DISAS: {{.*}}  14 a4 fc 98  	pref	0, -4($s4)
	pref[s9] 1, -256($s0)	# CHECK: pref 1, -256($s0) # encoding: [0x30,0xa4,0x00,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFs9_NM
				# DISAS: {{.*}}  30 a4 00 98  	pref	1, -256($s0)
	pref[s9] 12, 64($s3)	# CHECK: pref 12, 64($s3) # encoding: [0x93,0xa5,0x40,0x18]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFs9_NM
				# DISAS: {{.*}}  93 a5 40 18  	pref	12, 64($s3)
	pref[s9] 17, 252($s2)	# CHECK: pref 17, 252($s2) # encoding: [0x32,0xa6,0xfc,0x18]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFs9_NM
				# DISAS: {{.*}}  32 a6 fc 18  	pref	17, 252($s2)
	pref[u12] 12, 64($s3)	# CHECK: pref 12, 64($s3) # encoding: [0x93,0x85,0x40,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} PREF_NM
				# DISAS: {{.*}}  93 85 40 30  	pref	12, 64($s3)
	pref[u12] 17, 252($s2)	# CHECK: pref 17, 252($s2) # encoding: [0x32,0x86,0xfc,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} PREF_NM
				# DISAS: {{.*}}  32 86 fc 30  	pref	17, 252($s2)
	pref[u12] 30, 4092($t5)	# CHECK: pref 30, 4092($t5) # encoding: [0xc3,0x87,0xfc,0x3f]
				# CHECK-NEXT: <MCInst  #{{.*}} PREF_NM
				# DISAS: {{.*}}  c3 87 fc 3f  	pref	30, 4092($t5)

	synci[s9] -4($s4)	# CHECK: synci -4($s4) # encoding: [0xf4,0xa7,0xfc,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIs9_NM
				# DISAS: {{.*}}  f4 a7 fc 98  	synci	-4($s4)
	synci[s9] -256($s0)	# CHECK: synci -256($s0) # encoding: [0xf0,0xa7,0x00,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIs9_NM
				# DISAS: {{.*}}  f0 a7 00 98  	synci	-256($s0)
	synci[s9] 64($s3)	# CHECK: synci 64($s3) # encoding: [0xf3,0xa7,0x40,0x18]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIs9_NM
				# DISAS: {{.*}}  f3 a7 40 18  	synci	64($s3)
	synci[s9] 252($s2)	# CHECK: synci 252($s2) # encoding: [0xf2,0xa7,0xfc,0x18]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIs9_NM
				# DISAS: {{.*}}  f2 a7 fc 18  	synci	252($s2)
	synci[u12] 64($s3)	# CHECK: synci 64($s3) # encoding: [0xf3,0x87,0x40,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCI_NM
				# DISAS: {{.*}}  f3 87 40 30  	synci	64($s3)
	synci[u12] 252($s2)	# CHECK: synci 252($s2) # encoding: [0xf2,0x87,0xfc,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCI_NM
				# DISAS: {{.*}}  f2 87 fc 30  	synci	252($s2)
	synci[u12] 4092($t5)	# CHECK: synci 4092($t5) # encoding: [0xe3,0x87,0xfc,0x3f]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCI_NM
				# DISAS: {{.*}}  e3 87 fc 3f  	synci	4092($t5)
	addiupc[32] $a0, 2097154	# CHECK: addiupc $a0, 0x200002 # encoding: [0x9f,0x04,0xfe,0xff]
                                # CHECK-NEXT: <MCInst #{{.*}} ADDIUPC_NM
				# DISAS: {{.*}}  9f 04 fe ff  	lapc.h	$a0, 0x{{.*}}
	addiupc[48] $a0, 2097154	# CHECK: addiupc $a0, 0x200002 # encoding: [0x83,0x60,0xfc,0xff,0x1f,0x00]
                                # CHECK-NEXT: <MCInst #{{.*}} ADDIUPC48_NM
				# DISAS: {{.*}}  83 60 fc ff 1f 00  	lapc.b	$a0, 0x{{.*}}
	addiupc[48] $a0, 2097156	# CHECK: addiupc $a0, 0x200004 # encoding: [0x83,0x60,0xfe,0xff,0x1f,0x00]
                                # CHECK-NEXT: <MCInst #{{.*}} ADDIUPC48_NM
				# DISAS: {{.*}}  83 60 fe ff 1f 00  	lapc.b	$a0, 0x{{.*}}
	addiupc[32] $a0, -2097148	# CHECK: addiupc $a0, 0xffe00004 # encoding: [0x80,0x04,0x01,0x00]
                                # CHECK-NEXT: <MCInst #{{.*}} ADDIUPC_NM
				# DISAS: {{.*}}  80 04 01 00  	lapc.h	$a0, 0x{{.*}}
	addiupc[48] $a0, -2097148	# CHECK: addiupc $a0, 0xffe00004 # encoding: [0x83,0x60,0xfe,0xff,0xdf,0xff]
                                # CHECK-NEXT: <MCInst #{{.*}} ADDIUPC48_NM
				# DISAS: {{.*}}  83 60 fe ff df ff  	lapc.b	$a0, 0x{{.*}}
	addiupc[48] $a0, -2097150	# CHECK: addiupc $a0, 0xffe00002 # encoding: [0x83,0x60,0xfc,0xff,0xdf,0xff]
                                # CHECK-NEXT: <MCInst #{{.*}} ADDIUPC48_NM
				# DISAS: {{.*}}  83 60 fc ff df ff  	lapc.b	$a0, 0x{{.*}}
	jrc $ra
	.type   g_8,@object
	.comm   g_8,16,16
