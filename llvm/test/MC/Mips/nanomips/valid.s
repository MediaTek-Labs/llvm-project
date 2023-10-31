# Instructions that are valid
#
# Branches have some unusual encoding rules in nanoMIPS so we need to test:
#   rs == 0
#   rs != 0
#   rt == 0
#   rt != 0
#   rs < rt
#   rs == rt
#   rs > rt
# appropriately for each branch instruction
#
# RUN: llvm-mc %s -triple=nanomips-elf -show-encoding -show-inst 2> %t0 | FileCheck %s
	.text
	# CHECK: .text
	.set noat
#	.linkrelax
	# reg3-reg3 arithmetic, 16-bit
	addu	$a1, $s2, $a3	# CHECK: addu	$a1, $s2, $a3	# encoding: [0xaa,0xb3]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu16_NM
	subu	$a0, $a2, $a3	# CHECK: subu	$a0, $a2, $a3	# encoding: [0xe9,0xb3]
				# CHECK-NEXT: # <MCInst #{{.*}} SUBu16_NM

	# reg4-reg4 arithmetic, 16-bit
	addu	$a1, $a1, $a7	# CHECK: addu $a1, $a1, $a7	# encoding: [0xa3,0x3c]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu4x4_NM
	mul	$s2, $s2, $a3	# CHECK: mul $s2, $s2, $a3	# encoding: [0x4f,0x3e]
				# CHECK-NEXT: # <MCInst #{{.*}} MUL4x4_NM

	# 16-bit 4x4 with operands commuted
	addu	$a1, $a7, $a1	# CHECK: addu $a1, $a1, $a7	# encoding: [0xa3,0x3c]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu4x4_NM
	mul	$s2, $a3, $s2	# CHECK: mul $s2, $s2, $a3	# encoding: [0x4f,0x3e]
				# CHECK-NEXT: # <MCInst #{{.*}} MUL4x4_NM

	# reg-reg arithmetic, 32-bit
	add	$a1, $a2, $a3	# CHECK: add $a1, $a2, $a3	# encoding: [0xe6,0x20,0x10,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} ADD_NM
	addu	$a1, $s2, $t3	# CHECK: addu $a1, $s2, $t3	# encoding: [0xf2,0x21,0x50,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu_NM
	sub	$a1, $s2, $a3	# CHECK: sub $a1, $s2, $a3	# encoding: [0xf2,0x20,0x90,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} SUB_NM
	subu	$a0, $a2, $t3	# CHECK: subu $a0, $a2, $t3	# encoding: [0xe6,0x21,0xd0,0x21]
				# CHECK-NEXT: # <MCInst #{{.*}} SUBu_NM
	mul	$t1, $s2, $a3	# CHECK: mul $t1, $s2, $a3	# encoding: [0xf2,0x20,0x18,0x68]
				# CHECK-NEXT: # <MCInst #{{.*}} MUL_NM
	muh	$a1, $s2, $a3	# CHECK: muh $a1, $s2, $a3	# encoding: [0xf2,0x20,0x58,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} MUH_NM
	mulu	$k1, $s2, $a3	# CHECK: mulu $k1, $s2, $a3	# encoding: [0xf2,0x20,0x98,0xd8]
				# CHECK-NEXT: # <MCInst #{{.*}} MULU_NM
	muhu	$a1, $s2, $a3	# CHECK: muhu $a1, $s2, $a3	# encoding: [0xf2,0x20,0xd8,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} MUHU_NM
	div	$a1, $s2, $t0	# CHECK: div $a1, $s2, $t0	# encoding: [0x92,0x21,0x18,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} DIV_NM
	divu	$t4, $s2, $a3	# CHECK: divu $t4, $s2, $a3	# encoding: [0xf2,0x20,0x98,0x11]
				# CHECK-NEXT: # <MCInst #{{.*}} DIVU_NM
	mod	$a1, $t2, $a3	# CHECK: mod $a1, $t2, $a3	# encoding: [0xee,0x20,0x58,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} MOD_NM
	modu	$a1, $s2, $t5	# CHECK: modu $a1, $s2, $t5	# encoding: [0x72,0x20,0xd8,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} MODU_NM

	# reg-reg logic, 16-bit
	and	$a1, $a3, $a1	# CHECK: and $a1, $a3, $a1	# encoding: [0xf8,0x52]
				# CHECK-NEXT: # <MCInst #{{.*}} AND16_NM
	or	$s2, $a3, $s2	# CHECK: or $s2, $a3, $s2	# encoding: [0x7c,0x51]
				# CHECK-NEXT: # <MCInst #{{.*}} OR16_NM
	xor	$a3, $s3, $a3	# CHECK: xor $a3, $s3, $a3	# encoding: [0xb4,0x53]
				# CHECK-NEXT: # <MCInst #{{.*}} XOR16_NM
	not	$a3, $s3	# CHECK: not $a3, $s3		# encoding: [0xb0,0x53]
				# CHECK-NEXT: # <MCInst #{{.*}} NOT16_NM

	# 16-bit with operands commuted
	and	$a1, $a1, $a3	# CHECK: and $a1, $a3, $a1	# encoding: [0xf8,0x52]
				# CHECK-NEXT: # <MCInst #{{.*}} AND16_NM
	or	$s2, $s2, $a3	# CHECK: or $s2, $a3, $s2	# encoding: [0x7c,0x51]
				# CHECK-NEXT: # <MCInst #{{.*}} OR16_NM
	xor	$a3, $a3, $s3	# CHECK: xor $a3, $s3, $a3	# encoding: [0xb4,0x53]
				# CHECK-NEXT: # <MCInst #{{.*}} XOR16_NM

	# reg-reg logic, 32-bit
	and	$a1, $s4, $a3	# CHECK: and $a1, $s4, $a3	# encoding: [0xf4,0x20,0x50,0x2a]
				# CHECK-NEXT: # <MCInst #{{.*}} AND_NM
	or	$a1, $t2, $a3	# CHECK: or $a1, $t2, $a3	# encoding: [0xee,0x20,0x90,0x2a]
				# CHECK-NEXT: # <MCInst #{{.*}} OR_NM
	nor	$a3, $t3, $s3	# CHECK: nor $a3, $t3, $s3	# encoding: [0x6f,0x22,0xd0,0x3a]
				# CHECK-NEXT: # <MCInst #{{.*}} NOR_NM
	xor	$t0, $s2, $a3	# CHECK: xor $t0, $s2, $a3	# encoding: [0xf2,0x20,0x10,0x63]
				# CHECK-NEXT: # <MCInst #{{.*}} XOR_NM
	not	$a7, $s7	# CHECK: not $a7, $s7	# encoding: [0x17,0x20,0xd0,0x5a]
				# CHECK-NEXT: # <MCInst #{{.*}} NOR_NM

	# reg-reg bitwise, 32-bit
	sllv	$a1, $a7, $a3	# CHECK: sllv $a1, $a7, $a3	# encoding: [0xeb,0x20,0x10,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} SLLV_NM
	srlv	$a1, $s1, $a3	# CHECK: srlv $a1, $s1, $a3	# encoding: [0xf1,0x20,0x50,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} SRLV_NM
	srav	$a1, $s2, $a3	# CHECK: srav $a1, $s2, $a3	# encoding: [0xf2,0x20,0x90,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} SRAV_NM
	rotrv	$a1, $s2, $a3	# CHECK: rotrv $a1, $s2, $a3	# encoding: [0xf2,0x20,0xd0,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTRV_NM
	slt	$a1, $s2, $a3	# CHECK: slt $a1, $s2, $a3	# encoding: [0xf2,0x20,0x50,0x2b]
				# CHECK-NEXT: # <MCInst #{{.*}} SLT_NM
	sltu	$a1, $s2, $a3	# CHECK: sltu $a1, $s2, $a3	# encoding: [0xf2,0x20,0x90,0x2b]
				# CHECK-NEXT: # <MCInst #{{.*}} SLTU_NM
	sov	$a1, $s3, $t2	# CHECK: sov $a1, $s3, $t2	# encoding: [0xd3,0x21,0xd0,0x2b]
				# CHECK-NEXT: # <MCInst #{{.*}} SOV_NM

	# reg-imm logic, 16-bit
	sll	$a1, $s2, 1	# CHECK: sll $a1, $s2, 1	# encoding: [0xa1,0x32]
				# CHECK-NEXT: # <MCInst #{{.*}} SLL16_NM
	srl	$s1, $a3, 8	# CHECK: srl $s1, $a3, 8	# encoding: [0xf8,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SRL16_NM
	andi	$a0, $a1, 0	# CHECK: andi $a0, $a1, 0	# encoding: [0x50,0xf2]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
	andi	$a1, $a2, 1	# CHECK: andi $a1, $a2, 1	# encoding: [0xe1,0xf2]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
	andi	$a2, $a3, 6	# CHECK: andi $a2, $a3, 6	# encoding: [0x76,0xf3]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
	andi	$a3, $s0, 0xff	# CHECK: andi $a3, $s0, 255	# encoding: [0x8c,0xf3]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
	andi	$s1, $s3, 0xffff	# CHECK: andi $s1, $s3, 65535	# encoding: [0xbd,0xf0]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
	andi	$s3, $a3, 0xe	# CHECK: andi $s3, $a3, 14	# encoding: [0xfe,0xf1]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
	andi	$s2, $a1, 0xf	# CHECK: andi $s2, $a1, 15	# encoding: [0x5f,0xf1]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM

	# reg-imm logic, 32-bit
	andi	$a4, $a1, 0	# CHECK: andi $a4, $a1, 0	# encoding: [0x05,0x81,0x00,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	andi	$a1, $t2, 1	# CHECK: andi $a1, $t2, 1	# encoding: [0xae,0x80,0x01,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	andi	$a2, $t3, 6	# CHECK: andi $a2, $t3, 6	# encoding: [0xcf,0x80,0x06,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	andi	$a6, $s0, 0xff	# CHECK: andi $a6, $s0, 255	# encoding: [0x50,0x81,0xff,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	andi	$s1, $s4, 0xfff	# CHECK: andi $s1, $s4, 4095	# encoding: [0x34,0x82,0xff,0x2f]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	andi	$s3, $a4, 0xe	# CHECK: andi $s3, $a4, 14	# encoding: [0x68,0x82,0x0e,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	andi	$s2, $t0, 0xf	# CHECK: andi $s2, $t0, 15	# encoding: [0x4c,0x82,0x0f,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	andi	$a3, $s3, 12	# CHECK: andi $a3, $s3, 12	# encoding: [0xf3,0x80,0x0c,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	andi	$a1, $s2, 13	# CHECK: andi $a1, $s2, 13	# encoding: [0xb2,0x80,0x0d,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	andi	$a2, $s1, 16	# CHECK: andi $a2, $s1, 16	# encoding: [0xd1,0x80,0x10,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
	ori	$a1, $s5, 0x1	# CHECK: ori $a1, $s5, 1	# encoding: [0xb5,0x80,0x01,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} ORI_NM
	xori	$t1, $s2, 0xfff # CHECK: xori $t1, $s2, 4095	# encoding: [0xb2,0x81,0xff,0x1f]
				# CHECK-NEXT: # <MCInst #{{.*}} XORI_NM

	# reg-imm bit-wise, 32-bit
	slti	$a1, $s2, 0xff	# CHECK: slti $a1, $s2, 255	# encoding: [0xb2,0x80,0xff,0x40]
				# CHECK-NEXT: # <MCInst #{{.*}} SLTI_NM
	sltiu	$a1, $s2, 0xfff	# CHECK: sltiu $a1, $s2, 4095	# encoding: [0xb2,0x80,0xff,0x5f]
				# CHECK-NEXT: # <MCInst #{{.*}} SLTIU_NM
	seqi	$a1, $s2, 0xfff	# CHECK: seqi $a1, $s2, 4095	# encoding: [0xb2,0x80,0xff,0x6f]
				# CHECK-NEXT: # <MCInst #{{.*}} SEQI_NM
	sll	$a1, $s2, 9	# CHECK: sll $a1, $s2, 9	# encoding: [0xb2,0x80,0x09,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SLL_NM
	sll	$a1, $t2, 1	# CHECK: sll $a1, $t2, 1	# encoding: [0xae,0x80,0x01,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SLL_NM
	srl	$a1, $a2, 31	# CHECK: srl $a1, $a2, 31	# encoding: [0xa6,0x80,0x5f,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SRL_NM
	srl	$a1, $a7, 8	# CHECK: srl $a1, $a7, 8	# encoding: [0xab,0x80,0x48,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SRL_NM
	sra	$a1, $a7, 15	# CHECK: sra $a1, $a7, 15	# encoding: [0xab,0x80,0x8f,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SRA_NM
	rotr	$a1, $s2, 31	# CHECK: rotr $a1, $s2, 31	# encoding: [0xb2,0x80,0xdf,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTR_NM
	ext	$a1, $s2, 0, 1	# CHECK: ext $a1, $s2, 0, 1	# encoding: [0xb2,0x80,0x00,0xf0]
				# CHECK-NEXT: # <MCInst #{{.*}} EXT_NM
	ext	$a1, $s2, 16, 15	# CHECK: ext $a1, $s2, 16, 15	# encoding: [0xb2,0x80,0x90,0xf3]
				# CHECK-NEXT: # <MCInst #{{.*}} EXT_NM
	ext	$a1, $s2, 0, 32	# CHECK: ext $a1, $s2, 0, 32	# encoding: [0xb2,0x80,0xc0,0xf7]
				# CHECK-NEXT: # <MCInst #{{.*}} EXT_NM
	ins	$a1, $s2, 31, 1	# CHECK: ins $a1, $s2, 31, 1	# encoding: [0xb2,0x80,0xdf,0xe7]
				# CHECK-NEXT: # <MCInst #{{.*}} INS_NM
	ins	$a1, $s2, 15, 16	# CHECK: ins $a1, $s2, 15, 16	# encoding: [0xb2,0x80,0x8f,0xe7]
				# CHECK-NEXT: # <MCInst #{{.*}} INS_NM
	ins	$a1, $s2, 2, 30	# CHECK: ins $a1, $s2, 2, 30	# encoding: [0xb2,0x80,0xc2,0xe7]
				# CHECK-NEXT: # <MCInst #{{.*}} INS_NM

	extw	$a0, $a1, $a2, 2	# CHECK: extw $a0, $a1, $a2, 2 # encoding: [0xc5,0x20,0x9f,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} EXTW_NM
	extw	$a0, $s1, $t5, 0	# CHECK: extw $a0, $s1, $t5, 0	# encoding: [0x71,0x20,0x1f,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} EXTW_NM
	extw	$s1, $k1, $gp, 16	# CHECK: extw $s1, $k1, $gp, 16	# encoding: [0x9b,0x23,0x1f,0x8c]
				# CHECK-NEXT: # <MCInst #{{.*}} EXTW_NM
	extw	$a7, $s7, $t4, 31	# CHECK: extw $a7, $s7, $t4, 31	# encoding: [0x57,0x20,0xdf,0x5f]
				# CHECK-NEXT: # <MCInst #{{.*}} EXTW_NM
	rotx	$a1, $s3, 0, 0, 0	# CHECK: rotx $a1, $s3, 0, 0, 0	# encoding: [0xb3,0x80,0x00,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
	rotx	$a7, $s4, 1, 2, 1	# CHECK: rotx $a7, $s4, 1, 2, 1	# encoding: [0x74,0x81,0xc1,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
	rotx	$t8, $t0, 31, 6, 1	# CHECK: rotx $t8, $t0, 31, 6, 1 # encoding: [0x0c,0x83,0xdf,0xd1]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
	rotx	$a1, $s3, 3, 16, 0	# CHECK: rotx $a1, $s3, 3, 16, 0 # encoding: [0xb3,0x80,0x03,0xd4]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
	rotx	$a7, $s4, 16, 8, 0	# CHECK: rotx $a7, $s4, 16, 8, 0 # encoding: [0x74,0x81,0x10,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
	rotx	$t8, $t0, 25, 30, 1	# CHECK: rotx $t8, $t0, 25, 30, 1 # encoding: [0x0c,0x83,0xd9,0xd7]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
	bitrevb $a0, $s0	# CHECK: bitrevb $a0, $s0 # encoding: [0x90,0x80,0x47,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
	bitrevh $a1, $s1	# CHECK: bitrevh $a1, $s1 # encoding: [0xb1,0x80,0x0f,0xd4]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
	bitrevw $t0, $t3	# CHECK: bitrevw $t0, $t3 # encoding: [0x8f,0x81,0x1f,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} BITREVW_NM
	byterevh $a3, $s3	# CHECK: byterevh $a3, $s3 # encoding: [0xf3,0x80,0x08,0xd6]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
	byterevw $t0, $k0	# CHECK: byterevw $t0, $k0 # encoding: [0x9a,0x81,0x18,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} BYTEREVW_NM

	# compare and trap, 32-bit
	teq	$s2, $a3, 0	# CHECK: teq $s2, $a3, 0	# encoding: [0xf2,0x20,0x00,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} TEQ_NM
	teq	$s2, $a3, 31	# CHECK: teq $s2, $a3, 31	# encoding: [0xf2,0x20,0x00,0xf8]
				# CHECK-NEXT: # <MCInst #{{.*}} TEQ_NM
	tne	$s2, $a3, 15	# CHECK: tne $s2, $a3, 15	# encoding: [0xf2,0x20,0x00,0x7c]
				# CHECK-NEXT: # <MCInst #{{.*}} TNE_NM
	tne	$s2, $a3, 1	# CHECK: tne $s2, $a3, 1	# encoding: [0xf2,0x20,0x00,0x0c]
				# CHECK-NEXT: # <MCInst #{{.*}} TNE_NM

	# single and paired moves
	move	$a4, $a7	# CHECK: move $a4, $a7	# encoding: [0x0b,0x11]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVE_NM
	move	$s0, $k0	# CHECK: move $s0, $k0	# encoding: [0x1a,0x12]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVE_NM
	move	$t4, $gp	# CHECK: move $t4, $gp	# encoding: [0x5c,0x10]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVE_NM
	movep	$s0, $s1, $a0, $a1	# CHECK: movep $s0, $s1, $a0, $a1	# encoding: [0x30,0xfe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
	movep	$a7, $s7, $a1, $a2	# CHECK: movep $a7, $s7, $a1, $a2	# encoding: [0xe3,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
	movep	$a1, $s3, $a2, $a3	# CHECK: movep $a1, $s3, $a2, $a3	# encoding: [0x6d,0xfe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
	movep	$a6, $s0, $a3, $a4	# CHECK: movep $a6, $s0, $a3, $a4	# encoding: [0x0a,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
	movep	$a0, $a1, $s0, $s1	# CHECK: movep $a0, $a1, $s0, $s1	# encoding: [0x30,0xbe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEP_NM
	movep	$a1, $a2, $zero, $s7	# CHECK: movep $a1, $a2, $zero, $s7	# encoding: [0xe3,0xbf]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEP_NM
	movep	$a2, $a3, $a1, $s3	# CHECK: movep $a2, $a3, $a1, $s3	# encoding: [0x6d,0xbe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEP_NM
	movep	$a3, $a4, $a6, $s0	# CHECK: movep $a3, $a4, $a6, $s0	# encoding: [0x0a,0xbf]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEP_NM

	li	$a0, 1		# CHECK: li $a0, 1	# encoding: [0x01,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
	li	$a1, 64		# CHECK: li $a1, 64	# encoding: [0xc0,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
	li	$a3, 0		# CHECK: li $a3, 0	# encoding: [0x80,0xd3]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
	li	$s0, 126	# CHECK: li $s0, 126	# encoding: [0x7e,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
	li	$s1, -1		# CHECK: li $s1, -1	# encoding: [0xff,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
	li	$a2, 127	# CHECK: li $a2, 127	# encoding: [0xc0,0x00,0x7f,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIU_NM
	li	$a5, 65535	# CHECK: li $a5, 65535	# encoding: [0x20,0x01,0xff,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIU_NM
	li	$s2, -2		# CHECK: li $s2, -2	# encoding: [0x40,0x82,0x02,0x80]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIUNEG_NM
	li	$s7, -4095	# CHECK: li $s7, -4095	# encoding: [0xe0,0x82,0xff,0x8f]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIUNEG_NM
	li	$t0, 65537	# CHECK: li $t0, 65537	# encoding: [0x80,0x61,0x01,0x00,0x01,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
	li	$t1, 2147483647	# CHECK: li $t1, 2147483647	# encoding: [0xa0,0x61,0xff,0xff,0xff,0x7f]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
	li	$t4, -4097	# CHECK: li $t4, -4097	# encoding: [0x40,0x60,0xff,0xef,0xff,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
	li	$t5, -2147483647	# CHECK: li $t5, -2147483647	# encoding: [0x60,0x60,0x01,0x00,0x00,0x80]
					# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
	li	$a0, foo	# CHECK: li $a0, foo	# encoding: [0x80,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: foo, kind: fixup_NANOMIPS_I32
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM

	movn	$a1, $s2, $t4	# CHECK: movn $a1, $s2, $t4	# encoding: [0x52,0x20,0x10,0x2e]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVN_NM
	movn	$a3, $s7, $t8	# CHECK: movn $a3, $s7, $t8	# encoding: [0x17,0x23,0x10,0x3e]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVN_NM
	movn	$at, $t9, $s7	# CHECK: movn $at, $t9, $s7	# encoding: [0xf9,0x22,0x10,0x0e]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVN_NM
	movz	$s1, $a2, $gp	# CHECK: movz $s1, $a2, $gp	# encoding: [0x86,0x23,0x10,0x8a]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVZ_NM
	movz	$s3, $a7, $sp	# CHECK: movz $s3, $a7, $sp	# encoding: [0xab,0x23,0x10,0x9a]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVZ_NM
	movz	$s7, $k0, $fp	# CHECK: movz $s7, $k0, $fp	# encoding: [0xda,0x23,0x10,0xba]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVZ_NM

	clo	$a0, $a1	# CHECK: clo $a0, $a1	# encoding: [0x85,0x20,0x3f,0x4b]
				# CHECK-NEXT: # <MCInst #{{.*}} CLO_NM
	clo	$t0, $s1	# CHECK: clo $t0, $s1	# encoding: [0x91,0x21,0x3f,0x4b]
				# CHECK-NEXT: # <MCInst #{{.*}} CLO_NM
	clz	$k0, $s7	# CHECK: clz $k0, $s7	# encoding: [0x57,0x23,0x3f,0x5b]
				# CHECK-NEXT: # <MCInst #{{.*}} CLZ_NM
	clz	$s4, $a0	# CHECK: clz $s4, $a0	# encoding: [0x84,0x22,0x3f,0x5b]
				# CHECK-NEXT: # <MCInst #{{.*}} CLZ_NM

	seb	$a0, $a1	# CHECK: seb $a0, $a1	# encoding: [0x85,0x20,0x08,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} SEB_NM
	seb	$t0, $s1	# CHECK: seb $t0, $s1	# encoding: [0x91,0x21,0x08,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} SEB_NM
	seh	$k0, $s7	# CHECK: seh $k0, $s7	# encoding: [0x57,0x23,0x48,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} SEH_NM
	seh	$s4, $a0	# CHECK: seh $s4, $a0	# encoding: [0x84,0x22,0x48,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} SEH_NM

	lsa $a0, $a1, $a4, 1	# CHECK: lsa $a0, $a1, $a4, 1	# encoding: [0x05,0x21,0x0f,0x22]
				# CHECK-NEXT: # <MCInst #{{.*}} LSA_NM
	lsa $s0, $s1, $s4, 2	# CHECK: lsa $s0, $s1, $s4, 2	# encoding: [0x91,0x22,0x0f,0x84] 
				# CHECK-NEXT: # <MCInst #{{.*}} LSA_NM
	lsa $s3, $a7, $a4, 3	# CHECK: lsa $s3, $a7, $a4, 3	# encoding: [0x0b,0x21,0x0f,0x9e]
				# CHECK-NEXT: # <MCInst #{{.*}} LSA_NM
	lsa $t4, $a3, $s4, 0	# CHECK: lsa $t4, $a3, $s4, 0	# encoding: [0x87,0x22,0x0f,0x10]
				# CHECK-NEXT: # <MCInst #{{.*}} LSA_NM

	di		# CHECK: di # encoding: [0x00,0x20,0x7f,0x47]
			# CHECK-NEXT: # <MCInst #{{.*}} DI_NM
	di $a0		# CHECK: di $a0	# encoding: [0x80,0x20,0x7f,0x47]
			# CHECK-NEXT: # <MCInst #{{.*}} DI_NM
	di $s7		# CHECK: di $s7	# encoding: [0xe0,0x22,0x7f,0x47]
			# CHECK-NEXT: # <MCInst #{{.*}} DI_NM
	ei		# CHECK: ei # encoding: [0x00,0x20,0x7f,0x57]
			# CHECK-NEXT: # <MCInst #{{.*}} EI_NM
	ei $ra		# CHECK: ei $ra	# encoding: [0xe0,0x23,0x7f,0x57]
			# CHECK-NEXT: # <MCInst #{{.*}} EI_NM
	ei $t0		# CHECK: ei $t0	# encoding: [0x80,0x21,0x7f,0x57]
			# CHECK-NEXT: # <MCInst #{{.*}} EI_NM
	eret		# CHECK: eret	# encoding: [0x00,0x20,0x7f,0xf3]
			# CHECK-NEXT: # <MCInst #{{.*}} ERET_NM
	eretnc		# CHECK: eretnc	# encoding: [0x01,0x20,0x7f,0xf3]
			# CHECK-NEXT: # <MCInst #{{.*}} ERETNC_NM
	deret		# CHECK: deret	# encoding: [0x00,0x20,0x7f,0xe3]
			# CHECK-NEXT: # <MCInst #{{.*}} DERET_NM
	nop32		# CHECK: nop32	# encoding: [0x00,0x80,0x00,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} NOP32_NM
	pause		# CHECK: pause	# encoding: [0x00,0x80,0x05,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} PAUSE_NM
	ehb		# CHECK: ehb	# encoding: [0x00,0x80,0x03,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} EHB_NM

	sigrie 0	# CHECK: sigrie	0 # encoding: [0x00,0x00,0x00,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} SIGRIE_NM
	sigrie 0xffff	# CHECK: sigrie	65535 # encoding: [0x00,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SIGRIE_NM
	sigrie 0x7ffff	# CHECK: sigrie	524287 # encoding: [0x07,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SIGRIE_NM
	sdbbp 0		# CHECK: sdbbp	0 # encoding: [0x18,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP16_NM
	sdbbp 7		# CHECK: sdbbp	7 # encoding: [0x1f,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP16_NM
	sdbbp 8		# CHECK: sdbbp	8 # encoding: [0x18,0x00,0x08,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP_NM
	sdbbp 0x7ffff	# CHECK: sdbbp	524287 # encoding: [0x1f,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP_NM
	break 0		# CHECK: break	0 # encoding: [0x10,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK16_NM
	break 7		# CHECK: break	7 # encoding: [0x17,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK16_NM
	break 8		# CHECK: break	8 # encoding: [0x10,0x00,0x08,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK_NM
	break 0x7ffff	# CHECK: break	524287 # encoding: [0x17,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK_NM
	syscall 0	# CHECK: syscall 0 # encoding: [0x08,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL16_NM
	syscall 3	# CHECK: syscall 3 # encoding: [0x0b,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL16_NM
	syscall 4	# CHECK: syscall 4 # encoding: [0x08,0x00,0x04,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL_NM
	syscall 0x3ffff	# CHECK: syscall 262143 # encoding: [0x0b,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL_NM

	rdpgpr $a3, $s7	# CHECK: rdpgpr $a3, $s7 # encoding: [0xf7,0x20,0x7f,0xe1]
			# CHECK-NEXT: # <MCInst #{{.*}} RDPGPR_NM
	rdpgpr $fp, $sp	# CHECK: rdpgpr $fp, $sp # encoding: [0xdd,0x23,0x7f,0xe1]
			# CHECK-NEXT: # <MCInst #{{.*}} RDPGPR_NM
	wrpgpr $t0, $k0	# CHECK: wrpgpr $t0, $k0 # encoding: [0x9a,0x21,0x7f,0xf1]
			# CHECK-NEXT: # <MCInst #{{.*}} WRPGPR_NM
	wrpgpr $a0, $ra	# CHECK: wrpgpr $a0, $ra # encoding: [0x9f,0x20,0x7f,0xf1]
			# CHECK-NEXT: # <MCInst #{{.*}} WRPGPR_NM

	rdhwr	$a0, $0, 31
	rdhwr	$s0, $16, 16
	rdhwr	$a4, $31, 0

	lwm	$a1, 0($a3), 1	# CHECK: lwm $a1, 0($a3), 1 # encoding: [0xa7,0xa4,0x00,0x14]
				# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM
	lwm	$a7, 252($s7), 8	# CHECK: lwm $a7, 252($s7), 8 # encoding: [0x77,0xa5,0xfc,0x04]
					# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM
	lwm	$t0, -256($gp), 8	# CHECK: lwm $t0, -256($gp), 8 # encoding: [0x9c,0xa5,0x00,0x84]
					# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM
	lwm	$a2, 128($a4), 4	# CHECK: lwm $a2, 128($a4), 4 # encoding: [0xc8,0xa4,0x80,0x44]
					# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM
	lwm	$a6, -128($s6), 3	# CHECK: lwm $a6, -128($s6), 3 # encoding: [0x56,0xa5,0x80,0xb4]
					# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM

	swm	$a7, 252($s7), 8	# CHECK: swm $a7, 252($s7), 8 # encoding: [0x77,0xa5,0xfc,0x0c]
					# CHECK-NEXT: # <MCInst #{{.*}} SWM_NM
	swm	$t0, -256($gp), 7	# CHECK: swm $t0, -256($gp), 7 # encoding: [0x9c,0xa5,0x00,0xfc]
					# CHECK-NEXT: # <MCInst #{{.*}} SWM_NM
	swm	$a2, 128($a4), 4	# CHECK: swm $a2, 128($a4), 4 # encoding: [0xc8,0xa4,0x80,0x4c]
					# CHECK-NEXT: # <MCInst #{{.*}} SWM_NM
	swm	$a6, -128($s6), 3	# CHECK: swm $a6, -128($s6), 3 # encoding: [0x56,0xa5,0x80,0xbc]
					# CHECK-NEXT: # <MCInst #{{.*}} SWM_NM

	ualwm	$a1, 0($a3), 1		# CHECK: ualwm $a1, 0($a3), 1 # encoding: [0xa7,0xa4,0x00,0x15]
					# CHECK-NEXT: # <MCInst #{{.*}} UALWM_NM
	ualwm	$a7, 252($s7), 8	# CHECK: ualwm $a7, 252($s7), 8 # encoding: [0x77,0xa5,0xfc,0x05]
					# CHECK-NEXT: # <MCInst #{{.*}} UALWM_NM
	ualwm	$t0, -256($gp), 8	# CHECK: ualwm $t0, -256($gp), 8 # encoding: [0x9c,0xa5,0x00,0x85]
					# CHECK-NEXT: # <MCInst #{{.*}} UALWM_NM
	uaswm	$a2, 128($a4), 4	# CHECK: uaswm $a2, 128($a4), 4 # encoding: [0xc8,0xa4,0x80,0x4d]
					# CHECK-NEXT: # <MCInst #{{.*}} UASWM_NM
	uaswm	$a6, -128($s6), 3	# CHECK: uaswm $a6, -128($s6), 3 # encoding: [0x56,0xa5,0x80,0xbd]
					# CHECK-NEXT: # <MCInst #{{.*}} UASWM_NM

	ualw	$a1,0($a3)	# CHECK: ualw $a1, 0($a3) # encoding: [0xa7,0xa4,0x00,0x15]
				# CHECK-NEXT: # <MCInst #{{.*}} UALW_NM
	uasw	$a7,252($s7)	# CHECK: uasw $a7, 252($s7) # encoding: [0x77,0xa5,0xfc,0x1d]
				# CHECK-NEXT: # <MCInst #{{.*}} UASW_NM
	ualw	$t0,-256($gp)	# CHECK: ualw $t0, -256($gp) # encoding: [0x9c,0xa5,0x00,0x95]
				# CHECK-NEXT: # <MCInst #{{.*}} UALW_NM
	ualw	$a2,128($a4)	# CHECK: ualw $a2, 128($a4) # encoding: [0xc8,0xa4,0x80,0x15]
				# CHECK-NEXT: # <MCInst #{{.*}} UALW_NM

	# 16-bit SAVE/RESTORE[.JRC]
	save	16		# CHECK: save 16, 0 # encoding: [0x10,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
	save	128		# CHECK: save 128, 0 # encoding: [0x80,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
	save	240		# CHECK: save 240, 0 # encoding: [0xf0,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
	save	32, $ra		# CHECK: save 32, $ra # encoding: [0x21,0x1e]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
	save	32, $fp, $ra	# CHECK: save 32, $fp, $ra # encoding: [0x22,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
	save	32, $ra, $s0	# CHECK: save 32, $ra, $s0  # encoding: [0x22,0x1e]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
	save	192, $ra, $s0-$gp	# CHECK: save 192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xce,0x1e]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
	save	192, $fp, $ra, $s0-$gp	# CHECK: save 192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x1c]
					# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
	save	128, $fp, $ra, $s0-$k1, $gp	# CHECK: save 128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0x8f,0x1c]
					# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM

	restore.jrc	16	# CHECK: restore.jrc 16, 0
				# CHECK-NEXT: # encoding: [0x10,0x1d]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	128	# CHECK: restore.jrc 128, 0
				# CHECK-NEXT: # encoding: [0x80,0x1d]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	240	# CHECK: restore.jrc 240, 0
				# CHECK-NEXT: # encoding: [0xf0,0x1d]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	32, $ra	# CHECK: restore.jrc 32, $ra
				# CHECK-NEXT: # encoding: [0x21,0x1f]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	32, $fp, $ra	# CHECK: restore.jrc 32, $fp, $ra
				# CHECK-NEXT: # encoding: [0x22,0x1d]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	32, $ra, $s0	# CHECK: restore.jrc 32, $ra, $s0
					# CHECK-NEXT: # encoding: [0x22,0x1f]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	32, $ra, $s0-$s3	# CHECK: restore.jrc 32, $ra, $s0, $s1, $s2, $s3
						# CHECK-NEXT: # encoding: [0x25,0x1f]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	64, $fp, $ra, $s0-$s7	# CHECK: restore.jrc 64, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7
						# CHECK-NEXT: # encoding: [0x4a,0x1d]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	192, $ra, $s0-$gp	# CHECK: restore.jrc 192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
						# CHECK-NEXT: # encoding: [0xce,0x1f]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	192, $fp, $ra, $s0-$gp	# CHECK: restore.jrc 192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
						# CHECK-NEXT: # encoding: [0xcf,0x1d]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
	restore.jrc	128, $fp, $ra, $s0-$k1, $gp	# CHECK: restore.jrc 128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
						# CHECK-NEXT: # encoding: [0x8f,0x1d]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM

	# 32-bit SAVE/RESTORE[.JRC]
	save	40, $ra		# CHECK: save 40, $ra	# encoding: [0xe1,0x83,0x28,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
	save	256, $fp, $ra	# CHECK: save 256, $fp, $ra # encoding: [0xc2,0x83,0x00,0x31]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
	save	1024, $ra, $s0	# CHECK: save 1024, $ra, $s0 # encoding: [0xe2,0x83,0x00,0x34]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
	save	160, $zero, $gp	# CHECK: save 160, $zero, $gp # encoding: [0x02,0x80,0xa4,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
	save	64, $s0-$s3, $gp # CHECK: save 64, $s0, $s1, $s2, $s3, $gp # encoding: [0x05,0x82,0x44,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
	save	64, $a0-$s0, $gp # CHECK: save 64, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $gp # encoding: [0x8e,0x80,0x44,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
	save	128, $t3, $s0, $s1, $gp # CHECK: save 128, $t3, $s0, $s1, $gp # encoding: [0xe4,0x81,0x84,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
	save	128, $a4-$t3, $s0-$s3 # CHECK: save 128, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $s1, $s2, $s3 # encoding: [0x0c,0x81,0x80,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
	restore	48, $ra		# CHECK: restore 48, $ra	# encoding: [0xe1,0x83,0x32,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
	restore	256, $fp, $ra	# CHECK: restore 256, $fp, $ra	# encoding: [0xc2,0x83,0x02,0x31]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
	restore	1024, $ra, $s0	# CHECK: restore 1024, $ra, $s0	# encoding: [0xe2,0x83,0x02,0x34]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
	restore	160, $zero, $gp	# CHECK: restore 160, $zero, $gp # encoding: [0x02,0x80,0xa6,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
	restore	64, $s0, $s1, $s2, $s3, $gp	# CHECK: restore 64,  $s0, $s1, $s2, $s3, $gp # encoding: [0x05,0x82,0x46,0x30]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
	restore	64, $t4, $t5, $gp	# CHECK: restore 64, $t4, $t5, $gp # encoding: [0x43,0x80,0x46,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
	restore	128, $fp, $ra, $s0, $s1, $gp	# CHECK: restore 128, $fp, $ra, $s0, $s1, $gp # encoding: [0xc5,0x83,0x86,0x30]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
	restore	120, $fp, $ra, $s0-$k1, $gp	# CHECK: restore 120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x83,0x7e,0x30]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM

	restore.jrc	56, $ra	# CHECK: restore.jrc 56, $ra
				# CHECK-NEXT: # encoding: [0xe1,0x83,0x3b,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
	restore.jrc	256, $fp, $ra	# CHECK: restore.jrc 256, $fp, $ra
					# CHECK-NEXT: # encoding: [0xc2,0x83,0x03,0x31]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
	restore.jrc	1024, $ra, $s0	# CHECK: restore.jrc 1024, $ra, $s0
					# CHECK-NEXT: # encoding: [0xe2,0x83,0x03,0x34]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
	restore.jrc	160, $zero, $gp	# CHECK: restore.jrc 160, $zero, $gp
					# CHECK-NEXT: # encoding: [0x02,0x80,0xa7,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
	restore.jrc	64, $s0, $s1, $s2, $s3, $gp	# CHECK: restore.jrc 64, $s0, $s1, $s2, $s3, $gp
					# CHECK-NEXT: # encoding: [0x05,0x82,0x47,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
	restore.jrc	64, $t4, $t5, $gp	# CHECK: restore.jrc 64, $t4, $t5, $gp
					# CHECK-NEXT: # encoding: [0x43,0x80,0x47,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
	restore.jrc	128, $fp, $ra, $s0, $s1, $gp	# CHECK: restore.jrc 128, $fp, $ra, $s0, $s1, $gp
					# CHECK-NEXT: # encoding: [0xc5,0x83,0x87,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
	restore.jrc	120, $fp, $ra, $s0-$k1, $gp	# CHECK: restore.jrc 120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
					# CHECK-NEXT: # encoding: [0xcf,0x83,0x7f,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM

	addiu	$a1,$a1,8	# CHECK: addiu	$a1, $a1, 8 # encoding: [0xd2,0x92]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR2_NM
	addiu	$a1,$a3,0	# CHECK: addiu	$a1, $a3, 0 # encoding: [0xf0,0x92]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR2_NM
	addiu	$s1,$a1,28	# CHECK: addiu	$s1, $a1, 28 # encoding: [0xd7,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR2_NM
	addiu	$s1,$a1,32	# CHECK: addiu	$s1, $a1, 32 # encoding: [0x25,0x02,0x20,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
	addiu	$a1,$a1,65535	# CHECK: addiu	$a1, $a1, 65535 # encoding: [0xa5,0x00,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
	addiu	$s2,$s2,65536	# CHECK: addiu	$s2, $s2, 65536 # encoding: [0x41,0x62,0x00,0x00,0x01,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
	addiu	$t4,$t4,0x7fffffff	# CHECK: addiu	$t4, $t4, 2147483647 # encoding: [0x41,0x60,0xff,0xff,0xff,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
	addiu	$a4,$a4,0	# CHECK: addiu	$a4, $a4, 0 # encoding: [0x08,0x91]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIURS5_NM
	addiu	$t8,$t8,7	# CHECK: addiu	$t8, $t8, 7 # encoding: [0x0f,0x93]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIURS5_NM
	addiu	$sp,$sp,-8	# CHECK: addiu	$sp, $sp, -8 # encoding: [0xb8,0x93]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIURS5_NM
	addiu	$k1,$k1,8	# CHECK: addiu	$k1, $k1, 8 # encoding: [0x7b,0x03,0x08,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
	addiu	$k1,$k1,-9	# CHECK: addiu	$k1, $k1, -9 # encoding: [0x7b,0x83,0x09,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
	addiu	$a1,$sp,0	# CHECK: addiu	$a1, $sp, 0 # encoding: [0xc0,0x72]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR1SP_NM
	addiu	$s0,$sp,128	# CHECK: addiu	$s0, $sp, 128 # encoding: [0x60,0x70]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR1SP_NM
	addiu	$s3,$sp,252	# CHECK: addiu	$s3, $sp, 252 # encoding: [0xff,0x71]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR1SP_NM
	addiu	$s3,$sp,256	# CHECK: addiu	$s3, $sp, 256 # encoding: [0x7d,0x02,0x00,0x01]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
	addiu	$a1,$a2,-1	# CHECK: addiu	$a1, $a2, -1 # encoding: [0xa6,0x80,0x01,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
	addiu	$a1,$a2,-4095	# CHECK: addiu	$a1, $a2, -4095 # encoding: [0xa6,0x80,0xff,0x8f]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
	addiu	$s3,$s3,-4096	# CHECK: addiu	$s3, $s3, -4096 # encoding: [0x61,0x62,0x00,0xf0,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
	addiu	$a3,$a3,-2147483648	# CHECK: addiu	$a3, $a3, -2147483648 # encoding: [0xe1,0x60,0x00,0x00,0x00,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
	addiu	$a1, $gp, 0	# CHECK: addiu.b $a1, $gp, 0 # encoding: [0xac,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPB_NM
	addiu	$s0,$gp,131701	# CHECK: addiu.b $s0, $gp, 131701 # encoding: [0x0e,0x46,0x75,0x02]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPB_NM
	addiu	$s3,$gp,262143	# CHECK: addiu.b $s3, $gp, 262143 # encoding: [0x6f,0x46,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPB_NM
	addiu	$k0,$gp,262144	# CHECK: addiu.w $k0, $gp, 262144 # encoding: [0x44,0x43,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPW_NM
	addiu	$s7,$gp,2097148	# CHECK: addiu.w $s7, $gp, 2097148 # encoding: [0xff,0x42,0xfc,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPW_NM
	addiu	$a4,$gp,2097152	# CHECK: addiu.b32 $a4, $gp, 2097152 # encoding: [0x02,0x61,0x00,0x00,0x20,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGP48_NM
	addiu	$s4,$gp,262146	# CHECK: addiu.b32 $s4, $gp, 262146 # encoding: [0x82,0x62,0x02,0x00,0x04,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGP48_NM


	move.balc $a0, $a3, test	# CHECK: move.balc $a0, $a3, test # encoding: [0b111AAAAA,0x08,A,A]
					# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC21_S1
					# <MCInst #{{.*}} MOVEBALC_NM
	move.balc $a1, $zero, test	# CHECK: move.balc $a1, $zero, test # encoding: [0b011AAAAA,0x09,A,A]
					# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC21_S1
					# <MCInst #{{.*}} MOVEBALC_NM
	move.balc $a0, $a6, test	# CHECK: move.balc $a0, $a6, test # encoding: [0b010AAAAA,0x08,A,A]
					# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC21_S1
					# <MCInst #{{.*}} MOVEBALC_NM
	move.balc $a1, $s0, test	# CHECK: move.balc $a1, $s0, test # encoding: [0b000AAAAA,0x0b,A,A]
					# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC21_S1
					# <MCInst #{{.*}} MOVEBALC_NM
	move.balc $a0, $s7, test	# CHECK: move.balc $a0, $s7, test # encoding: [0b111AAAAA,0x0a,A,A]
					# <MCInst #{{.*}} MOVEBALC_NM

	lw	$a0, 0($s3)	# CHECK: lw $a0, 0($s3) # encoding: [0x30,0x16]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
	lw	$a1, 4($s2)	# CHECK: lw $a1, 4($s2) # encoding: [0xa1,0x16]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
	lw	$a2, 32($s1)	# CHECK: lw $a2, 32($s1) # encoding: [0x18,0x17]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
	lw	$a3, 60($s0)	# CHECK: lw $a3, 60($s0) # encoding: [0x8f,0x17]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM

	lw	$a7, 0($s7)	# CHECK: lw $a7, 0($s7) # encoding: [0x77,0x74]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
	lw	$s3, 4($a5)	# CHECK: lw $s3, 4($a5) # encoding: [0x61,0x77]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
	lw	$s4, 8($a3)	# CHECK: lw $s4, 8($a3) # encoding: [0x8f,0x76]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
	lw	$s6, 12($a0)	# CHECK: lw $s6, 12($a0) # encoding: [0xcc,0x77]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM

	lw	$a2, -4($s4)	# CHECK: lw $a2, -4($s4) # encoding: [0xd4,0xa4,0xfc,0xc0]
				# CHECK-NEXT: <MCInst  #{{.*}} LWs9_NM
	lw	$a3, -256($s0)	# CHECK: lw $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xc0]
				# CHECK-NEXT: <MCInst  #{{.*}} LWs9_NM
	lw	$a4, 64($s3)	# CHECK: lw $a4, 64($s3) # encoding: [0x13,0x85,0x40,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LW_NM
	lw	$a1, 252($s2)	# CHECK: lw $a1, 252($s2) # encoding: [0xb2,0x84,0xfc,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LW_NM
	lw	$s3, 4092($t5)	# CHECK: lw $s3, 4092($t5) # encoding: [0x63,0x86,0xfc,0x8f]
				# CHECK-NEXT: <MCInst  #{{.*}} LW_NM

	lw	$a0, 0($sp)	# CHECK: lw $a0, 0($sp) # encoding: [0x80,0x34]
				# CHECK-NEXT: <MCInst  #{{.*}} LWSP16_NM
	lw	$t0, 64($sp)	# CHECK: lw $t0, 64($sp) # encoding: [0x90,0x35]
				# CHECK-NEXT: <MCInst  #{{.*}} LWSP16_NM
	lw	$k0, 124($sp)	# CHECK: lw $k0, 124($sp) # encoding: [0x5f,0x37]
				# CHECK-NEXT: <MCInst  #{{.*}} LWSP16_NM

	lw	$a0, 0($gp)	# CHECK: lw $a0, 0($gp) # encoding: [0x00,0x56]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP16_NM
	lw	$a3, 256($gp)	# CHECK: lw $a3, 256($gp) # encoding: [0xc0,0x57]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP16_NM
	lw	$s0, 508($gp)	# CHECK: lw $s0, 508($gp) # encoding: [0x7f,0x54]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP16_NM
	lw	$a3, 4096($gp)	# CHECK: lw $a3, 4096($gp) # encoding: [0xe0,0x40,0x02,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP_NM
	lw	$s3, 65536($gp)	# CHECK: lw $s3, 65536($gp) # encoding: [0x61,0x42,0x02,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP_NM
	lw	$s0, 2097148($gp)	# CHECK: lw $s0, 2097148($gp) # encoding: [0x1f,0x42,0xfe,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LWGP_NM

	sw	$a0, 4($a1)	# CHECK: sw $a0, 4($a1) # encoding: [0x51,0x96]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
	sw	$a1, 4($s2)	# CHECK: sw $a1, 4($s2) # encoding: [0xa1,0x96]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
	sw	$a2, 32($s1)	# CHECK: sw $a2, 32($s1) # encoding: [0x18,0x97]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
	sw	$zero, 60($a3)	# CHECK: sw $zero, 60($a3) # encoding: [0x7f,0x94]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM

	sw	$zero, 0($s7)	# CHECK: sw $zero, 0($s7) # encoding: [0x77,0xf4]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
	sw	$s3, 4($a5)	# CHECK: sw $s3, 4($a5) # encoding: [0x61,0xf7]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
	sw	$s4, 8($a3)	# CHECK: sw $s4, 8($a3) # encoding: [0x8f,0xf6]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
	sw	$s6, 12($a0)	# CHECK: sw $s6, 12($a0) # encoding: [0xcc,0xf7]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM

	sw	$a2, -4($s6)	# CHECK: sw $a2, -4($s6) # encoding: [0xd6,0xa4,0xfc,0xc8]
				# CHECK-NEXT: <MCInst  #{{.*}} SWs9_NM
	sw	$a3, -256($s0)	# CHECK: sw $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xc8]
				# CHECK-NEXT: <MCInst  #{{.*}} SWs9_NM
	sw	$a5, 64($s3)	# CHECK: sw $a5, 64($s3) # encoding: [0x33,0x85,0x40,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
	sw	$a1, 252($s2)	# CHECK: sw $a1, 252($s2) # encoding: [0xb2,0x84,0xfc,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
	sw	$s3, 4092($t5)	# CHECK: sw $s3, 4092($t5) # encoding: [0x63,0x86,0xfc,0x9f]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM

	sw	$a0, 0($sp)	# CHECK: sw $a0, 0($sp) # encoding: [0x80,0xb4]
				# CHECK-NEXT: <MCInst  #{{.*}} SWSP16_NM
	sw	$t0, 64($sp)	# CHECK: sw $t0, 64($sp) # encoding: [0x90,0xb5]
				# CHECK-NEXT: <MCInst  #{{.*}} SWSP16_NM
	sw	$k0, 124($sp)	# CHECK: sw $k0, 124($sp) # encoding: [0x5f,0xb7]
				# CHECK-NEXT: <MCInst  #{{.*}} SWSP16_NM
	sw	$a1, 4092($sp)	# CHECK: sw $a1, 4092($sp) # encoding: [0xbd,0x84,0xfc,0x9f]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM

	sw	$a0, 0($gp)	# CHECK: sw $a0, 0($gp) # encoding: [0x00,0xd6]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP16_NM
	sw	$a3, 256($gp)	# CHECK: sw $a3, 256($gp) # encoding: [0xc0,0xd7]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP16_NM
	sw	$zero, 508($gp)	# CHECK: sw $zero, 508($gp) # encoding: [0x7f,0xd4]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP16_NM
	sw	$a2, 512($gp)	# CHECK: sw $a2, 512($gp) # encoding: [0xc0,0x40,0x03,0x02]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP_NM
	sw	$s2, 65536($gp)	# CHECK: sw $s2, 65536($gp) # encoding: [0x41,0x42,0x03,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP_NM
	sw	$zero, 2097148($gp)	# CHECK: sw $zero, 2097148($gp) # encoding: [0x1f,0x40,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP_NM

	lb	$a0, 0($s3)	# CHECK: lb $a0, 0($s3) # encoding:  [0x30,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
	lb	$a1, 1($s2)	# CHECK: lb $a1, 1($s2) # encoding: [0xa1,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
	lb	$a2, 2($s1)	# CHECK: lb $a2, 2($s1) # encoding: [0x12,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
	lb	$s0, 3($s0)	# CHECK: lb $s0, 3($s0) # encoding: [0x03,0x5c]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM

	lb	$a2, -4($s1)	# CHECK: lb $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LBs9_NM
	lb	$a3, -256($s5)	# CHECK: lb $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LBs9_NM
	lb	$a3, 4($s0)	# CHECK: lb $a3, 4($s0) # encoding: [0xf0,0x84,0x04,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LB_NM
	lb	$a5, 255($s2)	# CHECK: lb $a5, 255($s2) # encoding: [0x32,0x85,0xff,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LB_NM
	lb	$s3, 4095($t5)	# CHECK: lb $s3, 4095($t5) # encoding: [0x63,0x86,0xff,0x0f]
				# CHECK-NEXT: <MCInst  #{{.*}} LB_NM

	lb	$a0, 256($gp)	# CHECK: lb $a0, 256($gp) # encoding: [0x80,0x44,0x00,0x01]
				# CHECK-NEXT: <MCInst  #{{.*}} LBGP_NM
	lb	$s3, 4096($gp)	# CHECK: lb $s3, 4096($gp) # encoding: [0x60,0x46,0x00,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} LBGP_NM
	lb	$s0, 262143($gp)	# CHECK: lb $s0, 262143($gp) # encoding: [0x03,0x46,0xff,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LBGP_NM

	lbu	$a0, 0($s3)	# CHECK: lbu $a0, 0($s3) # encoding: [0x38,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
	lbu	$a1, 1($s2)	# CHECK: lbu $a1, 1($s2) # encoding: [0xa9,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
	lbu	$a2, 2($s1)	# CHECK: lbu $a2, 2($s1) # encoding: [0x1a,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
	lbu	$s0, 3($s0)	# CHECK: lbu $s0, 3($s0) # encoding: [0x0b,0x5c]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM

	lbu	$a2, -4($s1)	# CHECK: lbu $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUs9_NM
	lbu	$a3, -256($s5)	# CHECK: lbu $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUs9_NM
	lbu	$a3, 4($s0)	# CHECK: lbu $a3, 4($s0) # encoding: [0xf0,0x84,0x04,0x20]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU_NM
	lbu	$a5, 255($s2)	# CHECK: lbu $a5, 255($s2) # encoding: [0x32,0x85,0xff,0x20]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU_NM
	lbu	$s3, 4095($t5)	# CHECK: lbu $s3, 4095($t5) # encoding: [0x63,0x86,0xff,0x2f]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU_NM

	lbu	$a0, 0($gp)	# CHECK: lbu $a0, 0($gp) # encoding: [0x88,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUGP_NM
	lbu	$s3, 65536($gp)	# CHECK: lbu $s3, 65536($gp) # encoding: [0x69,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUGP_NM
	lbu	$s0, 262143($gp)	# CHECK: lbu $s0, 262143($gp) # encoding: [0x0b,0x46,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUGP_NM

	sb	$a0, 0($s3)	# CHECK: sb $a0, 0($s3) # encoding: [0x34,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
	sb	$a1, 1($s2)	# CHECK: sb $a1, 1($s2) # encoding: [0xa5,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
	sb	$a2, 2($s1)	# CHECK: sb $a2, 2($s1) # encoding: [0x16,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
	sb	$zero, 3($s0)	# CHECK: sb $zero, 3($s0) # encoding: [0x07,0x5c]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM

	sb	$a2, -4($s1)	# CHECK: sb $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0x88]
				# CHECK-NEXT: <MCInst  #{{.*}} SBs9_NM
	sb	$a3, -256($s5)	# CHECK: sb $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x88]
				# CHECK-NEXT: <MCInst  #{{.*}} SBs9_NM
	sb	$a3, 4($s0)	# CHECK: sb $a3, 4($s0) # encoding: [0xf0,0x84,0x04,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} SB_NM
	sb	$a5, 255($s2)	# CHECK: sb $a5, 255($s2) # encoding: [0x32,0x85,0xff,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} SB_NM
	sb	$s3, 4095($t5)	# CHECK: sb $s3, 4095($t5) # encoding: [0x63,0x86,0xff,0x1f]
				# CHECK-NEXT: <MCInst  #{{.*}} SB_NM

	sb	$a0, 0($gp)	# CHECK: sb $a0, 0($gp) # encoding: [0x84,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SBGP_NM
	sb	$s3, 65536($gp)	# CHECK: sb $s3, 65536($gp) # encoding: [0x65,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SBGP_NM
	sb	$s0, 262143($gp)	# CHECK: sb $s0, 262143($gp) # encoding: [0x07,0x46,0xff,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} SBGP_NM

	lh	$a0, 0($s3)	# CHECK: lh $a0, 0($s3) # encoding: [0x30,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
	lh	$a1, 2($s2)	# CHECK: lh $a1, 2($s2) # encoding: [0xa2,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
	lh	$a2, 4($s1)	# CHECK: lh $a2, 4($s1) # encoding: [0x14,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
	lh	$s0, 6($s0)	# CHECK: lh $s0, 6($s0) # encoding: [0x06,0x7c]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM

	lh	$a2, -4($s1)	# CHECK: lh $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0xa0]
				# CHECK-NEXT: <MCInst  #{{.*}} LHs9_NM
	lh	$a3, -256($s5)	# CHECK: lh $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xa0]
				# CHECK-NEXT: <MCInst  #{{.*}} LHs9_NM
	lh	$a4, 8($s0)	# CHECK: lh $a4, 8($s0) # encoding: [0x10,0x85,0x08,0x40]
				# CHECK-NEXT: <MCInst  #{{.*}} LH_NM
	lh	$a5, 254($s2)	# CHECK: lh $a5, 254($s2) # encoding: [0x32,0x85,0xfe,0x40]
				# CHECK-NEXT: <MCInst  #{{.*}} LH_NM
	lh	$s3, 4094($t5)	# CHECK: lh $s3, 4094($t5) # encoding: [0x63,0x86,0xfe,0x4f]
				# CHECK-NEXT: <MCInst  #{{.*}} LH_NM

	lh	$a0, 0($gp)	# CHECK: lh $a0, 0($gp) # encoding: [0x90,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHGP_NM
	lh	$s3, 65536($gp)	# CHECK: lh $s3, 65536($gp) # encoding: [0x71,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHGP_NM
	lh	$s0, 262142($gp)	# CHECK: lh $s0, 262142($gp) # encoding: [0x13,0x46,0xfe,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LHGP_NM
	lhu	$a0, 0($s3)	# CHECK: lhu $a0, 0($s3) # encoding: [0x38,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
	lhu	$a1, 2($s2)	# CHECK: lhu $a1, 2($s2) # encoding: [0xaa,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
	lhu	$a2, 4($s1)	# CHECK: lhu $a2, 4($s1) # encoding: [0x1c,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
	lhu	$s0, 6($s0)	# CHECK: lhu $s0, 6($s0) # encoding: [0x0e,0x7c]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
	lhu	$a2, -4($s1)	# CHECK: lhu $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0xb0]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUs9_NM
	lhu	$a3, -256($s5)		# CHECK: lhu $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xb0]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUs9_NM
	lhu	$a4, 8($s0)		# CHECK: lhu $a4, 8($s0) # encoding: [0x10,0x85,0x08,0x60]
					# CHECK-NEXT: <MCInst  #{{.*}} LHU_NM
	lhu	$a5, 254($s2)		# CHECK: lhu $a5, 254($s2) # encoding: [0x32,0x85,0xfe,0x60]
					# CHECK-NEXT: <MCInst  #{{.*}} LHU_NM
	lhu	$s3, 4094($t5)		# CHECK: lhu $s3, 4094($t5) # encoding: [0x63,0x86,0xfe,0x6f]
					# CHECK-NEXT: <MCInst  #{{.*}} LHU_NM
	lhu	$a0, 0($gp)	# CHECK: lhu $a0, 0($gp) # encoding: [0x90,0x44,0x01,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHUGP_NM
	lhu	$s3, 65536($gp)	# CHECK: lhu $s3, 65536($gp) # encoding: [0x71,0x46,0x01,0x00]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUGP_NM
	lhu	$s0, 262142($gp)	# CHECK: lhu $s0, 262142($gp) # encoding: [0x13,0x46,0xff,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUGP_NM

	sh	$a0, 0($s3)	# CHECK: sh $a0, 0($s3) # encoding: [0x31,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
	sh	$a1, 2($s2)	# CHECK: sh $a1, 2($s2) # encoding: [0xa3,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
	sh	$a2, 4($s1)	# CHECK: sh $a2, 4($s1) # encoding: [0x15,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
	sh	$zero, 6($s0)	# CHECK: sh $zero, 6($s0) # encoding: [0x07,0x7c]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM

	sh	$a2, -4($s1)    # CHECK: sh $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0xa8]
				# CHECK-NEXT: <MCInst  #{{.*}} SHs9_NM
	sh	$a3, -256($s5)  # CHECK: sh $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xa8]
				# CHECK-NEXT: <MCInst  #{{.*}} SHs9_NM
	sh	$a4, 8($s0)     # CHECK: sh $a4, 8($s0) # encoding: [0x10,0x85,0x08,0x50]
				# CHECK-NEXT: <MCInst  #{{.*}} SH_NM
	sh	$a5, 254($s2)   # CHECK: sh $a5, 254($s2) # encoding: [0x32,0x85,0xfe,0x50]
				# CHECK-NEXT: <MCInst  #{{.*}} SH_NM
	sh	$s3, 4094($t5)  # CHECK: sh $s3, 4094($t5) # encoding: [0x63,0x86,0xfe,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} SH_NM
	sh	$a0, 0($gp)	# CHECK: sh $a0, 0($gp) # encoding: [0x94,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SHGP_NM
	sh	$s3, 65536($gp)	# CHECK: sh $s3, 65536($gp) # encoding: [0x75,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SHGP_NM
	sh	$s0, 262142($gp)	# CHECK: sh $s0, 262142($gp) # encoding: [0x17,0x46,0xfe,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} SHGP_NM

	lwx	$a0, $s0($t0)	# CHECK: lwx $a0, $s0($t0) # encoding: [0x90,0x21,0x07,0x24]
				# CHECK-NEXT: <MCInst  #{{.*}} LWX_NM
	lbx	$a1, $s1($t1)	# CHECK: lbx $a1, $s1($t1) # encoding: [0xb1,0x21,0x07,0x28]
				# CHECK-NEXT: <MCInst  #{{.*}} LBX_NM
	lbux	$a2, $s2($t2)	# CHECK: lbux $a2, $s2($t2) # encoding: [0xd2,0x21,0x07,0x31]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUX_NM
	lhx	$a3, $s3($t3)	# CHECK: lhx $a3, $s3($t3) # encoding: [0xf3,0x21,0x07,0x3a]
				# CHECK-NEXT: <MCInst  #{{.*}} LHX_NM
	lhux	$a4, $s4($t4)	# CHECK: lhux $a4, $s4($t4) # encoding: [0x54,0x20,0x07,0x43]
				# CHECK-NEXT: <MCInst  #{{.*}} LHUX_NM
	swx	$a5, $s5($t5)	# CHECK: swx $a5, $s5($t5) # encoding: [0x75,0x20,0x87,0x4c]
				# CHECK-NEXT: <MCInst  #{{.*}} SWX_NM
	sbx	$a6, $s6($k0)	# CHECK: sbx $a6, $s6($k0) # encoding: [0x56,0x23,0x87,0x50]
				# CHECK-NEXT: <MCInst  #{{.*}} SBX_NM
	shx	$a7, $s7($k1)	# CHECK: shx $a7, $s7($k1) # encoding: [0x77,0x23,0x87,0x5a]
				# CHECK-NEXT: <MCInst  #{{.*}} SHX_NM

	lwxs	$s1, $a1($a2)	# CHECK: lwxs $s1, $a1($a2) # encoding: [0x53,0x53]
				# CHECK-NEXT: <MCInst  #{{.*}} LWXS16_NM
	lwxs	$s7, $a0($t5)	# CHECK: lwxs $s7, $a0($t5) # encoding: [0x64,0x20,0x47,0xbc]
				# CHECK-NEXT: <MCInst  #{{.*}} LWXS_NM
	swxs	$zero,$a1($s4)	# CHECK: swxs $zero, $a1($s4) # encoding: [0x85,0x22,0xc7,0x04]
				# CHECK-NEXT: <MCInst  #{{.*}} SWXS_NM
	lhxs	$s6, $a2($t0)	# CHECK: lhxs $s6, $a2($t0) # encoding: [0x86,0x21,0x47,0xb2]
				# CHECK-NEXT: <MCInst  #{{.*}} LHXS_NM
	lhuxs	$s5, $a3($t1)	# CHECK: lhuxs $s5, $a3($t1) # encoding: [0xa7,0x21,0x47,0xab]
				# CHECK-NEXT: <MCInst  #{{.*}} LHUXS_NM
	shxs	$zero,$a4($t2)	# CHECK: shxs $zero, $a4($t2) # encoding: [0xc8,0x21,0xc7,0x02]
				# CHECK-NEXT: <MCInst  #{{.*}} SHXS_NM

	swpc	$t0, test	# CHECK: swpc $t0, test	# encoding: [0x8f,0x61,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst  #{{.*}} SWPC_NM
	swpc	$t0, test-1048576	# CHECK: swpc $t0, test-1048576	# encoding: [0x8f,0x61,A,A,A,A]
					# CHECK-NEXT: fixup A - offset: 2, value: test-1048576, kind: fixup_NANOMIPS_PC_I32
					# CHECK-NEXT: <MCInst  #{{.*}} SWPC_NM
	lwpc	$t5, test	# CHECK: lwpc $t5, test	# encoding: [0x6b,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst  #{{.*}} LWPC_NM
	lwpc	$t5, test+2147483647	# CHECK: lwpc $t5, test+2147483647	# encoding: [0x6b,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test+2147483647, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst  #{{.*}} LWPC_NM

	addiu	$t0, $gp, %gp_rel(test)	# CHECK: addiu.b	$t0, $gp, %gp_rel(test) # encoding: [0b100011AA,0x45,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGPB_NM
	addiu.b	$t1, $gp, %gp_rel(test)	# CHECK: addiu.b	$t1, $gp, %gp_rel(test) # encoding: [0b101011AA,0x45,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGPB_NM
	addiu.w	$t2, $gp, %gp_rel(test)	# CHECK: addiu.w	$t2, $gp, %gp_rel(test) # encoding: [0b11000AAA,0x41,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL19_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGPW_NM
	addiu.b32 $t3, $gp, %gp_rel(test)	# CHECK: addiu.b32	$t3, $gp, %gp_rel(test) # encoding: [0xe2,0x61,A,A,A,A]
                                        # CHECK-NEXT: fixup A - offset: 2, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL_I32
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGP48_NM

	lw	$a0, %gp_rel(test)($gp)	# CHECK: lw	$a0, %gp_rel(test)($gp) # encoding: [0b0AAAAAAA,0x56]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL7_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} LWGP16_NM
	sw	$s1, %gp_rel(test)($gp)	# CHECK: sw	$s1, %gp_rel(test)($gp) # encoding: [0b1AAAAAAA,0xd4]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL7_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} SWGP16_NM
	lw	$t0, %gp_rel(test)($gp)	# CHECK: lw	$t0, %gp_rel(test)($gp) # encoding: [0b10000AAA,0x41,0x02'A',A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL19_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} LWGP_NM
	lh	$a1, %gp_rel(test)($gp)	# CHECK: lh	$a1, %gp_rel(test)($gp) # encoding: [0b1011000A,0x44,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL17_S1
                                        # CHECK-NEXT: <MCInst #{{.*}} LHGP_NM
	lhu	$a2, %gp_rel(test)($gp)	# CHECK: lhu	$a2, %gp_rel(test)($gp) # encoding: [0b1101000A,0x44,0x01'A',A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL17_S1
                                        # CHECK-NEXT: <MCInst #{{.*}} LHUGP_NM
	lb	$a3, %gp_rel(test)($gp)	# CHECK: lb	$a3, %gp_rel(test)($gp) # encoding: [0b111000AA,0x44,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} LBGP_NM
	lbu	$a4, %gp_rel(test)($gp)	# CHECK: lbu	$a4, %gp_rel(test)($gp) # encoding: [0b000010AA,0x45,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} LBUGP_NM
	sw	$t1, %gp_rel(test)($gp)	# CHECK: sw	$t1, %gp_rel(test)($gp) # encoding: [0b10100AAA,0x41,0x03'A',A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL19_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} SWGP_NM
	sh	$s1, %gp_rel(test)($gp)	# CHECK: sh	$s1, %gp_rel(test)($gp) # encoding: [0b0011010A,0x46,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL17_S1
                                        # CHECK-NEXT: <MCInst #{{.*}} SHGP_NM
	sb	$s3, %gp_rel(test)($gp)	# CHECK: sb	$s3, %gp_rel(test)($gp) # encoding: [0b011001AA,0x46,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} SBGP_NM

	lapc.h	$a0, test	# CHECK: lapc.h $a0, test # encoding: [0b100AAAAA,0x04,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: test, kind: fixup_NANOMIPS_PC21_S1
				# CHECK-NEXT: <MCInst #{{.*}} LAPC32_NM
	lapc.h	$a1, test-16	# CHECK: lapc.h $a1, test-16 # encoding: [0b101AAAAA,0x04,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: test-16, kind: fixup_NANOMIPS_PC21_S1
				# CHECK-NEXT: <MCInst #{{.*}} LAPC32_NM
	lapc.b	$a3, test-16	# CHECK: lapc.b $a3, test-16 # encoding: [0xe3,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test-16, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst #{{.*}} LAPC48_NM
	lapc	$a4, test+4	# CHECK: lapc.h $a4, test+4 # encoding: [0b000AAAAA,0x05,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: test+4, kind: fixup_NANOMIPS_PC21_S1
				# CHECK-NEXT: <MCInst #{{.*}} LAPC32_NM

	aluipc $a0, %pcrel_hi(test)	# CHECK: aluipc $a0, %pcrel_hi(test) # encoding: [0b1000AAAA,0xe0,0x02'A',A]
				# CHECK-NEXT: fixup A - offset: 0, value: %pcrel_hi(test), kind: fixup_NANOMIPS_PCHI20
				# CHECK-NEXT: <MCInst #{{.*}} ALUIPC_NM
	aluipc $a1, %hi(0x87654321)	# CHECK: aluipc $a1, %pcrel_hi(0x87654) # encoding: [0xa5,0xe0,0xef,0x40]
				# CHECK-NEXT: <MCInst #{{.*}} ALUIPC_NM
	aluipc $a2, 0x80000	# CHECK: aluipc $a2, %pcrel_hi(0x80000) # encoding: [0xc0,0xe0,0x03,0x00]
				# CHECK-NEXT: <MCInst #{{.*}} ALUIPC_NM
	aluipc $a3, 0xfffff	# CHECK: aluipc $a3, %pcrel_hi(0xfffff) # encoding: [0xff,0xe0,0xff,0xff]
				# CHECK-NEXT: <MCInst #{{.*}} ALUIPC_NM
	lui $s0, %hi(test)	# CHECK: lui $s0, %hi(test) # encoding: [0b0000AAAA,0xe2,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: %hi(test), kind: fixup_NANOMIPS_HI20
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
	lui $s1, %hi(0x12345678)	# CHECK: lui $s1, %hi(0x12345) # encoding: [0x34,0xe2,0x44,0x52]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
	lui $s2, %hi(0x80001000)	# CHECK: lui $s2, %hi(0x80001) # encoding: [0x40,0xe2,0x01,0x10]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
	lui $s3, %hi(0xfffff000)	# CHECK: lui $s3, %hi(0xfffff) # encoding: [0x7f,0xe2,0xfd,0xff]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM

	addiu $a0, $a0, %pcrel_lo(test)	# CHECK: addiu $a0, $a0, %lo(test) # encoding: [0x84,0x00,A,0b0000AAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: %lo(test), kind: fixup_NANOMIPS_LO12
				# CHECK-NEXT: <MCInst #{{.*}} ADDIU_NM
	addiu $s0, $s0, %lo(test)	# CHECK: addiu $s0, $s0, %lo(test) # encoding: [0x10,0x02,A,0b0000AAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: %lo(test), kind: fixup_NANOMIPS_LO12
				# CHECK-NEXT: <MCInst #{{.*}} ADDIU_NM

1:
	brsc	$a1	# CHECK: brsc $a1	# encoding: [0x05,0x48,0x00,0x80]
			# CHECK-NEXT: # <MCInst #{{.*}} BRSC_NM
	brsc	$a3	# CHECK: brsc $a3	# encoding: [0x07,0x48,0x00,0x80]
			# CHECK-NEXT: # <MCInst #{{.*}} BRSC_NM
	balrsc  $ra, $t0	# CHECK: balrsc $ra, $t0	# encoding: [0xec,0x4b,0x00,0x80]
				# CHECK-NEXT: # <MCInst #{{.*}} BALRSC_NM
	balrsc  $sp, $gp	# CHECK: balrsc $sp, $gp	# encoding: [0xbc,0x4b,0x00,0x80]
				# CHECK-NEXT: # <MCInst #{{.*}} BALRSC_NM

	balc[16] test	# CHECK: balc[16] test # encoding: [A,0b001110AA]
			# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC10_S1
			# CHECK-NEXT: <MCInst #{{.*}} BALC16_NM
	balc	test	# CHECK: balc test # encoding: [A,0b0010101A,A,A]
			# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC25_S1
			# CHECK-NEXT: <MCInst #{{.*}} BALC_NM
	balc	0x1ff0001	# CHECK: balc 0x1ff0001 # encoding: [A,0b0010101A,A,A]
			# CHECK-NEXT: fixup A - offset: 0, value: 33488897, kind: fixup_NANOMIPS_PC25_S1
			# CHECK-NEXT: <MCInst #{{.*}} BALC_NM


	bc	1b	# CHECK: bc .Ltmp0 # encoding: [A,0b000110AA]
			# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC10_S1
			# CHECK-NEXT: <MCInst #{{.*}} BC16_NM
	bc	1f	# CHECK: bc .Ltmp1 # encoding: [A,0b000110AA]
			# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC10_S1
			# CHECK-NEXT: <MCInst #{{.*}} BC16_NM
	beqc	$a1, $a2, 2f	# CHECK: beqc $a2, $a1, .Ltmp2 # encoding: [0b0101AAAA,0xdb]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC16_NM
	beqc	$a2, $a1, 2f	# CHECK: beqc $a2, $a1, .Ltmp2 # encoding: [0b0101AAAA,0xdb]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC16_NM
	beqc	$a4, $a1, 1f	# CHECK: beqc $a4, $a1, .Ltmp1 # encoding: [0xa8,0x88,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC_NM
	bnec	$a2, $a0, 2f	# CHECK: bnec $a0, $a2, .Ltmp2 # encoding: [0b0110AAAA,0xda]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC16_NM
	bnec	$a0, $a2, 2f	# CHECK: bnec $a0, $a2, .Ltmp2 # encoding: [0b0110AAAA,0xda]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC16_NM
	bnec	$a0, $s5, 1f	# CHECK: bnec $a0, $s5, .Ltmp1 # encoding: [0xa4,0xaa,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC_NM
2:
	bgec	$k0, $t1, 1b	# CHECK: bgec $k0, $t1, .Ltmp0 # encoding: [0xba,0x89,A,0b10AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BGEC_NM
	bltc	$a1, $s1, 1f	# CHECK: bltc $a1, $s1, .Ltmp1 # encoding: [0x25,0xaa,A,0b10AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BLTC_NM
	bltuc	$a2, $s2, 1b	# CHECK: bltuc $a2, $s2, .Ltmp0 # encoding: [0x46,0xaa,A,0b11AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BLTUC_NM
	beqzc	$a0, 1f		# CHECK: beqzc $a0, .Ltmp1 # encoding: [0b0AAAAAAA,0x9a]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQZC16_NM
	beqzc	$a4, 1f		# CHECK: beqzc $a4, .Ltmp1 # encoding: [0x00,0x89,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQZC_NM
	bnezc	$s0, 1b		# CHECK: bnezc $s0, .Ltmp0 # encoding: [0b0AAAAAAA,0xb8]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEZC16_NM
	bnezc	$s4, 1b		# CHECK: bnezc $s4, .Ltmp0 # encoding: [0x80,0xaa,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEZC_NM
	beqc	$a2, $zero, 1b	# CHECK: beqc $a2, $zero, .Ltmp0 # encoding: [0b0AAAAAAA,0x9b]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQCzero_NM
	beqc	$a6, $zero, 1b	# CHECK: beqzc $a6, .Ltmp0 # encoding: [0x0a,0x88,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC_NM
	bnec	$s2, $zero, 1f	# CHECK: bnec $s2, $zero, .Ltmp1 # encoding: [0b0AAAAAAA,0xb9]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNECzero_NM
	bnec	$s4, $zero, 1f	# CHECK: bnezc $s4, .Ltmp1 # encoding: [0x14,0xa8,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC_NM
	beqic	$a0, 0, 1f	# CHECK: beqic $a0, 0, .Ltmp1 # encoding: [0x80,0xc8,A,0b00000AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQIC_NM
	bneic	$a2, 1, 1f	# CHECK: bneic $a2, 1, .Ltmp1 # encoding: [0xd0,0xc8,A,0b00001AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEIC_NM
	bgeic	$a4, 64, 1f	# CHECK: bgeic $a4, 64, .Ltmp1 # encoding: [0x0a,0xc9,A,0b00000AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BGEIC_NM
	bgeiuc	$a6, 126, 1b	# CHECK: bgeiuc $a6, 126, .Ltmp0 # encoding: [0x4f,0xc9,A,0b11110AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BGEIUC_NM
	bltic	$s2, 127, 1b	# CHECK: bltic $s2, 127, .Ltmp0 # encoding: [0x5b,0xca,A,0b11111AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BLTIC_NM
	bltiuc	$s4, 10, 1b	# CHECK: bltiuc $s4, 10, .Ltmp0 # encoding: [0x9c,0xca,A,0b01010AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BLTIUC_NM
	bbnezc	$a1, 0, 1b	# CHECK: bbnezc $a1, 0, .Ltmp0 # encoding: [0xb4,0xc8,A,0b00000AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BBNEZC_NM
	bbeqzc	$s1, 31, 1f	# CHECK: bbeqzc $s1, 31, .Ltmp1 # encoding: [0x24,0xca,A,0b11111AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BBEQZC_NM

	jalrc	$a1		# CHECK: jalrc $ra, $a1	# encoding: [0xb0,0xd8]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC16_NM
	jalrc	$s1		# CHECK: jalrc $ra, $s1	# encoding: [0x30,0xda]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC16_NM
	jalrc	$t2, $s2	# CHECK: jalrc $t2, $s2	# encoding: [0xd2,0x49,0x00,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC_NM
	jalrc	$a5, $s5	# CHECK: jalrc $a5, $s5	# encoding: [0x35,0x49,0x00,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC_NM
	jalrc.hb $t0, $t1	# CHECK: jalrc.hb $t0, $t1	# encoding: [0x8d,0x49,0x00,0x10]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRCHB_NM
	jalrc.hb $s5, $a5	# CHECK: jalrc.hb $s5, $a5	# encoding: [0xa9,0x4a,0x00,0x10]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRCHB_NM
	jrc	$a1	# CHECK: jrc $a1	# encoding: [0xa0,0xd8]
			# CHECK-NEXT: # <MCInst #{{.*}} JRC_NM
	jrc	$ra	# CHECK: jrc $ra	# encoding: [0xe0,0xdb]
			# CHECK-NEXT: # <MCInst #{{.*}} JRC_NM

	1:

	ll	$a2, -4($s4)	# CHECK: ll $a2, -4($s4) # encoding: [0xd4,0xa4,0xfc,0xd1]
				# CHECK-NEXT: <MCInst  #{{.*}} LL_NM
	ll	$a3, -256($s0)	# CHECK: ll $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xd1]
				# CHECK-NEXT: <MCInst  #{{.*}} LL_NM
	ll	$a4, 64($s3)	# CHECK: ll $a4, 64($s3) # encoding: [0x13,0xa5,0x40,0x51]
				# CHECK-NEXT: <MCInst  #{{.*}} LL_NM
	ll	$a1, 252($s2)	# CHECK: ll $a1, 252($s2) # encoding: [0xb2,0xa4,0xfc,0x51]
				# CHECK-NEXT: <MCInst  #{{.*}} LL_NM
	sc	$a2, -4($s4)	# CHECK: sc $a2, -4($s4) # encoding: [0xd4,0xa4,0xfc,0xd9]
				# CHECK-NEXT: <MCInst  #{{.*}} SC_NM
	sc	$a3, -256($s0)	# CHECK: sc $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xd9]
				# CHECK-NEXT: <MCInst  #{{.*}} SC_NM
	sc	$a4, 64($s3)	# CHECK: sc $a4, 64($s3) # encoding: [0x13,0xa5,0x40,0x59]
				# CHECK-NEXT: <MCInst  #{{.*}} SC_NM
	sc	$a1, 252($s2)	# CHECK: sc $a1, 252($s2) # encoding: [0xb2,0xa4,0xfc,0x59]
				# CHECK-NEXT: <MCInst  #{{.*}} SC_NM
	llwp	$a0, $a1, ($s2)	# CHECK: llwp $a0, $a1, ($s2) # encoding: [0x92,0xa4,0x29,0x51]
				# CHECK-NEXT: <MCInst  #{{.*}} LLWP_NM
	scwp	$t0, $t1, ($s6)	# CHECK: scwp $t0, $t1, ($s6) # encoding: [0x96,0xa5,0x69,0x59]
				# CHECK-NEXT: <MCInst  #{{.*}} SCWP_NM

	pref	0, -4($s4)	# CHECK: pref 0, -4($s4) # encoding: [0x14,0xa4,0xfc,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFs9_NM
	pref	1, -256($s0)	# CHECK: pref 1, -256($s0) # encoding: [0x30,0xa4,0x00,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFs9_NM
	pref	12, 64($s3)	# CHECK: pref 12, 64($s3) # encoding: [0x93,0x85,0x40,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} PREF_NM
	pref	17, 252($s2)	# CHECK: pref 17, 252($s2) # encoding: [0x32,0x86,0xfc,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} PREF_NM
	pref	30, 4092($t5)	# CHECK: pref 30, 4092($t5) # encoding: [0xc3,0x87,0xfc,0x3f]
				# CHECK-NEXT: <MCInst  #{{.*}} PREF_NM

	synci	-4($s4)	# CHECK: synci -4($s4) # encoding: [0xf4,0xa7,0xfc,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIs9_NM
	synci	-256($s0)	# CHECK: synci -256($s0) # encoding: [0xf0,0xa7,0x00,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIs9_NM
	synci	64($s3)	# CHECK: synci 64($s3) # encoding: [0xf3,0x87,0x40,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCI_NM
	synci	252($s2)	# CHECK: synci 252($s2) # encoding: [0xf2,0x87,0xfc,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCI_NM
	synci	4092($t5)	# CHECK: synci 4092($t5) # encoding: [0xe3,0x87,0xfc,0x3f]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCI_NM

	cache	0, -4($s4)	# CHECK: cache 0, -4($s4) # encoding: [0x14,0xa4,0xfc,0xb9]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHE_NM
	cache	1, -256($s0)	# CHECK: cache 1, -256($s0) # encoding: [0x30,0xa4,0x00,0xb9]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHE_NM
	cache	12, 64($s3)	# CHECK: cache 12, 64($s3) # encoding: [0x93,0xa5,0x40,0x39]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHE_NM
	cache	31, 252($s2)	# CHECK: cache 31, 252($s2) # encoding: [0xf2,0xa7,0xfc,0x39]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHE_NM

	sync		# CHECK: sync	# encoding: [0x00,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync 0		# CHECK: sync	# encoding: [0x00,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync_wmb	# CHECK: sync_wmb	# encoding: [0x04,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync 4		# CHECK: sync_wmb	# encoding: [0x04,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync_mb		# CHECK: sync_mb	# encoding: [0x10,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync 0x10	# CHECK: sync_mb	# encoding: [0x10,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync_acquire	# CHECK: sync_acquire	# encoding: [0x11,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync 0x11	# CHECK: sync_acquire	# encoding: [0x11,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync_release	# CHECK: sync_release	# encoding: [0x12,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync 0x12	# CHECK: sync_release	# encoding: [0x12,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync_rmb	# CHECK: sync_rmb	# encoding: [0x13,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
	sync 0x13	# CHECK: sync_rmb	# encoding: [0x13,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM

	wait		# CHECK: wait	# encoding: [0x00,0x20,0x7f,0xc3]
			# CHECK-NEXT: # <MCInst #{{.*}} WAIT_NM
	wait 0		# CHECK: wait	# encoding: [0x00,0x20,0x7f,0xc3]
			# CHECK-NEXT: # <MCInst #{{.*}} WAIT_NM
	wait 1		# CHECK: wait 1	# encoding: [0x01,0x20,0x7f,0xc3]
			# CHECK-NEXT: # <MCInst #{{.*}} WAIT_NM
	wait 1023	# CHECK: wait 1023	# encoding: [0xff,0x23,0x7f,0xc3]
			# CHECK-NEXT: # <MCInst #{{.*}} WAIT_NM

	li	$t3, 65536	# CHECK: lui $t3, %hi(0x10) # encoding: [0xe1,0xe1,0x00,0x00]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
	li	$t4, -4096	# CHECK: lui $t4, %hi(-0x1) # encoding: [0x5f,0xe0,0xfd,0xff]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
	li	$t5, -2147483648	# CHECK: lui $t5, %hi(-0x80000) # encoding: [0x60,0xe0,0x01,0x00]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM

	jrc $ra
	.type   g_8,@object
	.comm   g_8,16,16
