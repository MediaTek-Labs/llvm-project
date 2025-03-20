# RUN: llvm-mc %s -triple=nanomips-elf -mattr=+eva -show-encoding -show-inst 2> %t0 | FileCheck %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+eva %s -o - \
# RUN:   | llvm-objdump --no-print-imm-hex --triple nanomips-elf -dr - | FileCheck --check-prefixes DISAS %s
	.text
	.globl test
test:	
	lbe	$a3, 252($s5)	# CHECK: lbe $a3, 252($s5) # encoding: [0xf5,0xa4,0xfc,0x02]
				# CHECK-NEXT: <MCInst  #{{.*}} LBE_NM
				# DISAS: {{.*}}  f5 a4 fc 02  	lbe	$a3, 252($s5)
	lbe	$a3, -256($s5)	# CHECK: lbe $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x82]
				# CHECK-NEXT: <MCInst  #{{.*}} LBE_NM
				# DISAS: {{.*}}  f5 a4 00 82  	lbe	$a3, -256($s5)
	sbe	$a2, 252($s1)	# CHECK: sbe $a2, 252($s1) # encoding: [0xd1,0xa4,0xfc,0x0a]
				# CHECK-NEXT: <MCInst  #{{.*}} SBE_NM
				# DISAS: {{.*}}  d1 a4 fc 0a  	sbe	$a2, 252($s1)
	sbe	$a3, -256($s5)	# CHECK: sbe $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x8a]
				# CHECK-NEXT: <MCInst  #{{.*}} SBE_NM
				# DISAS: {{.*}}  f5 a4 00 8a  	sbe	$a3, -256($s5)
	lbue	$a2, 252($s1)	# CHECK: lbue $a2, 252($s1) # encoding: [0xd1,0xa4,0xfc,0x12]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUE_NM
				# DISAS: {{.*}}  d1 a4 fc 12  	lbue	$a2, 252($s1)
	lbue	$a3, -256($s5)	# CHECK: lbue $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x92]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUE_NM
				# DISAS: {{.*}}  f5 a4 00 92  	lbue	$a3, -256($s5)
	lhe	$a2, 252($s1)	# CHECK: lhe $a2, 252($s1) # encoding: [0xd1,0xa4,0xfc,0x22]
				# CHECK-NEXT: <MCInst  #{{.*}} LHE_NM
				# DISAS: {{.*}}  d1 a4 fc 22  	lhe	$a2, 252($s1)
	lhe	$a3, -256($s5)	# CHECK: lhe $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xa2]
				# CHECK-NEXT: <MCInst  #{{.*}} LHE_NM
				# DISAS: {{.*}}  f5 a4 00 a2  	lhe	$a3, -256($s5)

	she	$a2, 252($s1)    # CHECK: she $a2, 252($s1) # encoding: [0xd1,0xa4,0xfc,0x2a]
				# CHECK-NEXT: <MCInst  #{{.*}} SHE_NM
				# DISAS: {{.*}}  d1 a4 fc 2a  	she	$a2, 252($s1)
	she	$a3, -256($s5)  # CHECK: she $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xaa]
				# CHECK-NEXT: <MCInst  #{{.*}} SHE_NM
				# DISAS: {{.*}}  f5 a4 00 aa  	she	$a3, -256($s5)
	lhue	$a2, 252($s1)	# CHECK: lhue $a2, 252($s1) # encoding: [0xd1,0xa4,0xfc,0x32]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUE_NM
				# DISAS: {{.*}}  d1 a4 fc 32  	lhue	$a2, 252($s1)
	lhue	$a3, -256($s5)		# CHECK: lhue $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xb2]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUE_NM
				# DISAS: {{.*}}  f5 a4 00 b2  	lhue	$a3, -256($s5)
	lwe	$a2, 252($s4)	# CHECK: lwe $a2, 252($s4) # encoding: [0xd4,0xa4,0xfc,0x42]
				# CHECK-NEXT: <MCInst  #{{.*}} LWE_NM
				# DISAS: {{.*}}  d4 a4 fc 42  	lwe	$a2, 252($s4)
	lwe	$a3, -256($s0)	# CHECK: lwe $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xc2]
				# CHECK-NEXT: <MCInst  #{{.*}} LWE_NM
				# DISAS: {{.*}}  f0 a4 00 c2  	lwe	$a3, -256($s0)
	swe	$a2, 252($s6)	# CHECK: swe $a2, 252($s6) # encoding: [0xd6,0xa4,0xfc,0x4a]
				# CHECK-NEXT: <MCInst  #{{.*}} SWE_NM
				# DISAS: {{.*}}  d6 a4 fc 4a  	swe	$a2, 252($s6)
	swe	$a3, -256($s0)	# CHECK: swe $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xca]
				# CHECK-NEXT: <MCInst  #{{.*}} SWE_NM
				# DISAS: {{.*}}  f0 a4 00 ca  	swe	$a3, -256($s0)

	
	prefe	0, 252($s4)	# CHECK: prefe 0, 252($s4) # encoding: [0x14,0xa4,0xfc,0x1a]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFE_NM
				# DISAS: {{.*}}  14 a4 fc 1a  	prefe	0, 252($s4)
	prefe	1, -256($s0)	# CHECK: prefe 1, -256($s0) # encoding: [0x30,0xa4,0x00,0x9a]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFE_NM
				# DISAS: {{.*}}  30 a4 00 9a  	prefe	1, -256($s0)
	syncie	252($s4)	# CHECK: syncie 252($s4) # encoding: [0xf4,0xa7,0xfc,0x1a]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIE_NM
				# DISAS: {{.*}}  f4 a7 fc 1a  	syncie	252($s4)
	syncie	-256($s0)	# CHECK: syncie -256($s0) # encoding: [0xf0,0xa7,0x00,0x9a]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIE_NM
				# DISAS: {{.*}}  f0 a7 00 9a  	syncie	-256($s0)
	cachee	0, -4($s4)	# CHECK: cachee 0, -4($s4) # encoding: [0x14,0xa4,0xfc,0xba]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHEE_NM
				# DISAS: {{.*}}  14 a4 fc ba  	cachee	0, -4($s4)
	cachee	1, -256($s0)	# CHECK: cachee 1, -256($s0) # encoding: [0x30,0xa4,0x00,0xba]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHEE_NM
				# DISAS: {{.*}}  30 a4 00 ba  	cachee	1, -256($s0)
	cachee	12, 64($s3)	# CHECK: cachee 12, 64($s3) # encoding: [0x93,0xa5,0x40,0x3a]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHEE_NM
				# DISAS: {{.*}}  93 a5 40 3a  	cachee	12, 64($s3)
	cachee	31, 252($s2)	# CHECK: cachee 31, 252($s2) # encoding: [0xf2,0xa7,0xfc,0x3a]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHEE_NM
				# DISAS: {{.*}}  f2 a7 fc 3a  	cachee	31, 252($s2)
	lle	$a2, -4($s4)	# CHECK: lle $a2, -4($s4) # encoding: [0xd4,0xa4,0xfc,0xd2]
				# CHECK-NEXT: <MCInst  #{{.*}} LLE_NM
				# DISAS: {{.*}}  d4 a4 fc d2  	lle	$a2, -4($s4)
	lle	$a3, -256($s0)	# CHECK: lle $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xd2]
				# CHECK-NEXT: <MCInst  #{{.*}} LLE_NM
				# DISAS: {{.*}}  f0 a4 00 d2  	lle	$a3, -256($s0)
	lle	$a4, 64($s3)	# CHECK: lle $a4, 64($s3) # encoding: [0x13,0xa5,0x40,0x52]
				# CHECK-NEXT: <MCInst  #{{.*}} LLE_NM
				# DISAS: {{.*}}  13 a5 40 52  	lle	$a4, 64($s3)
	lle	$a1, 252($s2)	# CHECK: lle $a1, 252($s2) # encoding: [0xb2,0xa4,0xfc,0x52]
				# CHECK-NEXT: <MCInst  #{{.*}} LLE_NM
				# DISAS: {{.*}}  b2 a4 fc 52  	lle	$a1, 252($s2)
	sce	$a2, -4($s4)	# CHECK: sce $a2, -4($s4) # encoding: [0xd4,0xa4,0xfc,0xda]
				# CHECK-NEXT: <MCInst  #{{.*}} SCE_NM
				# DISAS: {{.*}}  d4 a4 fc da  	sce	$a2, -4($s4)
	sce	$a3, -256($s0)	# CHECK: sce $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xda]
				# CHECK-NEXT: <MCInst  #{{.*}} SCE_NM
				# DISAS: {{.*}}  f0 a4 00 da  	sce	$a3, -256($s0)
	sce	$a4, 64($s3)	# CHECK: sce $a4, 64($s3) # encoding: [0x13,0xa5,0x40,0x5a]
				# CHECK-NEXT: <MCInst  #{{.*}} SCE_NM
				# DISAS: {{.*}}  13 a5 40 5a  	sce	$a4, 64($s3)
	sce	$a1, 252($s2)	# CHECK: sce $a1, 252($s2) # encoding: [0xb2,0xa4,0xfc,0x5a]
				# CHECK-NEXT: <MCInst  #{{.*}} SCE_NM
				# DISAS: {{.*}}  b2 a4 fc 5a  	sce	$a1, 252($s2)
