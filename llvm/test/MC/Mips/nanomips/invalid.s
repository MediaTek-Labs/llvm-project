# RUN: not llvm-mc -arch=nanomips < %s 2>%t1
# RUN: FileCheck %s < %t1
        .text
        .globl foo
foo:
        ll  $a4, 66($s3) # CHECK: error: invalid operand for instruction
        sc  $a2, -6($s4) # CHECK: error: invalid operand for instruction
        ll  $a4, 33($s3) # CHECK: error: invalid operand for instruction
        sc  $a2, -1($s4) # CHECK: error: invalid operand for instruction
	addiu.b32 $a4, $gp, -2147483649 # CHECK: [[@LINE]]:{{[0-9]+}}: error: expected 32-bit signed/unsigned GP-relative offset
	addiu.b32 $a4, $gp, 4294967296 # CHECK: [[@LINE]]:{{[0-9]+}}: error: expected 32-bit signed/unsigned GP-relative offset
	addiu.b $a0, $gp, 262144	# > 18 bits
	# CHECK: [[@LINE-1]]:{{[0-9]+}}: error: expected 18-bit unsigned GP-relative offset
	addiu[gp.b] $a0, $gp, -262143	# -ve
	# CHECK: [[@LINE-1]]:{{[0-9]+}}: error: expected 18-bit unsigned GP-relative offset
	addiu.w $a0, $gp, 524286	# > 21 bits
	# CHECK: [[@LINE-1]]:{{[0-9]+}}: error: expected word-aligned 21-bit unsigned GP-relative offset
	addiu.w $a0, $gp, -524284	# -ve
	# CHECK: [[@LINE-1]]:{{[0-9]+}}: error: expected word-aligned 21-bit unsigned GP-relative offset
	addiu[gp.w] $a0, $gp, 524283	# mis-aligned
	# CHECK: [[@LINE-1]]:{{[0-9]+}}: error: expected word-aligned 21-bit unsigned GP-relative offset
