# RUN: llvm-mc %s -triple=nanomips -show-encoding -show-inst \
# RUN:   -mattr=+crc | FileCheck %s

  .set noat
  crc32b $at, $t0	# CHECK: crc32b $at, $t0   # encoding: [0x2c,0x20,0xe8,0x03]
			# CHECK-NEXT: # <MCInst #{{.*}} CRC32B_NM
  crc32h $a0, $a1	# CHECK: crc32h $a0, $a1   # encoding: [0x85,0x20,0xe8,0x07]
			# CHECK-NEXT: # <MCInst #{{.*}} CRC32H_NM
  crc32w $a3, $a4	# CHECK: crc32w $a3, $a4   # encoding: [0xe8,0x20,0xe8,0x0b]
			# CHECK-NEXT: # <MCInst #{{.*}} CRC32W_NM
  crc32cb $at, $s0	# CHECK: crc32cb $at, $s0  # encoding: [0x30,0x20,0xe8,0x13]
			# CHECK-NEXT: # <MCInst #{{.*}} CRC32CB_NM
  crc32ch $a0, $a1      # CHECK: crc32ch $a0, $a1  # encoding: [0x85,0x20,0xe8,0x17]
			# CHECK-NEXT: # <MCInst #{{.*}} CRC32CH_NM
  crc32cw $a3, $a4      # CHECK: crc32cw $a3, $a4  # encoding: [0xe8,0x20,0xe8,0x1b]
			# CHECK-NEXT: # <MCInst #{{.*}} CRC32CW_NM
