# RUN: llvm-mc %s -triple=nanomips-elf -show-encoding -show-inst \
# RUN: -mattr=+ginv | FileCheck %s

  ginvi $a0	# CHECK: ginvi $a0     # encoding: [0x04,0x20,0x7f,0x1f]
		# CHECK-NEXT: # <MCInst #{{.*}} GINVI_NM
  ginvt $a0, 2	# CHECK: ginvt $a0, 2  # encoding: [0x44,0x20,0x7f,0x0f]
		# CHECK-NEXT: # <MCInst #{{.*}} GINVT_NM
