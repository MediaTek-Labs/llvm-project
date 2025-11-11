# RUN: llvm-mc %s -triple=nanomips-elf -show-encoding -show-inst \
# RUN: -mattr=+ginv | FileCheck %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+ginv %s -o - \
# RUN:   | llvm-objdump --no-print-imm-hex --triple nanomips-elf --mattr=+ginv -dr - | FileCheck --check-prefixes DISAS %s

  ginvi $a0	# CHECK: ginvi $a0     # encoding: [0x04,0x20,0x7f,0x1f]
		# CHECK-NEXT: # <MCInst #{{.*}} GINVI_NM
                # DISAS: {{.*}}: 2004 1f7f    ginvi   $a0
  ginvt $a0, 2	# CHECK: ginvt $a0, 2  # encoding: [0x44,0x20,0x7f,0x0f]
		# CHECK-NEXT: # <MCInst #{{.*}} GINVT_NM
                # DISAS: {{.*}}: 2044 0f7f    ginvt   $a0, 2
