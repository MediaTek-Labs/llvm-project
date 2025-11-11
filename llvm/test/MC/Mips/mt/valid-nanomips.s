# RUN: llvm-mc -triple=nanomips-elf -mattr=+mt -show-encoding < %s \
# RUN:   | FileCheck %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+mt %s -o - \
# RUN:   | llvm-objdump --no-print-imm-hex --triple nanomips-elf -dr - | FileCheck --check-prefixes DISAS %s
  dmt                       # CHECK:  dmt                        # encoding: [0x01,0x20,0xb0,0x0a]
                            # DISAS: {{.*}}: 2001 0ab0    dmt
  dmt $a1                   # CHECK:  dmt $a1                    # encoding: [0xa1,0x20,0xb0,0x0a]
                            # DISAS: {{.*}}: 20a1 0ab0    dmt  $a1
  emt                       # CHECK:  emt                        # encoding: [0x01,0x20,0xb0,0x0e]
                            # DISAS: {{.*}}: 2001 0eb0    emt
  emt $a0                   # CHECK:  emt $a0                    # encoding: [0x81,0x20,0xb0,0x0e]
                            # DISAS: {{.*}}: 2081 0eb0    emt  $a0
  dvpe                      # CHECK:  dvpe                       # encoding: [0x00,0x20,0xb0,0x0a]
                            # DISAS: {{.*}}: 2000 0ab0    dvpe
  dvpe $a2                  # CHECK:  dvpe  $a2                  # encoding: [0xc0,0x20,0xb0,0x0a]
                            # DISAS: {{.*}}: 20c0 0ab0    dvpe  $a2
  evpe                      # CHECK:  evpe                       # encoding: [0x00,0x20,0xb0,0x0e]
                            # DISAS: {{.*}}: 2000 0eb0    evpe
  evpe $a0                  # CHECK:  evpe  $a0                  # encoding: [0x80,0x20,0xb0,0x0e]
                            # DISAS: {{.*}}: 2080 0eb0    evpe $a0
  fork $t4, $t5, $a1        # CHECK:  fork  $t4, $t5, $a1        # encoding: [0xa3,0x20,0x28,0x12]
                            # DISAS: {{.*}}: 20a3 1228    fork $t4, $t5, $a1
  yield $a0                 # CHECK:  yield  $a0                 # encoding: [0x04,0x20,0x68,0x02]
                            # DISAS: {{.*}}: 2004 0268    yield $a0
  yield $a0, $a1            # CHECK:  yield $a0, $a1             # encoding: [0x85,0x20,0x68,0x02]
                            # DISAS: {{.*}}: 2085 0268
  mftr $a0, $5, 0, 2, 0     # CHECK:  mftr  $a0, $5, 0, 2, 0     # encoding: [0x85,0x20,0x30,0x12]
                            # DISAS: {{.*}}: 2085 1230    mftr $a0, $5, 0, 2, 0
  mftr $a0, $5, 1, 0, 0     # CHECK:  mftr  $a0, $5, 1, 0, 0     # encoding: [0x85,0x20,0x30,0x06]
                            # DISAS: {{.*}}: 2085 0630    mftr $a0, $5, 1, 0, 0
  mftr $a0, $0, 1, 1, 0     # CHECK:  mftr  $a0, $0, 1, 1, 0     # encoding: [0x80,0x20,0x30,0x0e]
                            # DISAS: {{.*}}: 2080 0e30    mftr $a0, $0, 1, 1, 0
  mftr $a0, $10, 1, 1, 0    # CHECK:  mftr  $a0, $10, 1, 1, 0    # encoding: [0x8a,0x20,0x30,0x0e]
                            # DISAS: {{.*}}: 208a 0e30    mftr $a0, $10, 1, 1, 0
  mftr $a0, $10, 1, 2, 0    # CHECK:  mftr  $a0, $10, 1, 2, 0    # encoding: [0x8a,0x20,0x30,0x16]
                            # DISAS: {{.*}}: 208a 1630    mftr $a0, $10, 1, 2, 0
  mftr $a0, $10, 1, 2, 1    # CHECK:  mftr  $a0, $10, 1, 2, 1    # encoding: [0x8a,0x20,0x38,0x16]
                            # DISAS: {{.*}}: 208a 1638    mftr $a0, $10, 1, 2, 1
  mftr $a0, $26, 1, 3, 0    # CHECK:  mftr  $a0, $26, 1, 3, 0    # encoding: [0x9a,0x20,0x30,0x1e]
                            # DISAS: {{.*}}: 209a 1e30    mftr $a0, $26, 1, 3, 0
  mftr $a0, $31, 1, 3, 0    # CHECK:  mftr  $a0, $31, 1, 3, 0    # encoding: [0x9f,0x20,0x30,0x1e]
  mftr $a0, $14, 1, 4, 0    # CHECK:  mftr  $a0, $14, 1, 4, 0    # encoding: [0x8e,0x20,0x30,0x26]
  mftr $a0, $15, 1, 5, 0    # CHECK:  mftr  $a0, $15, 1, 5, 0    # encoding: [0x8f,0x20,0x30,0x2e]
  mttr $a0, $5, 0, 2, 0     # CHECK:  mttr  $a0, $5, 0, 2, 0     # encoding: [0x85,0x20,0x70,0x12]
  mttr $a0, $5, 1, 0, 0     # CHECK:  mttr  $a0, $5, 1, 0, 0     # encoding: [0x85,0x20,0x70,0x06]
  mttr $a0, $0, 1, 1, 0     # CHECK:  mttr  $a0, $0, 1, 1, 0     # encoding: [0x80,0x20,0x70,0x0e]
  mttr $a0, $10, 1, 1, 0    # CHECK:  mttr  $a0, $10, 1, 1, 0    # encoding: [0x8a,0x20,0x70,0x0e]
  mttr $a0, $10, 1, 2, 0    # CHECK:  mttr  $a0, $10, 1, 2, 0    # encoding: [0x8a,0x20,0x70,0x16]
  mttr $a0, $10, 1, 2, 1    # CHECK:  mttr  $a0, $10, 1, 2, 1    # encoding: [0x8a,0x20,0x78,0x16]
  mttr $a0, $25, 1, 3, 0    # CHECK:  mttr  $a0, $25, 1, 3, 0    # encoding: [0x99,0x20,0x70,0x1e]
  mttr $a0, $31, 1, 3, 0    # CHECK:  mttr  $a0, $31, 1, 3, 0    # encoding: [0x9f,0x20,0x70,0x1e]
  mttr $a0, $14, 1, 4, 0    # CHECK:  mttr  $a0, $14, 1, 4, 0    # encoding: [0x8e,0x20,0x70,0x26]
  mttr $a0, $15, 1, 5, 0    # CHECK:  mttr  $a0, $15, 1, 5, 0    # encoding: [0x8f,0x20,0x70,0x2e]
