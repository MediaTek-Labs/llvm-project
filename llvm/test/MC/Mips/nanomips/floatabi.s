# RUN: not llvm-mc %s -triple nanomips-elf < %s 2> %t1
# RUN:   FileCheck %s < %t1

.module softfloat
.module hardfloat # CHECK: error: hardfloat is not supported with nanoMIPS

.set softfloat
.set hardfloat # CHECK: error: hardfloat is not supported with nanoMIPS
