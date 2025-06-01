# RUN: llvm-mc -triple=nanomips-elf -filetype=obj %s -o - | \
# RUN:   llvm-readelf -A - | FileCheck --check-prefixes=CHECK,CHECK-NOEXT   %s
# RUN: llvm-mc -triple=nanomips-elf -filetype=obj %s -o - | \
# RUN:   llvm-readelf -gnu -A - | FileCheck --check-prefixes=CHECK,CHECK-NOEXT   %s

// Check for missing ABIFlags
# RUN: llvm-mc -triple=nanomips-elf -filetype=obj %s -o - | \
# RUN:   llvm-objcopy --remove-section=.nanoMIPS.abiflags - | \
# RUN:   llvm-readobj -A - | \
# RUN: FileCheck --check-prefix=CHECK-MISSING %s

// Trigger check for corrupt/invalid ABIFlags by replacing
// assembler-generated section with _this_ file.
# RUN: llvm-mc -triple=nanomips-elf -filetype=obj %s -o - | \
# RUN:   llvm-objcopy --remove-section=.nanoMIPS.abiflags \
# RUN:     --add-section=.nanoMIPS.abiflags=%s - | \
# RUN:   llvm-readobj -A - 2> %t1
# RUN: FileCheck --check-prefix=CHECK-BADSIZE %s < %t1
# RUN: llvm-mc -triple=nanomips-elf -filetype=obj %s -o - | \
# RUN:   llvm-objcopy --remove-section=.nanoMIPS.abiflags \
# RUN:     --add-section=.nanoMIPS.abiflags=%s - | \
# RUN:   llvm-readobj -gnu -A - 2> %t1
# RUN: FileCheck --check-prefix=CHECK-BADSIZE %s < %t1

  .ent    test
test:
  nop

# CHECK: nanoMIPS ABI Flags Version: 0
# CHECK: ISA: NanoMips32r6
# CHECK: GPR size: 32
# CHECK: CPR1 size: 32
# CHECK: CPR2 size: 0
# CHECK: FP ABI: Hard or soft float
# CHECK-NOEXT: ISA Extension: None
# CHECK: ASEs: DSP, Enhanced VA Scheme, MT
# CHECK: FLAGS 1: 00000001
# CHECK: FLAGS 2: 00000000

# CHECK-MISSING: There is no .nanoMIPS.abiflags section in the file.
# CHECK-BADSIZE: warning: {{.*}}: unable to read the .nanoMIPS.abiflags section: it has a wrong size
