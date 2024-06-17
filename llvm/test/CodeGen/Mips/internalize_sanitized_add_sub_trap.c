// RUN: clang %s -O3 -fsanitize-trap=all -fsanitize=signed-integer-overflow \
// RUN:   -fsanitize=unsigned-integer-overflow -S -mllvm -print-after=codegenprepare \
// RUN:   -mllvm -enable-nmips-addsubtrap=0 |& \
// RUN:   FileCheck %s --check-prefixes=OPT-ALL,OPT-LEGACY --implicit-check-not OPT-NEW

// RUN: clang %s -O3 -fsanitize-trap=all -fsanitize=signed-integer-overflow \
// RUN:   -fsanitize=unsigned-integer-overflow -S -mllvm -print-after=mips-isel \
// RUN:   -mllvm -enable-nmips-addsubtrap=0 |& \
// RUN:   FileCheck %s --check-prefixes=BE-ALL,BE-LEGACY --implicit-check-not BE-NEW

// RUN: clang %s -O3 -fsanitize-trap=all -fsanitize=signed-integer-overflow \
// RUN:   -mllvm -enable-nmips-addsubtrap=1 \
// RUN:   -fsanitize=unsigned-integer-overflow -S -mllvm -print-after=codegenprepare |& \
// RUN:   FileCheck %s --check-prefixes=OPT-ALL,OPT-NEW --implicit-check-not OPT-LEGACY

// RUN: clang %s -O3 -fsanitize-trap=all -fsanitize=signed-integer-overflow \
// RUN:   -mllvm -enable-nmips-addsubtrap=1 \
// RUN:   -fsanitize=unsigned-integer-overflow -S -mllvm -print-after=mips-isel |& \
// RUN:   FileCheck %s --check-prefixes=BE-ALL,BE-NEW --implicit-check-not BE-LEGACY

// OPT-ALL-LABEL: @add(
// BE-ALL: Machine code for function add:
int add(int a, int b) {
// OPT-LEGACY: call{{.*}}llvm.sadd.with.overflow.i32
// OPT-LEGACY: call{{.*}}ubsantrap
// OPT-NEW:  call{{.*}}llvm.mips.sadd.trap.on.overflow.i32
// BE-LEGACY: ADDu_NM
// BE-NEW: ADD_NM
 return a + b;
}
// BE-ALL: # End machine code for function add.

// OPT-ALL-LABEL: @add_folded(
// BE-ALL: Machine code for function add_folded:
int add_folded(int a, int b) {
 a &= 0xffff0000;
 b &= 0xffff0000;
// leave code as is since addition result is not used
// OPT-ALL: call{{.*}}llvm.sadd.with.overflow.i32
// OPT-ALL: call{{.*}}ubsantrap
// BE-ALL: ADDu_NM
 int res = a + b;
// the optimizer should fold it into zero
// OPT-ALL: ret i32 0
 return res & 0xffff;
}
// BE-ALL: # End machine code for function add_folded.

// check that optimization works well
// when a multiple overflows fall to a single trap
// OPT-ALL-LABEL: @add_combined_traps(
// BE-ALL: Machine code for function add_combined_traps:
int add_combined_traps(int a, int b, int c, unsigned e, unsigned f) {
// OPT-ALL: call{{.*}}llvm.uadd.with.overflow.i32
// BE-ALL: ADDu_NM
  unsigned usum = e + f;
// OPT-LEGACY: call{{.*}}llvm.sadd.with.overflow.i32
// OPT-NEW: call{{.*}}llvm.mips.sadd.trap.on.overflow.i32
// BE-LEGACY: ADDu_NM
// BE-NEW: ADD_NM
  int sum1 = a + b;
// OPT-LEGACY: call{{.*}}llvm.sadd.with.overflow.i32
// OPT-NEW: call{{.*}}llvm.mips.sadd.trap.on.overflow.i32
// BE-LEGACY: ADDu_NM
// BE-NEW: ADD_NM
  int sum2 = sum1 + c;
// OPT-ALL: call{{.*}}llvm.uadd.with.overflow.i32
// BE-ALL: ADDu_NM
  return usum + sum2;
}
// BE-ALL: # End machine code for function add_combined_traps.

// OPT-ALL-LABEL: @sub(
// BE-ALL: Machine code for function sub:
int sub(int a, int b) {
// OPT-LEGACY: call{{.*}}llvm.ssub.with.overflow.i32
// OPT-LEGACY: call{{.*}}ubsantrap
// OPT-NEW:  call{{.*}}llvm.mips.ssub.trap.on.overflow.i32
// BE-LEGACY: SUBu_NM
// BE-NEW: SUB_NM
 return a - b;
}
// BE-ALL: # End machine code for function sub.

// OPT-ALL-LABEL: @sub_folded(
// BE-ALL: Machine code for function sub_folded:
int sub_folded(int a, int b) {
 a |= 0xffff;
 b |= 0xffff;
// leave code as is since subition result is not used
// OPT-ALL: call{{.*}}llvm.ssub.with.overflow.i32
// OPT-ALL: call{{.*}}ubsantrap
// BE-ALL: SUBu_NM
 int res = a - b;
// the optimizer should fold it into zero
// OPT-ALL: ret i32 0
 return res & 0xffff;
}
// BE-ALL: # End machine code for function sub_folded.

// check that optimization works well
// when a multiple overflows fall to a single trap
// OPT-ALL-LABEL: @sub_combined_traps(
// BE-ALL: Machine code for function sub_combined_traps:
int sub_combined_traps(int a, int b, int c, unsigned e, unsigned f) {
// OPT-ALL: call{{.*}}llvm.usub.with.overflow.i32
// BE-ALL: SUBu_NM
  unsigned usum = e - f;
// OPT-LEGACY: call{{.*}}llvm.ssub.with.overflow.i32
// OPT-NEW: call{{.*}}llvm.mips.ssub.trap.on.overflow.i32
// BE-LEGACY: SUBu_NM
// BE-NEW: SUB_NM
  int sum1 = a - b;
// OPT-LEGACY: call{{.*}}llvm.ssub.with.overflow.i32
// OPT-NEW: call{{.*}}llvm.mips.ssub.trap.on.overflow.i32
// BE-LEGACY: SUBu_NM
// BE-NEW: SUB_NM
  int sum2 = sum1 - c;
// OPT-ALL: call{{.*}}llvm.usub.with.overflow.i32
// BE-ALL: SUBu_NM
  return usum - sum2;
}
// BE-ALL: # End machine code for function sub_combined_traps.
