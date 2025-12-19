// RUN: clang %s -O0 --target=nanomips -S -mdsp -o - | FileCheck %s --check-prefixes NOOPT,ALL
// RUN: clang %s -O2 --target=nanomips -S -mdsp -o - | FileCheck %s --check-prefixes OPT,ALL

// Under current builtin model addq/subq have side effects
// thus they are not optimized out
// but their results are not being used
// Under -O0 flag their result is legalized by seh
// Under optimization flag their result is ignored

// ALL-LABEL: addq_zero1_test
short addq_zero1_test(short a) {
// ALL: addq_s.w
// NOOPT: seh
// OPT-NOT: seh
  return __builtin_mips_addq_s_w(0, a);
}

// ALL-LABEL: subq_zero1_test
short subq_zero1_test(short a) {
// ALL: subq_s.w
// NOOPT: seh
// OPT-NOT: seh
  return __builtin_mips_subq_s_w(0, a);
}

// ALL-LABEL: addq_zero2_test
short addq_zero2_test(short a) {
// ALL: addq_s.w
// NOOPT: seh
// OPT-NOT: seh
  return __builtin_mips_addq_s_w(a, 0);
}

// ALL-LABEL: subq_zero2_test
short subq_zero2_test(short a) {
// ALL: subq_s.w
// NOOPT: seh
// OPT-NOT: seh
  return __builtin_mips_subq_s_w(a, 0);
}
