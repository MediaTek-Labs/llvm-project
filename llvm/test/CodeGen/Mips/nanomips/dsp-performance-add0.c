// RUN: clang %s -O0 --target=nanomips -S -mdsp -o - | FileCheck %s --check-prefixes NOOPT,ALL
// RUN: clang %s -O2 --target=nanomips -S -mdsp -o - | FileCheck %s --check-prefixes OPT,ALL

// ALL-LABEL: addq_zero1_test
short addq_zero1_test(short a) {
// NOOPT: addq_s.w
// OPT-NOT: addq_s.w
  return __builtin_mips_addq_s_w(0, a);
}

// ALL-LABEL: subq_zero1_test
short subq_zero1_test(short a) {
// NOOPT: subq_s.w
// OPT-NOT: subq_s.w
  return __builtin_mips_subq_s_w(0, a);
}

// ALL-LABEL: addq_zero2_test
short addq_zero2_test(short a) {
// NOOPT: addq_s.w
// OPT-NOT: addq_s.w
  return __builtin_mips_addq_s_w(a, 0);
}

// ALL-LABEL: subq_zero2_test
short subq_zero2_test(short a) {
// NOOPT: subq_s.w
// OPT-NOT: subq_s.w
  return __builtin_mips_subq_s_w(a, 0);
}
