// RUN: clang %s -O3 -fsanitize-trap=all -fsanitize=signed-integer-overflow \
// RUN:   -S -mllvm -print-after=codegenprepare \
// RUN:   -mllvm -enable-nmips-addsubtrap=0 |& \
// RUN:   FileCheck %s --check-prefixes CHECK,NOOPT --implicit-check-not OPT

// RUN: clang %s -O3 -fsanitize-trap=all -fsanitize=signed-integer-overflow \
// RUN:   -S -mllvm -print-after=codegenprepare \
// RUN:   -mllvm -disable-nmips-condtrap=0 -mllvm -enable-nmips-addsubtrap=0 |& \
// RUN:   FileCheck %s --check-prefixes CHECK,OPT --implicit-check-not NOOPT

// check that by the time of optimization end there are no unnecessary branches
// CHECK: @add
int add(int a, int b) {
// CHECK: [[PACKED:%[0-9]+]] = {{.*}}@llvm.sadd.with.overflow
// CHECK: [[OVERFLOW:%[0-9]+]] = extractvalue{{.*}}[[PACKED]], 1
// NOOPT: br{{.*}}label
// NOOPT: llvm.ubsantrap
// OPT: [[EXT_OVERFLOW:%[0-9]+]] = zext{{.*}}[[OVERFLOW]]
// OPT: llvm.mips.condtrap({{.*}}[[EXT_OVERFLOW]]
// CHECK: [[ADD:%[0-9]+]] = extractvalue{{.*}}[[PACKED]], 0
// CHECK: ret{{.*}}[[ADD]]
  return a + b;
}
