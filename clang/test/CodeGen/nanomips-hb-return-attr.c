// RUN: %clang_cc1 -triple nanomips-elf -emit-llvm  -o  - %s | FileCheck %s

void foo(void);

void foo(void) {return;}

void __attribute__((use_hazard_barrier_return)) bar(void);

void bar(void) {return;}

// CHECK: define{{.*}} void @foo() [[FOO:#[0-9]+]]
// CHECK: define{{.*}} void @bar() [[BAR:#[0-9]+]]

// CHECK-NOT: attributes [[FOO]] = { {{.*}} "use-hazard-barrier-return"{{.*}} }
// CHECK: attributes [[BAR]] = { {{.*}} "use-hazard-barrier-return"{{.*}} }
