// RUN: %clang_cc1 -emit-llvm -o - %s -fsanitize=kcfi | FileCheck  -- check-prefix=CHECK-ENABLED %s
// RUN: %clang_cc1 -emit-llvm -o - %s -fsanitize=kcfi -fsanitize-kcfi-disable-check | FileCheck  -- check-prefix=CHECK-DISABLED %s

// CHECK-ENABLED: {{define.*@callee.*!kcfi_type}}
// CHECK-DISABLED: {{define.*@callee.*!kcfi_type}}

int callee(int x) {
  int i;
  for (i = 0; i < 32; i++)
    x ^= (x << i) ^ i;
  return x;
}

void *p = callee;

// CHECK-ENABLED: {{define.*@caller.*!kcfi_type}}
// CHECK-DISABLED: {{define.*@caller.*!kcfi_type}}
int caller() {
  void (*callee)(int) = p;
  // CHECK-DISABLED-NOT: @llvm.trap
  // CHECK-ENABLED: @llvm.trap
  callee(77);
  return 0;
}
