// RUN: clang -Os -fsanitize=kcfi %s -c -o %t && llvm-objdump -d %t | FileCheck %s

// Check that potentially-indirectly-called functions are aligned to
// 32-bits so that reading addr-4 will be 32-bit aligned.

// CHECK: {{[a-f0-9]{7}[048c] <a>:}}
int a(int x) { return x; }
// CHECK: {{[a-f0-9]{7}[048c] <b>:}}
int b(int x) { return x; }
// CHECK: {{[a-f0-9]{7}[048c] <c>:}}
int c(int x) { return x; }
// CHECK: {{[a-f0-9]{7}[048c] <d>:}}
int d(int x) { return x; }

int caller(int x) {
  int (*callee)(int) = a;
  if (x > 0) callee = b;
  else if (x > 10) callee = c;
  else if (x > 20) callee = d;
  return callee(x);
}
