// RUN: clang -fsanitize-trap=all -fsanitize=signed-integer-overflow %s -c -O |& FileCheck %s

int do_it_s(int x) {
  signed i = 0x7fffffff;
// CHECK: in function do_it_s i32 (i32): Sanitizer trap will always be triggered
  return i + 1;
}

int do_it_maybe(int flag) {
// CHECK-NOT: in function do_it_maybe i32 (i32): Sanitizer trap will always be triggered
  signed i = 0x7fffffff;
  if (flag) {
    return i + 1;
  }
  return i;
}

