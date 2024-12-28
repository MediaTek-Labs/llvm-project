// RUN: %clang_cc1 -fsanitize=array-bounds -O -fsanitize-trap=array-bounds -emit-llvm  %s -o - | FileCheck %s

struct S { char a; char b; };
struct S arr_s[10];

// CHECK-LABEL: @f8
int f8(int i) {
  // Assigning a struct to the value of an indexed array of structs
  // will dereference the array, so bounds checking should check that
  // it is in the range [0..10).
 
  // CHECK: {{.*}} = icmp ult i32 %i, 10
  struct S s2 = arr_s[i];
  return s2.a;
}
