//// Test1: allow all by default, foo() skipped by function attribute
// RUN: echo "default:allow" > %t0.list
// RUN: %clang_cc1 -debug-info-kind=standalone -fprofile-list=%t0.list -emit-llvm -finstrument-functions -disable-llvm-passes %s -o - | \
// RUN: FileCheck %s --check-prefixes=SKIPFOO,ALLOWBAR,ALLOWBAZ1,CHECK-ALLOWED
//// Test2: skip all by default
// RUN: echo "default:skip" > %t1.list
// RUN: %clang_cc1 -debug-info-kind=standalone -fprofile-list=%t1.list -emit-llvm -finstrument-functions -disable-llvm-passes %s -o - | \
// RUN: FileCheck %s --check-prefixes=SKIPFOO,SKIPBAR,SKIPBAZ
//// Test3: skip by default, bar enabled in profile-list
// RUN: echo "default:skip" > %t2.list
// RUN: echo "function:bar=allow" >> %t2.list
// RUN: %clang_cc1 -debug-info-kind=standalone -fprofile-list=%t2.list -emit-llvm -finstrument-functions -disable-llvm-passes %s -o - | \
// RUN: FileCheck %s --check-prefixes=SKIPFOO,ALLOWBAR,SKIPBAZ,CHECK-ALLOWED
//// Test4: allow by default, bar skipped in profile-list
// RUN: echo "default:allow" > %t3.list
// RUN: echo "function:bar=skip" >> %t3.list
// RUN: %clang_cc1 -debug-info-kind=standalone -fprofile-list=%t3.list -emit-llvm -finstrument-functions -disable-llvm-passes %s -o - | \
// RUN: FileCheck %s --check-prefixes=SKIPFOO,SKIPBAR,ALLOWBAZ2,CHECK-ALLOWED

int foo(int) __attribute__((no_instrument_function));
int foo(int x) {
// SKIPFOO: @foo(i32 {{.*}}%x) #[[ATTRSKIP:[0-9]+]]
  return x;
}

/* Function attribute overrides profile-list in all cases  */
int bar(int x) {
// SKIPBAR: @bar(i32 {{.*}}%x) #[[ATTRSKIP]]
// ALLOWBAR: @bar(i32 {{.*}}%x) #[[ATTRALLOW:[0-9]+]]
  return x;
}

int baz(int x) {
// SKIPBAZ: @baz(i32 {{.*}}%x) #[[ATTRSKIP]]
// ALLOWBAZ1: @baz(i32 {{.*}}%x) #[[ATTRALLOW]]
// ALLOWBAZ2: @baz(i32 {{.*}}%x) #[[ATTRALLOW:[0-9]+]]
  return x;
}

// SKIPFOO-NOT: attributes #[[ATTRSKIP]] = {{.*}}instrument-function-

// CHECK-ALLOWED: attributes #[[ATTRALLOW]] =
// CHECK-ALLOWED-SAME: "instrument-function-entry"="__cyg_profile_func_enter"
// CHECK-ALLOWED-SAME: "instrument-function-exit"="__cyg_profile_func_exit"

