// RUN: clang %s -O3 -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S \
// RUN:   -fwarn-ubsantrap  -mllvm -trace-warn-trap-visitor=1 |& FileCheck %s

unsigned foo(unsigned a) {
if (a == 0)
  goto err;
// CHECK: Found guarding precondition for overflow trap: foo
#line 10
  return --a;
err:
// CHECK: {{.*}}/unguarded_ub_trap_overfiltering.c:10:10: in function foo i32 (i32): Operation overflow is not guarded
#line 10
  return --a;
}
