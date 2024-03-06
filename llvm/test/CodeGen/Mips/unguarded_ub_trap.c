// the checker without sanitizing has no effect:
// RUN: clang %s -O3 -g -S -fwarn-ubsantrap |& FileCheck %s --check-prefix SILENT --allow-empty

// the checker is disabled by default:
// RUN: clang %s -O3 -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S |& \
// RUN:   FileCheck %s --check-prefix SILENT --allow-empty

// the negative flag disables the positive one:
// RUN:  clang %s -O3 -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S \
// RUN:    -fwarn-ubsantrap -fno-warn-ubsantrap |& \
// RUN:    FileCheck %s --check-prefix SILENT --allow-empty

// those types are promoted and do not overflow
// no need to define precondition (no need for -DSILENT)
// make sure no calls to traps, and no warnings are generated:
// RUN: for W in 8 16; do \
// RUN:   clang %s -O0 -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S \
// RUN:     -fwarn-ubsantrap -DWIDTH=$W -Xclang -disable-llvm-passes -o %t.s && \
// RUN:     not grep -q "call.*llvm.ubsantrap" %t.s || exit 1; \
// RUN:   clang %s -O3 -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S \
// RUN:     -fwarn-ubsantrap -DWIDTH=$W |& \
// RUN:     FileCheck %s --check-prefix SILENT --allow-empty || exit 1;\
// RUN: done

// those types may overflow
// RUN: for W in 32 64; do \
// RUN:   clang %s -O3 -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S \
// RUN:     -fwarn-ubsantrap -DWIDTH=$W |& \
// RUN:     FileCheck %s --check-prefix WARN-$W || exit 1;\
// RUN: done

// define precondition by -DSILENT
// make sure Front-End filtering works
// RUN: for W in 32 64; do \
// RUN:  clang %s -O3 -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S \
// RUN:    -fwarn-ubsantrap -DWIDTH=$W -DSILENT -mllvm -trace-warn-trap-visitor=1 |& \
// RUN:    FileCheck %s --check-prefixes SILENT,FE-FILTER --allow-empty || exit 1; \
// RUN: done

// trap is not guarded by a strictly defind predocndition
// but optimized away anyway
// RUN: clang %s -O3 -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S \
// RUN:   -fwarn-ubsantrap -mllvm -warn-trap-in-frontend |& \
// RUN:   FileCheck %s --check-prefix OPT-FILTER

// check -Os:
// RUN:  clang %s -Os -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S -fwarn-ubsantrap |& \
// RUN:    FileCheck %s --check-prefix WARN-32

// check -O0:
// RUN:  clang %s -O0 -fsanitize-trap=all -fsanitize=unsigned-integer-overflow -g -S -fwarn-ubsantrap |& \
// RUN:    FileCheck %s --check-prefix WARN-32

// SILENT-NOT: Operation overflow is not guarded
#include <limits.h>
typedef unsigned char UCHAR;
typedef unsigned short USHORT;
typedef unsigned int UINT;
typedef unsigned long long UINT64;
typedef struct {
  int arr[10];
} S;

#ifndef WIDTH
#  define WIDTH 32
#endif

#if WIDTH == 8
#  define TYPE UCHAR
#  define TYPE_MAX UCHAR_MAX
#  define TYPE_FFF 0xff
#elif WIDTH == 16
#  define TYPE USHORT
#  define TYPE_MAX USHRT_MAX
#  define TYPE_FFF 0xffff
#elif WIDTH == 32
#  define TYPE UINT
#  define TYPE_MAX UINT_MAX
#  define TYPE_FFF 0xffffffff
#elif WIDTH == 64
#  define TYPE UINT64
#  define TYPE_MAX ULLONG_MAX
#  define TYPE_FFF 0xffffffffffffffff
#else
#  error "unsupported width"
#endif
struct aa {
  int length;
  unsigned char *data;
};
typedef struct aa AA;
TYPE if_else_sub(TYPE a, TYPE b) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: if_else_sub
  if (a < b) goto err;
  else {
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:50:13: in function if_else_sub i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:50:13: in function if_else_sub i32 (i32, i32): Operation overflow is not guarded
#line 50
   return a - b;
#ifdef SILENT
 }
err:
  return -1;
#endif
}

TYPE sub(TYPE a, TYPE b) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: sub
  if (a < b) goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:60:12: in function sub i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:60:12: in function sub i32 (i32, i32): Operation overflow is not guarded
#line 60
  return a - b;
err:
  return -1;
}
TYPE predec(TYPE n) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: predec
  if (n == 0) goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:70:3: in function predec i64 (i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:70:3: in function predec i32 (i32): Operation overflow is not guarded
#line 70
  --n;
  return n;
err:
  return -1;
}
TYPE predec_ret(TYPE n) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: predec_ret
  if (n == 0) goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:75:10: in function predec_ret i64 (i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:75:10: in function predec_ret i32 (i32): Operation overflow is not guarded
#line 75
  return --n;
err:
  return -1;
}
TYPE postdec(TYPE n) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: postdec
  if (n == 0) goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:80:4: in function postdec i64 (i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:80:4: in function postdec i32 (i32): Operation overflow is not guarded
#line 80
  n--;
  return n;
err:
  return -1;
}
TYPE add_guard_ff(TYPE b, TYPE a) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: add_guard_ff
  if (TYPE_FFF - a < b)
    goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:90:12: in function add_guard_ff i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:90:12: in function add_guard_ff i32 (i32, i32): Operation overflow is not guarded
#line 90
  return a + b;
err:
  return -1;
}

TYPE add_assign_guard_umax(TYPE b, TYPE a) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: add_assign_guard_umax
// FE-FILTER-DAG: Found guarding precondition for overflow trap: add_assign_guard_umax
  if (TYPE_MAX - a < b)
    goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:95:12: in function add_assign_guard_umax i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:95:12: in function add_assign_guard_umax i32 (i32, i32): Operation overflow is not guarded
#line 95
  return a += b;
err:
  return -1;
}


TYPE add_guard_umax(TYPE b, TYPE a) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: add_guard_umax
// FE-FILTER-DAG: Found guarding precondition for overflow trap: add_guard_umax
  if (TYPE_MAX - a < b)
    goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:100:12: in function add_guard_umax i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:100:12: in function add_guard_umax i32 (i32, i32): Operation overflow is not guarded
#line 100
  return a + b;
err:
  return -1;
}

TYPE mul_ff(TYPE b, TYPE a) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: mul_ff
  if (a && (TYPE_FFF/a) < b)
    goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:105:11: in function mul_ff i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:105:11: in function mul_ff i32 (i32, i32): Operation overflow is not guarded
#line 105
  return a*b;
err:
  return -1;
}
TYPE mul(TYPE b, TYPE a) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: mul
  if (a && (TYPE_MAX/a) < b)
    goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:110:11: in function mul i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:110:11: in function mul i32 (i32, i32): Operation overflow is not guarded
#line 110
  return a*b;
err:
  return -1;
}

TYPE mul_assign(TYPE b, TYPE a) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: mul_assign
  if (a && (TYPE_MAX/a) < b)
    goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:115:12: in function mul_assign i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:115:12: in function mul_assign i32 (i32, i32): Operation overflow is not guarded
#line 115
  return a *= b;
err:
  return -1;
}

TYPE mul_assign_flat(TYPE b, TYPE a) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: mul_assign_flat
  if (a && (TYPE_MAX/a) < b)
    goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:117:5: in function mul_assign_flat i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:117:5: in function mul_assign_flat i32 (i32, i32): Operation overflow is not guarded
#line 117
  a *= b;
  return a;
err:
  return 1;
}

TYPE preinc(TYPE b, TYPE a) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: preinc
  if (a == TYPE_MAX)
    goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:120:10: in function preinc i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:120:10: in function preinc i32 (i32, i32): Operation overflow is not guarded
#line 120
  return ++a;
err:
  return -1;
}

TYPE postinc(TYPE b, TYPE a) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: postinc
  if (a == TYPE_FFF)
    goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:130:11: in function postinc i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:130:11: in function postinc i32 (i32, i32): Operation overflow is not guarded
#line 130
  return a++;
err:
  return -1;
}

int break_decision();
TYPE sub_loop(TYPE b, TYPE a) {
  for (;;) {
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: sub_loop
    if (a < b)
      goto err;
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:140:11: in function sub_loop i64 (i64, i64): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:140:11: in function sub_loop i32 (i32, i32): Operation overflow is not guarded
#line 140
    a = a - b;
    if (break_decision())
      break;
  }
  err:
   return a;
}
void ERR_new(void);
void ERR_set_dbg(char *, int, const char*);
void ERR_set_err(int, int, const char *,...);
void hidden(TYPE *len, TYPE *off);
TYPE sub_realistic1() {
  int i = 0;
  TYPE diff, len, off;
  for (;;) {
    hidden(&len, &off);
#ifdef SILENT
// FE-FILTER-DAG: Found guarding precondition for overflow trap: sub_realistic1
    if (len < off ) {
      (ERR_new(), ERR_set_dbg(__FILE__, __LINE__, __func__), ERR_set_err)(13, 18L, ((void *)0));
      goto err;
    }
#endif
// WARN-64-DAG: {{.*}}/unguarded_ub_trap.c:150:16: in function sub_realistic1 i64 (): Operation overflow is not guarded
// WARN-32-DAG: {{.*}}/unguarded_ub_trap.c:150:16: in function sub_realistic1 i32 (): Operation overflow is not guarded
#line 150
    diff = len - off;
  }
  return diff;
err:
  return -1;
}
int sub_realistic2() {
  TYPE off = 0, len = 0, want = 8, diff;
  for (;;) {
// FE-FILTER-DAG: Found guarding precondition for overflow trap: sub_realistic2
      if (want < diff) {
        (ERR_new(), ERR_set_dbg(__FILE__, __LINE__, __func__), ERR_set_err)(13, 18L, ((void *)0));
         goto err;
      }
#line 160
    want -= diff;
//    }
  }
  return off;
err:
  return -1;
}
TYPE sub_realistic3() {
  TYPE off = 0, len = 0, want = 8, diff;
  for (;;) {
      if (want <= 0) {
        (ERR_new(), ERR_set_dbg(__FILE__, __LINE__, __func__), ERR_set_err)(13, 18L, ((void *)0));
         goto err;
      }
// check that this trap is generated and being caught
// by the front-end, and then optimized away
// OPT-FILTER:{{.*}}/unguarded_ub_trap.c:170:9: warning: Unguarded overflow
// OPT-FILTER-NOT:in function sub_realistic3{{.*}}Operation overflow is not guarded
#line 170
    want--;
  }
  return off;
err:
  return -1;
}
// does not trigger for now, even though multiplication is generated
int array_access(S *s, TYPE len) {
  return s->arr[len];
}

// regression test: was crashing
int foo(AA *a) {
  while ((a->length > 0) && (a->data[a->length -1] == 0))
    a->length--;
  return 1;
}
