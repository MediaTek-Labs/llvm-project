; RUN: llc -mcpu=nanomips < %s | FileCheck %s
; CHECK-NOT: __sync_val_compare_and_swap_8

; CHECK-LABEL: atomic_cmpxchg
; CHECK: llwp
; CHECK: scwp

define  noundef i1 @atomic_cmpxchg(ptr noundef %0,
                         ptr noundef %1,
                         i64 noundef signext %2,
                         i64 noundef signext %3,
                         ptr noundef %4) {
  %6 = load i64, ptr %0, align 8
  %7 = load i64, ptr %1, align 8
  %8 = cmpxchg ptr %4, i64 %2, i64 %3 monotonic monotonic, align 8
  %9 = extractvalue { i64, i1 } %8, 0
  %10 = extractvalue { i64, i1 } %8, 1
  ret i1 %10
}

; check reuse of compare & swap where new value is {0,0}
; CHECK-LABEL: atomic_load
; CHECK: llwp
; CHECK: move [[REG:\$a[0-9]+]], $zero
; CHECK: scwp [[REG]], $zero, ($a{{.*}})

define noundef i64 @atomic_load(ptr noundef %0) {
  %2 = load atomic i64, ptr %0 monotonic, align 8
  ret i64 %2
}
