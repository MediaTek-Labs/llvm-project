; RUN: llc -mcpu=nanomips < %s | FileCheck %s

; CHECK-DAG: atomic_cmpxchg
; CHECK: llwp
; CHECK: scwp
; CHECK-DAG: .end atomic_cmpxchg

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
; CHECK-DAG: llwp
; CHECK-NOT: scwp
; CHECK-DAG: .end atomic_load

define noundef i64 @atomic_load(ptr noundef %0) {
  %2 = load atomic i64, ptr %0 monotonic, align 8
  ret i64 %2
}

; check reuse of load & exchange where old value is discarded
; CHECK-DAG: atomic_store
; CHECK: sync
; CHECK: llwp
; CHECK: or{{.*}}zero
; CHECK: or{{.*}}zero
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync
; CHECK-DAG: .end atomic_store

define dso_local void @atomic_store(ptr nocapture noundef writeonly %0, i64 noundef signext %1) {
  store atomic i64 %1, ptr %0 seq_cst, align 8
  ret void
}

; CHECK-DAG: atomicrmw_add_monotonic
; CHECK-NOT: sync
; CHECK-DAG: llwp
; CHECK: addu
; CHECK: addu
; CHECK: sltu
; CHECK: addu
; CHECK: scwp
; CHECK-DAG: beqzc
; CHECK-NOT: sync
; CHECK-DAG: .end atomicrmw_add_monotonic

define noundef i64 @atomicrmw_add_monotonic(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw add ptr %0, i64 %1 monotonic, align 8
  ret i64 %3
}

; all memory order variants are mapped to monotonic
; accompanied by different fence instructions

; CHECK-DAG: atomicrmw_add_acquire:
; CHECK-NOT: sync
; CHECK-DAG: llwp
; CHECK: addu
; CHECK: addu
; CHECK: sltu
; CHECK: addu
; CHECK: scwp
; CHECK-DAG: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_add_acquire

define noundef i64 @atomicrmw_add_acquire(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw add ptr %0, i64 %1 acquire, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_add_release
; CHECK: sync 0
; CHECK-DAG: llwp
; CHECK: addu
; CHECK: addu
; CHECK: sltu
; CHECK: addu
; CHECK: scwp
; CHECK-DAG: beqzc
; CHECK-NOT: sync
; CHECK-DAG: .end atomicrmw_add_release

define noundef i64 @atomicrmw_add_release(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw add ptr %0, i64 %1 release, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_add_acq_rel
; CHECK: sync 0
; CHECK-DAG: llwp
; CHECK: addu
; CHECK: addu
; CHECK: sltu
; CHECK: addu
; CHECK: scwp
; CHECK-DAG: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_add_acq_rel

define noundef i64 @atomicrmw_add_acq_rel(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw add ptr %0, i64 %1 acq_rel, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_add_seq_cst
; CHECK: sync 0
; CHECK: llwp
; CHECK: addu
; CHECK: addu
; CHECK: sltu
; CHECK: addu
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_add_seq_cst

define noundef i64 @atomicrmw_add_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw add ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_sub_seq_cst
; CHECK: sync 0
; CHECK: llwp
; CHECK: subu
; CHECK: subu
; CHECK: sltu
; CHECK: subu
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_sub_seq_cst

define noundef i64 @atomicrmw_sub_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw sub ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_and_seq_cst
; CHECK: sync 0
; CHECK: llwp
; CHECK: and
; CHECK: and
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_and_seq_cst

define noundef i64 @atomicrmw_and_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw and ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_or_seq_cst
; CHECK: sync 0
; CHECK: llwp
; CHECK: or
; CHECK: or
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_or_seq_cst

define noundef i64 @atomicrmw_or_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw or ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_xor_seq_cst
; CHECK: sync 0
; CHECK: llwp
; CHECK: xor
; CHECK: xor
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_xor_seq_cst

define noundef i64 @atomicrmw_xor_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw xor ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_nand_seq_cst
; CHECK: sync 0
; CHECK: llwp
; CHECK: and
; CHECK: nor {{.*}}zero
; CHECK: and
; CHECK: nor {{.*}}zero
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_nand_seq_cst

define noundef i64 @atomicrmw_nand_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw nand ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_min_seq_cst
; CHECK: sync 0
; CHECK: llwp
; CHECK: slt
; CHECK: sltu
; CHECK: xor
; CHECK: movn
; CHECK: or {{.*}}zero
; CHECK: movz
; CHECK: or {{.*}}zero
; CHECK: movz
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_min_seq_cst

define noundef i64 @atomicrmw_min_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw min ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_umin_seq_cst
; CHECK: sync 0
; CHECK: llwp
; CHECK: sltu
; The only difference w.r.t signed min
; CHECK: sltu
; CHECK: xor
; CHECK: movn
; CHECK: or {{.*}}zero
; CHECK: movz
; CHECK: or {{.*}}zero
; CHECK: movz
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_umin_seq_cst

define noundef i64 @atomicrmw_umin_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw umin ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}
; CHECK-DAG: atomicrmw_max_seq_cst:
; CHECK: sync 0
; CHECK: llwp
; CHECK: slt
; CHECK: sltu
; CHECK: xor
; CHECK: movn
; CHECK: or {{.*}}zero
; As opposed to movz in min
; CHECK: movn
; CHECK: or {{.*}}zero
; As opposed to movz in min
; CHECK: movn
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0

; CHECK-DAG: .end atomicrmw_max_seq_cst

define noundef i64 @atomicrmw_max_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw max ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_umax_seq_cst:
; CHECK: sync 0
; CHECK: llwp
; The only difference w.r.t signed max
; CHECK: sltu
; CHECK: sltu
; CHECK: xor
; CHECK: movn
; CHECK: or {{.*}}zero
; CHECK: movn
; CHECK: or {{.*}}zero
; CHECK: movn
; CHECK: scwp
; CHECK: beqzc
; CHECK-DAG: .end atomicrmw_umax_seq_cst

define noundef i64 @atomicrmw_umax_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw umax ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}

; CHECK-DAG: atomicrmw_xchg_seq_cst
; CHECK: sync 0
; CHECK: llwp
; CHECK: or {{.*}}zero
; CHECK: or {{.*}}zero
; CHECK: scwp
; CHECK: beqzc
; CHECK: sync 0
; CHECK-DAG: .end atomicrmw_xchg_seq_cst

define noundef i64 @atomicrmw_xchg_seq_cst(ptr noundef %0, i64 noundef signext %1) {
  %3 = atomicrmw xchg ptr %0, i64 %1 seq_cst, align 8
  ret i64 %3
}
