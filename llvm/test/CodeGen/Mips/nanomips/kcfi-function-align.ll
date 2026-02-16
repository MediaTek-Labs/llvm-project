; RUN: llc -mtriple=nanomips -nmips-guard-kcfi-prefetch=true -asm-show-inst < %s | FileCheck --check-prefixes=CHECK,GUARDED %s
; RUN: llc -mtriple=nanomips -asm-show-inst < %s | FileCheck --check-prefixes=CHECK,UNGUARDED %s

;; Function alignment unknown. Needs an align to 4 bytes, and no extra nops
; GUARDED: bc{{.*}}Ltmp
; UNGUARDED-NOT: bc{{.*}}Ltmp
; CHECK: .p2align 2
; CHECK-NOT: nop
; CHECK: .4byte{{.*}}0x993e738c
; CHECK-NOT: nop
; CHECK-label: test_0:
define dso_local i32 @test_0() !kcfi_type !3 {
  ret i32 0
}

;; Function known aligned to 4 bytes. Doesn't need an extra align unless we emit a guard.
; CHECK: .p2align 2
; GUARDED: bc{{.*}}Ltmp
; GUARDED: .p2align 2
; UNGUARDED-NOT: bc{{.*}}Ltmp
; UNGUARDED-NOT: .p2align
; CHECK: .4byte{{.*}}0x50794
; CHECK-LABEL: test_1:
define dso_local i32 @test_1(i32 noundef signext %0) align 4 !kcfi_type !4 {
  ret i32 1
}

;; 8-byte aligned. Needs some extra alignment and nops.
; CHECK: .p2align 3
; GUARDED: bc{{.*}}Ltmp
; UNGUARDED-NOT: bc{{.*}}Ltmp
; UNGUARDED: nop
; CHECK:     nop
; CHECK-NOT: nop
; CHECK: .4byte{{.*}}0x148a1a47
; CHECK-label: test_3:
define dso_local i32 @test_3(i32 noundef signext %0, i32 noundef signext %1,
                                 i32 noundef signext %2)
  align 8 !kcfi_type !10 {
  ret i32 3
}

;; 16-byte aligned. Needs lots of nops. For the unguarded case, (16-(4))/2 = 6,
;; for guarded (16-(4+2))/2 = 5 nops.
; CHECK: .p2align 4
; GUARDED: bc{{.*}}Ltmp
; UNGUARDED-NOT: bc{{.*}}Ltmp
; UNGUARDED: nop
; CHECK: nop
; CHECK: nop
; CHECK: nop
; CHECK: nop
; CHECK: nop
; CHECK-NOT: nop
; CHECK: .4byte{{.*}}0xab5888ac
; CHECK-label: test_6:
define dso_local i32 @test_6(i32 noundef signext %0, i32 noundef signext %1,
                             i32 noundef signext %2, i32 noundef signext %3,
                             i32 noundef signext %4, i32 noundef signext %5)
  align 16 !kcfi_type !13 {
  ret i32 6
}

!llvm.module.flags = !{!0, !1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 4, !"kcfi", i32 1}
!3 = !{i32 -1723960436}
!4 = !{i32 329620}
!10 = !{i32 344595015}
!13 = !{i32 -1420261204}
