; RUN: llc -mtriple=nanomips -asm-show-inst < %s | FileCheck %s




; ModuleID = 'signatures.bc'
;source_filename = "signatures.c"
;target datalayout = "e-m:e-p:32:32-i8:8:32-i16:16:32-i64:64-n32:64-S128"
;target triple = "nanomips-unknown-unknown-elf"

;; Signature is 16-bit instruction followed by 16-bit branch
; CHECK: bc{{.*}}Ltmp
; CHECK: .4byte{{.*}}0x993e738c
; CHECK-label: test_0:
define dso_local i32 @test_0() !kcfi_type !3 {
  ret i32 0
}

;; Signature is 32-bit LAPC, no workaround needed
; CHECK-NOT: bc{{.*}}Ltmp
; CHECK: .4byte{{.*}}0x50794
; CHECK-label: test_1:
define dso_local i32 @test_1(i32 noundef signext %0) !kcfi_type !4 {
  ret i32 1
}

;; Signature is 16-bit branch
; CHECK: bc{{.*}}Ltmp
; CHECK: .4byte{{.*}}0x148a1a47
; CHECK-label: test_3:
define dso_local i32 @test_3(i32 noundef signext %0, i32 noundef signext %1, i32 noundef signext %2) !kcfi_type !10 {
  ret i32 3
}

;; Signature is a 32-bit branch
; CHECK: bc{{.*}}Ltmp
; CHECK: .4byte{{.*}}0xab5888ac
; CHECK-label: test_6:
define dso_local i32 @test_6(i32 noundef signext %0, i32 noundef signext %1, i32 noundef signext %2, i32 noundef signext %3, i32 noundef signext %4, i32 noundef signext %5) !kcfi_type !13 {
  ret i32 6
}


!llvm.module.flags = !{!0, !1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 4, !"kcfi", i32 1}
!3 = !{i32 -1723960436}
!4 = !{i32 329620}
!10 = !{i32 344595015}
!13 = !{i32 -1420261204}
