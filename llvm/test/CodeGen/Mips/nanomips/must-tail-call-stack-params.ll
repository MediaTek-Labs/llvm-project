;; RUN: clang -Os %s  -S -o - | FileCheck %s
source_filename = "2007-01-04-KNR-Args.c"
target datalayout = "e-m:e-p:32:32-i8:8:32-i16:16:32-i64:64-n32:64-S128"
target triple = "nanomips-unknown-unknown-elf"

define i32 @p1(i32 %0, double %1, i32 %2, double %d1, i32 %i, double %3, i32 %l, double %d2) {
entry:
  %f2.addr = alloca float, align 4
  %f2 = fptrunc double %3 to float
  store float %f2, float* %f2.addr, align 4, !tbaa !0
  %conv = zext i8 undef to i32
  %conv4 = fpext float undef to double
  %conv5 = sext i16 undef to i32
  %4 = load double, double* undef, align 8, !tbaa !4
  %5 = load i32, i32* undef, align 4, !tbaa !6
  %6 = load float, float* %f2.addr, align 4, !tbaa !0
  %conv6 = fpext float %6 to double
  %7 = load i32, i32* undef, align 4, !tbaa !8
  %8 = load double, double* undef, align 8, !tbaa !4
;; CHECK: restore
;; CHECK: bc	printf
  %call = call i32 (i8*, ...) @printf(i8* poison, i32 %conv, double %conv4, i32 %conv5, double %4, i32 %5, double %conv6, i32 %7, double %8)
  ret i32 undef
}

declare i32 @printf(i8*, ...)

!0 = !{!1, !1, i64 0}
!1 = !{!"float", !2, i64 0}
!2 = !{!"omnipotent char", !3, i64 0}
!3 = !{!"Simple C/C++ TBAA"}
!4 = !{!5, !5, i64 0}
!5 = !{!"double", !2, i64 0}
!6 = !{!7, !7, i64 0}
!7 = !{!"int", !2, i64 0}
!8 = !{!9, !9, i64 0}
!9 = !{!"long", !2, i64 0}
