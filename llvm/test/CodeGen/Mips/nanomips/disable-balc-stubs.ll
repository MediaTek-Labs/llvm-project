; Should enable REQUIRES: nanomips
; ModuleID = 'disable-balc-stubs.c'
source_filename = "disable-balc-stubs.c"
target datalayout = "e-m:e-p:32:32-i8:8:32-i16:16:32-i64:64-n32:64-S128"
target triple = "nanomips"

; RUN: llc %s -o - | FileCheck %s
; RUN: llc %s --disable-nanomips-balc-stubs -o - | FileCheck %s --check-prefix=CHECK-DISABLED


; Function Attrs: noinline nounwind optnone
define dso_local void @y() #0 {
entry:
  ret void
}

; Function Attrs: noinline nounwind optsize
; CHECK: f:
; CHECK-DISABLED: f:
define dso_local void @f() #1 {
entry:
  ; CHECK-NOT: R_NANOMIPS_NOTRAMP
  ; CHECK-DISABLED: R_NANOMIPS_NOTRAMP 
  call void @y()
  ret void
}

; Function Attrs: noinline nounwind optnone
; CHECK: g:
; CHECK-DISABLED: g:
define dso_local void @g() #0 {
entry:
  ; CHECK: R_NANOMIPS_NOTRAMP
  call void @y()
  ret void
}

attributes #0 = { noinline nounwind optnone "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="i7200" "target-features"="+i7200,+pcrel,+relax,+soft-float,-noabicalls" "use-soft-float"="true" }
attributes #1 = { noinline nounwind optsize "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="i7200" "target-features"="+i7200,+pcrel,+relax,+soft-float,-noabicalls" "use-soft-float"="true" }
