; RUN: llc -mcpu=nanomips  < %s | FileCheck %s

@d1 = dso_local local_unnamed_addr global i16 0, align 4
@out = dso_local local_unnamed_addr global i16 0, align 4

define dso_local void @foo(i32 noundef signext %i) {
entry:
;
  %0 = load i16, ptr @d1, align 4
; CHECK: __gnu_h2f_ieee
  %1 = tail call float @llvm.convert.from.fp16.f32(i16 %0)
  %conv = uitofp i32 %i to float
  %add = fadd float %1, %conv
; CHECK: __gnu_f2h_ieee
; CHECK-NOT: __gnu_{{.*}}_ieee
  %2 = tail call i16 @llvm.convert.to.fp16.f32(float %add)
  store i16 %2, ptr @out, align 4
  ret void
}

declare float @llvm.convert.from.fp16.f32(i16)
declare i16 @llvm.convert.to.fp16.f32(float)
