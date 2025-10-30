; RUN: llc -mcpu=nanomips -mattr=+dsp   < %s | FileCheck %s

define dso_local void @test1(ptr nocapture noundef writeonly %ptr32_z,
                             ptr nocapture noundef readonly %ptr32_x,
                             ptr nocapture noundef readonly %ptr32_y) local_unnamed_addr #0 {
entry:
  %0 = load <2 x i16>, ptr %ptr32_x, align 4
  %1 = load <2 x i16>, ptr %ptr32_y, align 4
  %2 = tail call i64 @llvm.mips.dpa.w.ph(i64 0, <2 x i16> %0, <2 x i16> %1)
; CHECK: test1
; CHECK: dpa
; CHECK: li [[ONE:\$[ats][0-9]+]], 1
; CHECK: madd $ac{{[0-3]}}, [[ONE]], [[ONE]]
  %add = add nsw i64 %2, 1
  %3 = tail call i32 @llvm.mips.extr.rs.w(i64 %add, i32 0)
  store i32 %3, ptr %ptr32_z, align 4
  ret void
}

define dso_local void @test9(ptr nocapture noundef writeonly %ptr32_z,
                             ptr nocapture noundef readonly %ptr32_x,
                             ptr nocapture noundef readonly %ptr32_y) local_unnamed_addr #0 {
entry:
  %0 = load <2 x i16>, ptr %ptr32_x, align 4
  %1 = load <2 x i16>, ptr %ptr32_y, align 4
  %2 = tail call i64 @llvm.mips.dpa.w.ph(i64 0, <2 x i16> %0, <2 x i16> %1)
; CHECK: test9
; CHECK: dpa
; CHECK-DAG: li [[NINE:\$[ats][0-9]+]], 9
; CHECK-DAG: li [[ONE:\$[ats][0-9]+]], 1
; CHECK: madd $ac{{[0-3]}}, [[NINE]], [[ONE]]
  %add = add nsw i64 %2, 9
  %3 = tail call i32 @llvm.mips.extr.rs.w(i64 %add, i32 0)
  store i32 %3, ptr %ptr32_z, align 4
  ret void
}

define dso_local void @testn(ptr nocapture noundef writeonly %ptr32_z,
                             ptr nocapture noundef readonly %ptr32_x,
                             ptr nocapture noundef readonly %ptr32_y,
                             i32 noundef signext %n) local_unnamed_addr #0 {
entry:
  %0 = load <2 x i16>, ptr %ptr32_x, align 4
  %1 = load <2 x i16>, ptr %ptr32_y, align 4
  %2 = tail call i64 @llvm.mips.dpa.w.ph(i64 0, <2 x i16> %0, <2 x i16> %1)
  %conv2 = zext i32 %n to i64
; CHECK: testn
; CHECK: dpa
; CHECK: li [[ONE:\$[ats][0-9]+]], 1
; CHECK: madd $ac{{[0-3]}}, ${{[ats][0-9]}}, [[ONE]]
  %add = add nsw i64 %2, %conv2
  %3 = tail call i32 @llvm.mips.extr.rs.w(i64 %add, i32 0)
  store i32 %3, ptr %ptr32_z, align 4
  ret void
}

define dso_local i64 @init0(i64 noundef signext %a,
                            i64 noundef signext %b) local_unnamed_addr {
entry:
  %conv.i = trunc i64 %a to i32
  %0 = bitcast i32 %conv.i to <2 x i16>
  %conv1.i = trunc i64 %b to i32
  %1 = bitcast i32 %conv1.i to <2 x i16>
; CHECK: init0
; CHECK: mult $ac{{[0-3]}}, $zero, $zero
  %2 = tail call i64 @llvm.mips.dpa.w.ph(i64 0, <2 x i16> %0, <2 x i16> %1)
  ret i64 %2
}

declare i64 @llvm.mips.dpa.w.ph(i64, <2 x i16>, <2 x i16>) #1
declare i32 @llvm.mips.extr.rs.w(i64, i32) #0
attributes #0 = { nounwind  }
attributes #1 = { nofree nosync nounwind memory(none) }
