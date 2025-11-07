; RUN: llc -mcpu=nanomips -mattr=+dsp   < %s | FileCheck %s

; Function Attrs: nofree nosync nounwind optsize memory(argmem: read)
define dso_local i64 @L40_dmac_loop(ptr nocapture noundef readonly %ptr32_x,
                                         ptr nocapture noundef readonly %ptr32_y,
                                         i32 noundef signext %i) local_unnamed_addr #0 {
entry:
  %0 = ptrtoint ptr %ptr32_x to i32
  %1 = shl i32 %i, 2
  %2 = add i32 %0, %1
  %3 = add i32 %2, -12
  %4 = inttoptr i32 %3 to ptr
; CHECK-LABEL: L40_dmac_loop
; CHECK-DAG: lw{{.*}}, -12({{.*}})
; CHECK-DAG: lw{{.*}}, -12({{.*}})
; CHECK-DAG: lw{{.*}}, -8({{.*}})
; CHECK-DAG: lw{{.*}}, -8({{.*}})
; CHECK-DAG: lw{{.*}}, -4({{.*}})
; CHECK-DAG: lw{{.*}}, -4({{.*}})
; CHECK-DAG: dpa.w.ph
; CHECK-DAG: dpa.w.ph
; CHECK-DAG: dpa.w.ph
; CHECK-DAG: dpa.w.ph
; CHECK-LABEL: end L40_dmac_loop
  %5 = load <2 x i16>, ptr %4, align 4
  %6 = ptrtoint ptr %ptr32_y to i32
  %7 = add i32 %6, %1
  %8 = add i32 %7, -12
  %9 = inttoptr i32 %8 to ptr
  %10 = load <2 x i16>, ptr %9, align 4
  %11 = tail call i64 @llvm.mips.dpa.w.ph(i64 0, <2 x i16> %5, <2 x i16> %10)
  %12 = add i32 %2, -8
  %13 = inttoptr i32 %12 to ptr
  %14 = load <2 x i16>, ptr %13, align 4
  %15 = add i32 %7, -8
  %16 = inttoptr i32 %15 to ptr
  %17 = load <2 x i16>, ptr %16, align 4
  %18 = tail call i64 @llvm.mips.dpa.w.ph(i64 %11, <2 x i16> %14, <2 x i16> %17)
  %19 = add i32 %2, -4
  %20 = inttoptr i32 %19 to ptr
  %21 = load <2 x i16>, ptr %20, align 4
  %22 = add i32 %7, -4
  %23 = inttoptr i32 %22 to ptr
  %24 = load <2 x i16>, ptr %23, align 4
  %25 = tail call i64 @llvm.mips.dpa.w.ph(i64 %18, <2 x i16> %21, <2 x i16> %24)
  %arrayidx13 = getelementptr inbounds i32, ptr %ptr32_x, i32 %i
  %26 = load <2 x i16>, ptr %arrayidx13, align 4
  %arrayidx14 = getelementptr inbounds i32, ptr %ptr32_y, i32 %i
  %27 = load <2 x i16>, ptr %arrayidx14, align 4
  %28 = tail call i64 @llvm.mips.dpa.w.ph(i64 %25, <2 x i16> %26, <2 x i16> %27)
  ret i64 %28
}

; Function Attrs: nofree nosync nounwind memory(none)
declare i64 @llvm.mips.dpa.w.ph(i64, <2 x i16>, <2 x i16>) #1

attributes #0 = { nofree nosync nounwind optsize memory(argmem: read) }
attributes #1 = { nofree nosync nounwind memory(none) }
