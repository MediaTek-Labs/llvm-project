; RUN: llc -mcpu=nanomips -mattr=+dsp < %s | FileCheck %s

; verify there is no redundant moves between accumulators
; CHECK-LABEL: acc_back2back:
; CHECK: madd [[ACC:\$ac[0-3]]]
; CHECK-NEXT: maddu [[ACC]]
; CHECK-NEXT: mthlip{{.*}}, [[ACC]]
; CHECK-NEXT: msub [[ACC]]
; CHECK-NEXT: msubu [[ACC]]

; Function Attrs: nofree nosync nounwind memory(none)
define dso_local i64 @acc_back2back(i32 noundef signext %0, i32 noundef signext %1,
                                    i32 noundef signext %2,
                                    i64 noundef signext %3) local_unnamed_addr #0 {
  %5 = tail call i64 @llvm.mips.madd(i64 %3, i32 %0, i32 %1)
  %6 = tail call i64 @llvm.mips.maddu(i64 %5, i32 %2, i32 %1)
  %7 = tail call i64 @llvm.mips.mthlip(i64 %6, i32 %0)
  %8 = tail call i64 @llvm.mips.msub(i64 %7, i32 %2, i32 %1)
  %9 = tail call i64 @llvm.mips.msubu(i64 %8, i32 %0, i32 %2)
  ret i64 %9
}
declare i64 @llvm.mips.madd(i64, i32, i32) nounwind readnone
declare i64 @llvm.mips.mthlip(i64, i32) nounwind
declare i64 @llvm.mips.maddu(i64, i32, i32) nounwind readnone
declare i64 @llvm.mips.msub(i64, i32, i32) nounwind readnone
declare i64 @llvm.mips.msubu(i64, i32, i32) nounwind readnone
