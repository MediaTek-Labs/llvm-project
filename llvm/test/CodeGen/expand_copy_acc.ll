; check that copy accumulator is using nanomips' registers
; RUN: llc -mcpu=nanomips -mattr=+dsp -verify-machineinstrs < %s
define i32 @E_ACELP_toeplitz_mul() {
entry:
  %0 = tail call i64 @llvm.mips.maq.s.w.phr(i64 0, <2 x i16> zeroinitializer, <2 x i16> zeroinitializer)
  %1 = tail call i64 @llvm.mips.maq.s.w.phr(i64 0, <2 x i16> zeroinitializer, <2 x i16> zeroinitializer)
  %2 = tail call i64 @llvm.mips.maq.s.w.phr(i64 0, <2 x i16> zeroinitializer, <2 x i16> zeroinitializer)
  ret i32 0
}

; Function Attrs: nounwind
declare i64 @llvm.mips.maq.s.w.phr(i64, <2 x i16>, <2 x i16>) #0

; uselistorder directives
uselistorder ptr @llvm.mips.maq.s.w.phr, { 2, 1, 0 }

attributes #0 = { nounwind }
