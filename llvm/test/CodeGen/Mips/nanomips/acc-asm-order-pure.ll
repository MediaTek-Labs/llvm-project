; RUN: llc -mcpu=nanomips -mattr=+dsp  < %s | FileCheck %s
; Function Attrs: nounwind
; CHECK-COUNT-4:  dummy use 
define dso_local i64 @pure_asm() local_unnamed_addr #0 {
entry:
  %0 = tail call i64 @llvm.mips.madd(i64 0, i32 4660, i32 22136)
  %1 = tail call i64 asm  "# dummy use 01 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %0) #2
  %2 = tail call i64 asm  "# dummy use 02 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %1) #2
  %3 = tail call i64 asm  "# dummy use 03 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %2) #2
  %4 = tail call i64 asm  "# dummy use 04 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %3) #2
  ret i64 %4
}

; Function Attrs: nofree nosync nounwind memory(none)
declare i64 @llvm.mips.madd(i64, i32, i32) #1

; Function Attrs: nofree nosync nounwind memory(none)

attributes #0 = { nounwind  "target-features"="+dsp" }
attributes #1 = { nofree nosync nounwind memory(none) }
attributes #2 = { nounwind }
