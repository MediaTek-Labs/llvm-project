; RUN: llc -mcpu=nanomips -mattr=+dsp -print-after=mips-isel -o /dev/null  < %s |& FileCheck %s
; Function Attrs: nounwind
define dso_local i64 @pure_asm() local_unnamed_addr #0 {
entry:
; CHECK: [[ACC1:%[0-9]+:acc64dspnm]] = MADD_DSP_NM
  %0 = tail call i64 @llvm.mips.madd(i64 0, i32 4660, i32 22136)

; CHECK-DAG: [[GPR01_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC1]]
; CHECK-DAG: [[GPR01_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC1]]
; CHECK:  [[ACC01_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR01_IN_LO]], {{.*}} [[GPR01_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC01_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC01_IN]]
  %1 = tail call i64 asm sideeffect "# dummy use 01 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %0) #2

; CHECK-DAG: [[GPR02_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC01_OUT]]
; CHECK-DAG: [[GPR02_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC01_OUT]]
; CHECK:  [[ACC02_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR02_IN_LO]], {{.*}} [[GPR02_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC02_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC02_IN]]
  %2 = tail call i64 asm sideeffect "# dummy use 02 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %1) #2

; CHECK-DAG: [[GPR03_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC02_OUT]]
; CHECK-DAG: [[GPR03_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC02_OUT]]
; CHECK:  [[ACC03_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR03_IN_LO]], {{.*}} [[GPR03_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC03_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC03_IN]]
  %3 = tail call i64 asm sideeffect "# dummy use 03 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %2) #2

; CHECK-DAG: [[GPR04_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC03_OUT]]
; CHECK-DAG: [[GPR04_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC03_OUT]]
; CHECK:  [[ACC04_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR04_IN_LO]], {{.*}} [[GPR04_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC04_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC04_IN]]
  %4 = tail call i64 asm sideeffect "# dummy use 04 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %3) #2

; CHECK-DAG: [[GPR05_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC04_OUT]]
; CHECK-DAG: [[GPR05_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC04_OUT]]
; CHECK:  [[ACC05_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR05_IN_LO]], {{.*}} [[GPR05_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC05_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC05_IN]]
  %5 = tail call i64 asm sideeffect "# dummy use 05 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %4) #2

; CHECK-DAG: [[GPR06_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC05_OUT]]
; CHECK-DAG: [[GPR06_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC05_OUT]]
; CHECK:  [[ACC06_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR06_IN_LO]], {{.*}} [[GPR06_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC06_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC06_IN]]
  %6 = tail call i64 asm sideeffect "# dummy use 06 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %5) #2

; CHECK-DAG: [[GPR07_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC06_OUT]]
; CHECK-DAG: [[GPR07_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC06_OUT]]
; CHECK:  [[ACC07_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR07_IN_LO]], {{.*}} [[GPR07_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC07_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC07_IN]]
  %7 = tail call i64 asm sideeffect "# dummy use 07 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %6) #2

; CHECK-DAG: [[GPR08_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC07_OUT]]
; CHECK-DAG: [[GPR08_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC07_OUT]]
; CHECK:  [[ACC08_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR08_IN_LO]], {{.*}} [[GPR08_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC08_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC08_IN]]
  %8 = tail call i64 asm sideeffect "# dummy use 08 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %7) #2

; CHECK-DAG: [[GPR09_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC08_OUT]]
; CHECK-DAG: [[GPR09_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC08_OUT]]
; CHECK:  [[ACC09_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR09_IN_LO]], {{.*}} [[GPR09_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC09_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC09_IN]]
  %9 = tail call i64 asm sideeffect "# dummy use 09 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %8) #2

; CHECK-DAG: [[GPR10_IN_HI:%[0-9]+:gprnm32]] = MFHI_NM [[ACC09_OUT]]
; CHECK-DAG: [[GPR10_IN_LO:%[0-9]+:gprnm32]] = MFLO_NM [[ACC09_OUT]]
; CHECK:  [[ACC10_IN:%[0-9]+:acc64dspnm]] = PseudoMTLOHI_DSP_NM {{.*}} [[GPR10_IN_LO]], {{.*}} [[GPR10_IN_HI]]
; CHECK: INLINEASM {{.*}} [[ACC10_OUT:%[0-9]+:acc64dspnm]]{{.*}}[[ACC10_IN]]
  %10 = tail call i64 asm sideeffect "# dummy use 10 $0 $1 \0A\09", "=A,A,~{$1}"(i64 %9) #2
  ret i64 %10
}

; Function Attrs: nofree nosync nounwind memory(none)
declare i64 @llvm.mips.madd(i64, i32, i32) #1

; Function Attrs: nofree nosync nounwind memory(none)

attributes #0 = { nounwind  "target-features"="+dsp" }
attributes #1 = { nofree nosync nounwind memory(none) }
attributes #2 = { nounwind }
