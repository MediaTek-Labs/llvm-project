;RUN:  llc -mcpu=nanomips -mattr=+dsp < %s |  FileCheck %s
; Function Attrs: nofree nosync nounwind memory(none)
declare <2 x i16> @llvm.mips.subq.s.ph(<2 x i16>, <2 x i16>) #1

; CHECK-DAG: foo:
; CHECK-NOT: ($sp)
; CHECK-DAG: subq_s.ph
; CHECK-NOT: ($sp)
; CHECK-DAG: Lfunc_end0:

; Function Attrs: nofree nosync nounwind memory(none)
define dso_local zeroext i16 @foo(i16 noundef zeroext %var1,
                                  i16 noundef zeroext %var2) local_unnamed_addr #1 {
entry:
  %0 = insertelement <2 x i16> <i16 poison, i16 0>, i16 %var1, i64 0
  %1 = insertelement <2 x i16> <i16 poison, i16 0>, i16 %var2, i64 0
  %2 = tail call <2 x i16> @llvm.mips.subq.s.ph(<2 x i16> %0, <2 x i16> %1)
  %conv2 = extractelement <2 x i16> %2, i64 0
  ret i16 %conv2
}

; compilation of this function may hang
; if we do NOT disable conversion of
;   truncate (extract v2i16:0)
; into
;   extract_vector_element v2i16, 0
; since we do the reverse conversion as well
define i32 @hang() {
  %1 = load <2 x i16>, ptr null, align 4
  %2 = extractelement <2 x i16> %1, i64 0
  %3 = sub i16 0, %2
  store i16 %3, ptr null, align 4
  ret i32 0
}

define dso_local signext i16 @no_zext() local_unnamed_addr {
entry:
; CHECK-LABEL: no_zext
; CHECK-DAG: li {{.*}}, 1
; CHECK-DAG: li {{.*}}, 2
; CHECK-NOT: lw
; CHECK: addq_s.ph

  %0 = tail call <2 x i16> @llvm.mips.addq.s.ph(<2 x i16> <i16 2, i16 0>,
                                                <2 x i16> <i16 1, i16 0>)
  %conv2.i = extractelement <2 x i16> %0, i64 0
  ret i16 %conv2.i
}

; Function Attrs: nofree nosync nounwind memory(none)
declare <2 x i16> @llvm.mips.addq.s.ph(<2 x i16>, <2 x i16>) #1

attributes #1 = { nofree nosync nounwind memory(none) }
