; RUN: llc -mcpu=nanomips -mattr=+dsp < %s | FileCheck %s

; CHECK-LABEL: test_add_v2q15_:
; CHECK: addq.ph $a{{[0-9]+}}

define { i32 } @test_add_v2q15_(i32 %a.coerce, i32 %b.coerce) {
entry:
  %0 = bitcast i32 %a.coerce to <2 x i16>
  %1 = bitcast i32 %b.coerce to <2 x i16>
  %add = add <2 x i16> %0, %1
  %2 = bitcast <2 x i16> %add to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

; CHECK-LABEL: test_sub_v2q15_:
; CHECK: subq.ph $a{{[0-9]+}}

define { i32 } @test_sub_v2q15_(i32 %a.coerce, i32 %b.coerce) {
entry:
  %0 = bitcast i32 %a.coerce to <2 x i16>
  %1 = bitcast i32 %b.coerce to <2 x i16>
  %sub = sub <2 x i16> %0, %1
  %2 = bitcast <2 x i16> %sub to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

;; CHxxxECK-LABEL: test_mul_v2q15_:
;; CHxxxECK: mul.ph $a{{[0-9]+}}
;
;define { i32 } @test_mul_v2q15_(i32 %a.coerce, i32 %b.coerce) {
;entry:
;  %0 = bitcast i32 %a.coerce to <2 x i16>
;  %1 = bitcast i32 %b.coerce to <2 x i16>
;  %mul = mul <2 x i16> %0, %1
;  %2 = bitcast <2 x i16> %mul to i32
;  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
;  ret { i32 } %.fca.0.insert
;}

; CHECK-LABEL: test_add_v4i8_:
; CHECK: addu.qb $a{{[0-9]+}}

define { i32 } @test_add_v4i8_(i32 %a.coerce, i32 %b.coerce) {
entry:
  %0 = bitcast i32 %a.coerce to <4 x i8>
  %1 = bitcast i32 %b.coerce to <4 x i8>
  %add = add <4 x i8> %0, %1
  %2 = bitcast <4 x i8> %add to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

; CHECK-LABEL: test_sub_v4i8_:
; CHECK: subu.qb $a{{[0-9]+}}

define { i32 } @test_sub_v4i8_(i32 %a.coerce, i32 %b.coerce) {
entry:
  %0 = bitcast i32 %a.coerce to <4 x i8>
  %1 = bitcast i32 %b.coerce to <4 x i8>
  %sub = sub <4 x i8> %0, %1
  %2 = bitcast <4 x i8> %sub to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

;; DSP-ASE doesn't have a v4i8 multiply instruction. Check that multiply node gets expanded.
;; CHxxxECK-LABEL: test_mul_v4i8_:
;; CHxxxECK: mul $a{{[0-9]+}}
;; CHxxxECK: mul $a{{[0-9]+}}
;; CHxxxECK: mul $a{{[0-9]+}}
;; CHxxxECK: mul $a{{[0-9]+}}
;
;define { i32 } @test_mul_v4i8_(i32 %a.coerce, i32 %b.coerce) {
;entry:
;  %0 = bitcast i32 %a.coerce to <4 x i8>
;  %1 = bitcast i32 %b.coerce to <4 x i8>
;  %mul = mul <4 x i8> %0, %1
;  %2 = bitcast <4 x i8> %mul to i32
;  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
;  ret { i32 } %.fca.0.insert
;}

;; CHxxxECK-LABEL: test_addsc:
;; CHxxxECK: addsc $a{{[0-9]+}}
;; CHxxxECK: addwc $a{{[0-9]+}}
;
;define i64 @test_addsc(i64 %a, i64 %b) {
;entry:
;  %add = add nsw i64 %b, %a
;  ret i64 %add
;}

; CHECK-LABEL: shift1_v2i16_shl_:
; CHECK: shll.ph $a{{[0-9]+}}, $a{{[0-9]+}}, 15

define { i32 } @shift1_v2i16_shl_(i32 %a0.coerce) {
entry:
  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %shl = shl <2 x i16> %0, <i16 15, i16 15>
  %1 = bitcast <2 x i16> %shl to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
  ret { i32 } %.fca.0.insert
}

;; CHxxxECK-LABEL: shift1_v2i16_sra_:
;; CHxxxECK: shra.ph $a{{[0-9]+}}, $a{{[0-9]+}}, 15
;
;define { i32 } @shift1_v2i16_sra_(i32 %a0.coerce) {
;entry:
;  %0 = bitcast i32 %a0.coerce to <2 x i16>
;  %shr = ashr <2 x i16> %0, <i16 15, i16 15>
;  %1 = bitcast <2 x i16> %shr to i32
;  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
;  ret { i32 } %.fca.0.insert
;}

; CHECK-LABEL: shift1_v2ui16_srl_:
; CHECK: shrl.ph $a{{[0-9]+}}, $a{{[0-9]+}}, 15

define { i32 } @shift1_v2ui16_srl_(i32 %a0.coerce) {
entry:
  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %shr = lshr <2 x i16> %0, <i16 15, i16 15>
  %1 = bitcast <2 x i16> %shr to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
  ret { i32 } %.fca.0.insert
}

; CHECK-LABEL: shift1_v4i8_shl_:
; CHECK: shll.qb $a{{[0-9]+}}, $a{{[0-9]+}}, 7

define { i32 } @shift1_v4i8_shl_(i32 %a0.coerce) {
entry:
  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %shl = shl <4 x i8> %0, <i8 7, i8 7, i8 7, i8 7>
  %1 = bitcast <4 x i8> %shl to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
  ret { i32 } %.fca.0.insert
}

;; CHxxxECK-LABEL: shift1_v4i8_sra_:
;; CHxxxECK: shra.qb $a{{[0-9]+}}, $a{{[0-9]+}}, 7
;
;define { i32 } @shift1_v4i8_sra_(i32 %a0.coerce) {
;entry:
;  %0 = bitcast i32 %a0.coerce to <4 x i8>
;  %shr = ashr <4 x i8> %0, <i8 7, i8 7, i8 7, i8 7>
;  %1 = bitcast <4 x i8> %shr to i32
;  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
;  ret { i32 } %.fca.0.insert
;}

; CHECK-LABEL: shift1_v4ui8_srl_:
; CHECK: shrl.qb $a{{[0-9]+}}, $a{{[0-9]+}}, 7

define { i32 } @shift1_v4ui8_srl_(i32 %a0.coerce) {
entry:
  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %shr = lshr <4 x i8> %0, <i8 7, i8 7, i8 7, i8 7>
  %1 = bitcast <4 x i8> %shr to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
  ret { i32 } %.fca.0.insert
}

; Check that shift node is expanded if splat element size is not 16-bit.
;
; CHECK-LABEL: test_vector_splat_imm_v2q15:
; CHECK-NOT: shll.ph

define { i32 } @test_vector_splat_imm_v2q15(i32 %a.coerce) {
entry:
  %0 = bitcast i32 %a.coerce to <2 x i16>
  %shl = shl <2 x i16> %0, <i16 0, i16 2>
  %1 = bitcast <2 x i16> %shl to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
  ret { i32 } %.fca.0.insert
}

; Check that shift node is expanded if splat element size is not 8-bit.
;
; CHECK-LABEL: test_vector_splat_imm_v4i8:
; CHECK-NOT: shll.qb

define { i32 } @test_vector_splat_imm_v4i8(i32 %a.coerce) {
entry:
  %0 = bitcast i32 %a.coerce to <4 x i8>
  %shl = shl <4 x i8> %0, <i8 0, i8 2, i8 0, i8 2>
  %1 = bitcast <4 x i8> %shl to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
  ret { i32 } %.fca.0.insert
}

; Check that shift node is expanded if shift amount doesn't fit in 4-bit sa field.
;
; CHECK-LABEL: test_shift_amount_v2q15:
; CHECK-NOT: shll.ph

define { i32 } @test_shift_amount_v2q15(i32 %a.coerce) {
entry:
  %0 = bitcast i32 %a.coerce to <2 x i16>
  %shl = shl <2 x i16> %0, <i16 16, i16 16>
  %1 = bitcast <2 x i16> %shl to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
  ret { i32 } %.fca.0.insert
}

; Check that shift node is expanded if shift amount doesn't fit in 3-bit sa field.
;
; CHECK-LABEL: test_shift_amount_v4i8:
; CHECK-NOT: shll.qb

define { i32 } @test_shift_amount_v4i8(i32 %a.coerce) {
entry:
  %0 = bitcast i32 %a.coerce to <4 x i8>
  %shl = shl <4 x i8> %0, <i8 8, i8 8, i8 8, i8 8>
  %1 = bitcast <4 x i8> %shl to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
  ret { i32 } %.fca.0.insert
}
