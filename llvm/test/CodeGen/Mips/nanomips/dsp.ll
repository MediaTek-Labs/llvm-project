; RUN: llc -mcpu=nanomips -mattr=+dsp -verify-machineinstrs < %s | \
; RUN:     FileCheck %s

define i32 @test__builtin_mips_extr_w1(i32 %i0, i32, i64 %a0) nounwind {
entry:
; CHECK: extr.w $

  %1 = tail call i32 @llvm.mips.extr.w(i64 %a0, i32 15)
  ret i32 %1
}

declare i32 @llvm.mips.extr.w(i64, i32) nounwind

define i32 @test__builtin_mips_extr_w2(i32 %i0, i32, i64 %a0, i32 %a1) nounwind {
entry:
; CHECK: extrv.w $

  %1 = tail call i32 @llvm.mips.extr.w(i64 %a0, i32 %a1)
  ret i32 %1
}

define i32 @test__builtin_mips_extr_r_w1(i32 %i0, i32, i64 %a0) nounwind {
entry:
; CHECK: extr_r.w $

  %1 = tail call i32 @llvm.mips.extr.r.w(i64 %a0, i32 15)
  ret i32 %1
}

declare i32 @llvm.mips.extr.r.w(i64, i32) nounwind

;define i32 @test__builtin_mips_extr_s_h1(i32 %i0, i32, i64 %a0, i32 %a1) nounwind {
;entry:
;; CHxxxECK: extrv_s.h
;
;  %1 = tail call i32 @llvm.mips.extr.s.h(i64 %a0, i32 %a1)
;  ret i32 %1
;}
;
;declare i32 @llvm.mips.extr.s.h(i64, i32) nounwind

define i32 @test__builtin_mips_extr_rs_w1(i32 %i0, i32, i64 %a0) nounwind {
entry:
; CHECK: extr_rs.w $

  %1 = tail call i32 @llvm.mips.extr.rs.w(i64 %a0, i32 15)
  ret i32 %1
}

declare i32 @llvm.mips.extr.rs.w(i64, i32) nounwind

define i32 @test__builtin_mips_extr_rs_w2(i32 %i0, i32, i64 %a0, i32 %a1) nounwind {
entry:
; CHECK: extrv_rs.w $

  %1 = tail call i32 @llvm.mips.extr.rs.w(i64 %a0, i32 %a1)
  ret i32 %1
}

;define i32 @test__builtin_mips_extr_s_h2(i32 %i0, i32, i64 %a0) nounwind {
;entry:
;; CHxxxECK: extr_s.h
;
;  %1 = tail call i32 @llvm.mips.extr.s.h(i64 %a0, i32 15)
;  ret i32 %1
;}

define i32 @test__builtin_mips_extr_r_w2(i32 %i0, i32, i64 %a0, i32 %a1) nounwind {
entry:
; CHECK: extrv_r.w $

  %1 = tail call i32 @llvm.mips.extr.r.w(i64 %a0, i32 %a1)
  ret i32 %1
}

define i32 @test__builtin_mips_extp1(i32 %i0, i32, i64 %a0) nounwind {
entry:
; CHECK: extp {{.*}}, 15

  %1 = tail call i32 @llvm.mips.extp(i64 %a0, i32 15)
  ret i32 %1
}

declare i32 @llvm.mips.extp(i64, i32) nounwind

define i32 @test__builtin_mips_extp2(i32 %i0, i32, i64 %a0, i32 %a1) nounwind {
entry:
; CHECK: extpv $

  %1 = tail call i32 @llvm.mips.extp(i64 %a0, i32 %a1)
  ret i32 %1
}

define i32 @test__builtin_mips_extpdp1(i32 %i0, i32, i64 %a0) nounwind {
entry:
; CHECK: extpdp {{.*}}, 15

  %1 = tail call i32 @llvm.mips.extpdp(i64 %a0, i32 15)
  ret i32 %1
}

declare i32 @llvm.mips.extpdp(i64, i32) nounwind

define i32 @test__builtin_mips_extpdp2(i32 %i0, i32, i64 %a0, i32 %a1) nounwind {
entry:
; CHECK: extpdpv $

  %1 = tail call i32 @llvm.mips.extpdp(i64 %a0, i32 %a1)
  ret i32 %1
}

define i64 @test__builtin_mips_shilo1(i32 %i0, i32, i64 %a0) nounwind readnone {
entry:
; CHECK: shilo $ac{{[0-9]}}

  %1 = tail call i64 @llvm.mips.shilo(i64 %a0, i32 0)
  ret i64 %1
}

declare i64 @llvm.mips.shilo(i64, i32) nounwind readnone

define i64 @test__builtin_mips_shilo2(i32 %i0, i32, i64 %a0, i32 %a1) nounwind readnone {
entry:
; CHECK: shilov $

  %1 = tail call i64 @llvm.mips.shilo(i64 %a0, i32 %a1)
  ret i64 %1
}

define i64 @test__builtin_mips_mthlip1(i32 %i0, i32, i64 %a0, i32 %a1) nounwind {
entry:
; CHECK: mthlip $

  %1 = tail call i64 @llvm.mips.mthlip(i64 %a0, i32 %a1)
  ret i64 %1
}

declare i64 @llvm.mips.mthlip(i64, i32) nounwind

define i32 @test__builtin_mips_bposge321(i32 %i0) nounwind readonly {
entry:
; CHECK: bposge32c .LBB

  %0 = tail call i32 @llvm.mips.bposge32()
  ret i32 %0
}

declare i32 @llvm.mips.bposge32() nounwind readonly

define i64 @test__builtin_mips_mult1(i32 %i0, i32 %a0, i32 %a1) nounwind readnone {
entry:
; CHECK: mult $ac{{[0-9]}}

  %0 = tail call i64 @llvm.mips.mult(i32 %a0, i32 %a1)
  ret i64 %0
}

declare i64 @llvm.mips.mult(i32, i32) nounwind readnone

define i64 @test__builtin_mips_multu1(i32 %i0, i32 %a0, i32 %a1) nounwind readnone {
entry:
; CHECK: multu $ac{{[0-9]}}

  %0 = tail call i64 @llvm.mips.multu(i32 %a0, i32 %a1)
  ret i64 %0
}

declare i64 @llvm.mips.multu(i32, i32) nounwind readnone

define i64 @test__builtin_mips_madd1(i32 %i0, i32, i64 %a0, i32 %a1, i32 %a2) nounwind readnone {
entry:
; CHECK: madd $ac{{[0-9]}}

  %1 = tail call i64 @llvm.mips.madd(i64 %a0, i32 %a1, i32 %a2)
  ret i64 %1
}

declare i64 @llvm.mips.madd(i64, i32, i32) nounwind readnone

define i64 @test__builtin_mips_maddu1(i32 %i0, i32, i64 %a0, i32 %a1, i32 %a2) nounwind readnone {
entry:
; CHECK: maddu $ac{{[0-9]}}

  %1 = tail call i64 @llvm.mips.maddu(i64 %a0, i32 %a1, i32 %a2)
  ret i64 %1
}

declare i64 @llvm.mips.maddu(i64, i32, i32) nounwind readnone

define i64 @test__builtin_mips_msub1(i32 %i0, i32, i64 %a0, i32 %a1, i32 %a2) nounwind readnone {
entry:
; CHECK: msub $ac{{[0-9]}}

  %1 = tail call i64 @llvm.mips.msub(i64 %a0, i32 %a1, i32 %a2)
  ret i64 %1
}

declare i64 @llvm.mips.msub(i64, i32, i32) nounwind readnone

define i64 @test__builtin_mips_msubu1(i32 %i0, i32, i64 %a0, i32 %a1, i32 %a2) nounwind readnone {
entry:
; CHECK: msubu $ac{{[0-9]}}

  %1 = tail call i64 @llvm.mips.msubu(i64 %a0, i32 %a1, i32 %a2)
  ret i64 %1
}

declare i64 @llvm.mips.msubu(i64, i32, i32) nounwind readnone

define i32 @test__builtin_mips_modsub1(i32 %i0, i32 %a0, i32 %a1) nounwind readnone {
entry:
; CHECK: modsub $

  %0 = tail call i32 @llvm.mips.modsub(i32 %a0, i32 %a1)
  ret i32 %0
}

declare i32 @llvm.mips.modsub(i32, i32) nounwind readnone

define { i32 } @test__builtin_mips_addq_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: addq.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  %2 = tail call <2 x i16> @llvm.mips.addq.ph(<2 x i16> %0, <2 x i16> %1)
  %3 = bitcast <2 x i16> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.addq.ph(<2 x i16>, <2 x i16>) nounwind

define { i32 } @test__builtin_mips_addq_s_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: addq_s.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  %2 = tail call <2 x i16> @llvm.mips.addq.s.ph(<2 x i16> %0, <2 x i16> %1)
  %3 = bitcast <2 x i16> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.addq.s.ph(<2 x i16>, <2 x i16>) nounwind

define { i32 } @test__builtin_mips_addu_qb1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: addu.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = bitcast i32 %a1.coerce to <4 x i8>
  %2 = tail call <4 x i8> @llvm.mips.addu.qb(<4 x i8> %0, <4 x i8> %1)
  %3 = bitcast <4 x i8> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <4 x i8> @llvm.mips.addu.qb(<4 x i8>, <4 x i8>) nounwind

define { i32 } @test__builtin_mips_addu_s_qb1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: addu_s.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = bitcast i32 %a1.coerce to <4 x i8>
  %2 = tail call <4 x i8> @llvm.mips.addu.s.qb(<4 x i8> %0, <4 x i8> %1)
  %3 = bitcast <4 x i8> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <4 x i8> @llvm.mips.addu.s.qb(<4 x i8>, <4 x i8>) nounwind

define { i32 } @test__builtin_mips_subq_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: subq.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  %2 = tail call <2 x i16> @llvm.mips.subq.ph(<2 x i16> %0, <2 x i16> %1)
  %3 = bitcast <2 x i16> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.subq.ph(<2 x i16>, <2 x i16>) nounwind

define { i32 } @test__builtin_mips_subq_s_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: subq_s.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  %2 = tail call <2 x i16> @llvm.mips.subq.s.ph(<2 x i16> %0, <2 x i16> %1)
  %3 = bitcast <2 x i16> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.subq.s.ph(<2 x i16>, <2 x i16>) nounwind

define i32 @test__builtin_mips_subq_s_w1(i32 %i0, i32 %a0, i32 %a1) nounwind {
entry:
; CHECK: subq_s.w $

  %0 = tail call i32 @llvm.mips.subq.s.w(i32 %a0, i32 %a1)
  ret i32 %0
}

declare i32 @llvm.mips.subq.s.w(i32, i32) nounwind

define { i32 } @test__builtin_mips_subu_qb1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: subu.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = bitcast i32 %a1.coerce to <4 x i8>
  %2 = tail call <4 x i8> @llvm.mips.subu.qb(<4 x i8> %0, <4 x i8> %1)
  %3 = bitcast <4 x i8> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <4 x i8> @llvm.mips.subu.qb(<4 x i8>, <4 x i8>) nounwind

define { i32 } @test__builtin_mips_subu_s_qb1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: subu_s.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = bitcast i32 %a1.coerce to <4 x i8>
  %2 = tail call <4 x i8> @llvm.mips.subu.s.qb(<4 x i8> %0, <4 x i8> %1)
  %3 = bitcast <4 x i8> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <4 x i8> @llvm.mips.subu.s.qb(<4 x i8>, <4 x i8>) nounwind

define i32 @test__builtin_mips_append1(i32 %i0, i32 %a0, i32 %a1) nounwind readnone {
entry:
; CHECK: append $

  %0 = tail call i32 @llvm.mips.append(i32 %a0, i32 %a1, i32 15)
  ret i32 %0
}

declare i32 @llvm.mips.append(i32, i32, i32) nounwind readnone

define { i32 } @test__builtin_mips_precrq_ph_w1(i32 %i0, i32 %a0, i32 %a1) nounwind readnone {
entry:
; CHECK: precrq.ph.w $

  %0 = tail call <2 x i16> @llvm.mips.precrq.ph.w(i32 %a0, i32 %a1)
  %1 = bitcast <2 x i16> %0 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %1, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.precrq.ph.w(i32, i32) nounwind readnone

define i32 @test__builtin_mips_cmpgu_eq_qb1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: cmpgu.eq.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = bitcast i32 %a1.coerce to <4 x i8>
  %2 = tail call i32 @llvm.mips.cmpgu.eq.qb(<4 x i8> %0, <4 x i8> %1)
  ret i32 %2
}

declare i32 @llvm.mips.cmpgu.eq.qb(<4 x i8>, <4 x i8>) nounwind

define i32 @test__builtin_mips_cmpgu_lt_qb1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: cmpgu.lt.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = bitcast i32 %a1.coerce to <4 x i8>
  %2 = tail call i32 @llvm.mips.cmpgu.lt.qb(<4 x i8> %0, <4 x i8> %1)
  ret i32 %2
}

declare i32 @llvm.mips.cmpgu.lt.qb(<4 x i8>, <4 x i8>) nounwind

define i32 @test__builtin_mips_cmpgu_le_qb1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: cmpgu.le.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = bitcast i32 %a1.coerce to <4 x i8>
  %2 = tail call i32 @llvm.mips.cmpgu.le.qb(<4 x i8> %0, <4 x i8> %1)
  ret i32 %2
}

declare i32 @llvm.mips.cmpgu.le.qb(<4 x i8>, <4 x i8>) nounwind

define i32 @test__builtin_mips_cmp_eq_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: cmp.eq.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  tail call void @llvm.mips.cmp.eq.ph(<2 x i16> %0, <2 x i16> %1)
  %2 = tail call i32 @llvm.mips.rddsp(i32 31)
  ret i32 %2
}

declare void @llvm.mips.cmp.eq.ph(<2 x i16>, <2 x i16>) nounwind
declare i32 @llvm.mips.rddsp(i32) nounwind readonly

define i32 @test__builtin_mips_cmp_lt_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: cmp.lt.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  tail call void @llvm.mips.cmp.lt.ph(<2 x i16> %0, <2 x i16> %1)
  %2 = tail call i32 @llvm.mips.rddsp(i32 31)
  ret i32 %2
}

declare void @llvm.mips.cmp.lt.ph(<2 x i16>, <2 x i16>) nounwind

define i32 @test__builtin_mips_cmp_le_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: cmp.le.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  tail call void @llvm.mips.cmp.le.ph(<2 x i16> %0, <2 x i16> %1)
  %2 = tail call i32 @llvm.mips.rddsp(i32 31)
  ret i32 %2
}

declare void @llvm.mips.cmp.le.ph(<2 x i16>, <2 x i16>) nounwind

define { i32 } @test__builtin_mips_pick_qb1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind readonly {
entry:
; CHECK: pick.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = bitcast i32 %a1.coerce to <4 x i8>
  tail call void @llvm.mips.wrdsp(i32 %i0, i32 16)
  %2 = tail call <4 x i8> @llvm.mips.pick.qb(<4 x i8> %0, <4 x i8> %1)
  %3 = bitcast <4 x i8> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <4 x i8> @llvm.mips.pick.qb(<4 x i8>, <4 x i8>) nounwind readonly
declare void @llvm.mips.wrdsp(i32, i32) nounwind

define { i32 } @test__builtin_mips_pick_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind readonly {
entry:
; CHECK: pick.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  tail call void @llvm.mips.wrdsp(i32 %i0, i32 16)
  %2 = tail call <2 x i16> @llvm.mips.pick.ph(<2 x i16> %0, <2 x i16> %1)
  %3 = bitcast <2 x i16> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.pick.ph(<2 x i16>, <2 x i16>) nounwind readonly

define { i32 } @test__builtin_mips_packrl_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind readnone {
entry:
; CHECK: packrl.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  %2 = tail call <2 x i16> @llvm.mips.packrl.ph(<2 x i16> %0, <2 x i16> %1)
  %3 = bitcast <2 x i16> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.packrl.ph(<2 x i16>, <2 x i16>) nounwind readnone

define { i32 } @test__builtin_mips_shll_qb1(i32 %i0, i32 %a0.coerce) nounwind {
entry:
; CHECK: shll.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = tail call <4 x i8> @llvm.mips.shll.qb(<4 x i8> %0, i32 3)
  %2 = bitcast <4 x i8> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

declare <4 x i8> @llvm.mips.shll.qb(<4 x i8>, i32) nounwind

define { i32 } @test__builtin_mips_shll_ph1(i32 %i0, i32 %a0.coerce) nounwind {
entry:
; CHECK: shll.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = tail call <2 x i16> @llvm.mips.shll.ph(<2 x i16> %0, i32 7)
  %2 = bitcast <2 x i16> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.shll.ph(<2 x i16>, i32) nounwind

define { i32 } @test__builtin_mips_shll_s_ph1(i32 %i0, i32 %a0.coerce) nounwind {
entry:
; CHECK: shll_s.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = tail call <2 x i16> @llvm.mips.shll.s.ph(<2 x i16> %0, i32 7)
  %2 = bitcast <2 x i16> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.shll.s.ph(<2 x i16>, i32) nounwind

define i32 @test__builtin_mips_shll_s_w1(i32 %i0, i32 %a0) nounwind {
entry:
; CHECK: shll_s.w

  %0 = tail call i32 @llvm.mips.shll.s.w(i32 %a0, i32 15)
  ret i32 %0
}

declare i32 @llvm.mips.shll.s.w(i32, i32) nounwind

define i32 @test__builtin_mips_shll_s_w2(i32 %i0, i32 %a0, i32 %a1) nounwind {
entry:
; CHECK: shllv_s.w $

  %0 = tail call i32 @llvm.mips.shll.s.w(i32 %a0, i32 %a1)
  ret i32 %0
}

define { i32 } @test__builtin_mips_shra_ph1(i32 %i0, i32 %a0.coerce) nounwind readnone {
entry:
; CHECK: shra.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = tail call <2 x i16> @llvm.mips.shra.ph(<2 x i16> %0, i32 7)
  %2 = bitcast <2 x i16> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.shra.ph(<2 x i16>, i32) nounwind readnone

define { i32 } @test__builtin_mips_shra_ph2(i32 %i0, i32 %a0.coerce, i32 %a1) nounwind readnone {
entry:
; CHECK: shrav.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = tail call <2 x i16> @llvm.mips.shra.ph(<2 x i16> %0, i32 %a1)
  %2 = bitcast <2 x i16> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

define { i32 } @test__builtin_mips_shra_r_ph1(i32 %i0, i32 %a0.coerce) nounwind readnone {
entry:
; CHECK: shra_r.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = tail call <2 x i16> @llvm.mips.shra.r.ph(<2 x i16> %0, i32 7)
  %2 = bitcast <2 x i16> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.shra.r.ph(<2 x i16>, i32) nounwind readnone

define { i32 } @test__builtin_mips_shra_r_ph2(i32 %i0, i32 %a0.coerce, i32 %a1) nounwind readnone {
entry:
; CHECK: shrav_r.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = tail call <2 x i16> @llvm.mips.shra.r.ph(<2 x i16> %0, i32 %a1)
  %2 = bitcast <2 x i16> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

define { i32 } @test__builtin_mips_shra_qb1(i32 %i0, i32 %a0.coerce) nounwind readnone {
entry:
; CHECK: shra.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = tail call <4 x i8> @llvm.mips.shra.qb(<4 x i8> %0, i32 3)
  %2 = bitcast <4 x i8> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

declare <4 x i8> @llvm.mips.shra.qb(<4 x i8>, i32) nounwind readnone

define { i32 } @test__builtin_mips_shra_r_qb1(i32 %i0, i32 %a0.coerce) nounwind readnone {
entry:
; CHECK: shra_r.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = tail call <4 x i8> @llvm.mips.shra.r.qb(<4 x i8> %0, i32 3)
  %2 = bitcast <4 x i8> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

declare <4 x i8> @llvm.mips.shra.r.qb(<4 x i8>, i32) nounwind readnone

define { i32 } @test__builtin_mips_shra_qb2(i32 %i0, i32 %a0.coerce, i32 %a1) nounwind readnone {
entry:
; CHECK: shrav.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = tail call <4 x i8> @llvm.mips.shra.qb(<4 x i8> %0, i32 %a1)
  %2 = bitcast <4 x i8> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

define { i32 } @test__builtin_mips_shra_r_qb2(i32 %i0, i32 %a0.coerce, i32 %a1) nounwind readnone {
entry:
; CHECK: shrav_r.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = tail call <4 x i8> @llvm.mips.shra.r.qb(<4 x i8> %0, i32 %a1)
  %2 = bitcast <4 x i8> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

define i32 @test__builtin_mips_shra_r_w1(i32 %i0, i32 %a0) nounwind readnone {
entry:
; CHECK: shra_r.w $

  %0 = tail call i32 @llvm.mips.shra.r.w(i32 %a0, i32 15)
  ret i32 %0
}

declare i32 @llvm.mips.shra.r.w(i32, i32) nounwind readnone

define i32 @test__builtin_mips_shra_r_w2(i32 %i0, i32 %a0, i32 %a1) nounwind readnone {
entry:
; CHECK: shrav_r.w $

  %0 = tail call i32 @llvm.mips.shra.r.w(i32 %a0, i32 %a1)
  ret i32 %0
}

define { i32 } @test__builtin_mips_shrl_qb1(i32 %i0, i32 %a0.coerce) nounwind readnone {
entry:
; CHECK: shrl.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = tail call <4 x i8> @llvm.mips.shrl.qb(<4 x i8> %0, i32 3)
  %2 = bitcast <4 x i8> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

define { i32 } @test__builtin_mips_shrl_qb2(i32 %i0, i32 %a0.coerce, i32 %a1) nounwind readnone {
entry:
; CHECK: shrlv.qb $

  %0 = bitcast i32 %a0.coerce to <4 x i8>
  %1 = tail call <4 x i8> @llvm.mips.shrl.qb(<4 x i8> %0, i32 %a1)
  %2 = bitcast <4 x i8> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

declare <4 x i8> @llvm.mips.shrl.qb(<4 x i8>, i32) nounwind readnone

define { i32 } @test__builtin_mips_shrl_ph2(i32 %i0, i32 %a0.coerce, i32 %a1) nounwind readnone {
entry:
; CHECK: shrlv.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = tail call <2 x i16> @llvm.mips.shrl.ph(<2 x i16> %0, i32 %a1)
  %2 = bitcast <2 x i16> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}


define { i32 } @test__builtin_mips_mul_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: mul.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  %2 = tail call <2 x i16> @llvm.mips.mul.ph(<2 x i16> %0, <2 x i16> %1)
  %3 = bitcast <2 x i16> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.mul.ph(<2 x i16>, <2 x i16>) nounwind

define { i32 } @test__builtin_mips_mul_s_ph1(i32 %i0, i32 %a0.coerce, i32 %a1.coerce) nounwind {
entry:
; CHECK: mul_s.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = bitcast i32 %a1.coerce to <2 x i16>
  %2 = tail call <2 x i16> @llvm.mips.mul.s.ph(<2 x i16> %0, <2 x i16> %1)
  %3 = bitcast <2 x i16> %2 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %3, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.mul.s.ph(<2 x i16>, <2 x i16>) nounwind

define { i32 } @test__builtin_mips_shrl_ph1(i32 %i0, i32 %a0.coerce) nounwind readnone {
entry:
; CHECK: shrl.ph $

  %0 = bitcast i32 %a0.coerce to <2 x i16>
  %1 = tail call <2 x i16> @llvm.mips.shrl.ph(<2 x i16> %0, i32 7)
  %2 = bitcast <2 x i16> %1 to i32
  %.fca.0.insert = insertvalue { i32 } undef, i32 %2, 0
  ret { i32 } %.fca.0.insert
}

declare <2 x i16> @llvm.mips.shrl.ph(<2 x i16>, i32) nounwind readnone

define i32 @test__builtin_mips_bitrev1(i32 %i0, i32 %a0) nounwind readnone {
entry:
; CHECK: bitrev $

  %0 = tail call i32 @llvm.mips.bitrev(i32 %a0)
  ret i32 %0
}

declare i32 @llvm.mips.bitrev(i32) nounwind readnone
