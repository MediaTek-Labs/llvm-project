// REQUIRES: mips-registered-target
// RUN: %clang_cc1 -triple nanomips-unknown-elf \
// RUN:            -target-feature +dsp -emit-llvm %s -o - \
// RUN:   | FileCheck %s
typedef int q31;
typedef int i32;
typedef unsigned int ui32;
typedef long long a64;

typedef signed char v4i8 __attribute__ ((vector_size(4)));
typedef signed char v4q7 __attribute__ ((vector_size(4)));
typedef short v2i16 __attribute__ ((vector_size(4)));
typedef short v2q15 __attribute__ ((vector_size(4)));

void foo() {
  v2q15 v2q15_r, v2q15_a, v2q15_b, v2q15_c;
  v2i16 v2i16_r, v2i16_a, v2i16_b, v2i16_c;
  v4q7 v4q7_r, v4q7_a, v4q7_b;
  v4i8 v4i8_r, v4i8_a, v4i8_b, v4i8_c;
  q31 q31_r, q31_a, q31_b, q31_c;
  i32 i32_r, i32_a, i32_b, i32_c;
  ui32 ui32_r, ui32_a, ui32_b, ui32_c;
  a64 a64_r, a64_a, a64_b;

  // MIPS DSP Rev 1

  v4i8_a = (v4i8) {1, 2, 3, 0xFF};
  v4i8_b = (v4i8) {2, 4, 6, 8};
  v4i8_r = __builtin_mips_addu_qb(v4i8_a, v4i8_b);
// CHECK: call <4 x i8> @llvm.mips.addu.qb
  v4i8_r = __builtin_mips_addu_s_qb(v4i8_a, v4i8_b);
// CHECK: call <4 x i8> @llvm.mips.addu.s.qb
  v4i8_r = __builtin_mips_subu_qb(v4i8_a, v4i8_b);
// CHECK: call <4 x i8> @llvm.mips.subu.qb
  v4i8_r = __builtin_mips_subu_s_qb(v4i8_a, v4i8_b);
// CHECK: call <4 x i8> @llvm.mips.subu.s.qb

  v2q15_a = (v2q15) {0x0000, 0x8000};
  v2q15_b = (v2q15) {0x8000, 0x8000};
  v2q15_r = __builtin_mips_addq_ph(v2q15_a, v2q15_b);
// CHECK: call <2 x i16> @llvm.mips.addq.ph
  v2q15_r = __builtin_mips_addq_s_ph(v2q15_a, v2q15_b);
// CHECK: call <2 x i16> @llvm.mips.addq.s.ph
  v2q15_r = __builtin_mips_subq_ph(v2q15_a, v2q15_b);
// CHECK: call <2 x i16> @llvm.mips.subq.ph
  v2q15_r = __builtin_mips_subq_s_ph(v2q15_a, v2q15_b);
// CHECK: call <2 x i16> @llvm.mips.subq.s.ph

  i32_a = 20;
  i32_b = 0x1402;
  i32_r = __builtin_mips_modsub(i32_a, i32_b);
// CHECK: call i32 @llvm.mips.modsub

  a64_a = 0x12345678;
  i32_b = 0x80000000;
  i32_c = 0x11112222;
  a64_r = __builtin_mips_madd(a64_a, i32_b, i32_c);
// CHECK: call i64 @llvm.mips.madd
  a64_a = 0x12345678;
  ui32_b = 0x80000000;
  ui32_c = 0x11112222;
  a64_r = __builtin_mips_maddu(a64_a, ui32_b, ui32_c);
// CHECK: call i64 @llvm.mips.maddu
  a64_a = 0x12345678;
  i32_b = 0x80000000;
  i32_c = 0x11112222;
  a64_r = __builtin_mips_msub(a64_a, i32_b, i32_c);
// CHECK: call i64 @llvm.mips.msub
  a64_a = 0x12345678;
  ui32_b = 0x80000000;
  ui32_c = 0x11112222;
  a64_r = __builtin_mips_msubu(a64_a, ui32_b, ui32_c);
// CHECK: call i64 @llvm.mips.msubu

  q31_a = 0x12345678;
  q31_b = 0x11112222;
  v2q15_r = __builtin_mips_precrq_ph_w(q31_a, q31_b);
// CHECK: call <2 x i16> @llvm.mips.precrq.ph.w

  v4i8_a = (v4i8) {1, 2, 3, 4};
  v4i8_r = __builtin_mips_shll_qb(v4i8_a, 2);
// CHECK: call <4 x i8> @llvm.mips.shll.qb
  v4i8_a = (v4i8) {128, 64, 32, 16};
  v4i8_r = __builtin_mips_shrl_qb(v4i8_a, 2);
// CHECK: call <4 x i8> @llvm.mips.shrl.qb
  v2q15_a = (v2q15) {0x0001, 0x8000};
  v2q15_r = __builtin_mips_shll_ph(v2q15_a, 2);
// CHECK: call <2 x i16> @llvm.mips.shll.ph
  v2q15_r = __builtin_mips_shll_s_ph(v2q15_a, 2);
// CHECK: call <2 x i16> @llvm.mips.shll.s.ph

  i32_a = 0x80000000;
  i32_b = 0x11112222;
  a64_r = __builtin_mips_mult(i32_a, i32_b);
// CHECK: call i64 @llvm.mips.mult
  ui32_a = 0x80000000;
  ui32_b = 0x11112222;
  a64_r = __builtin_mips_multu(ui32_a, ui32_b);
// CHECK: call i64 @llvm.mips.multu

  i32_a = 0x12345678;
  i32_b = 0x87654321;
  i32_r = __builtin_mips_append(i32_a, i32_b, 16);
// CHECK: call i32 @llvm.mips.append

  i32_r = __builtin_mips_cmpgu_eq_qb(v4i8_a, v4i8_b);
// CHECK: call i32 @llvm.mips.cmpgu.eq.qb
  i32_r = __builtin_mips_cmpgu_lt_qb(v4i8_a, v4i8_b);
// CHECK: call i32 @llvm.mips.cmpgu.lt.qb
  i32_r = __builtin_mips_cmpgu_le_qb(v4i8_a, v4i8_b);
// CHECK: call i32 @llvm.mips.cmpgu.le.qb
  v2q15_a = (v2q15) {0x1111, 0x1234};
  v2q15_b = (v2q15) {0x4444, 0x1234};
  __builtin_mips_cmp_lt_ph(v2q15_a, v2q15_b);
// CHECK: call void @llvm.mips.cmp.lt.ph
  __builtin_mips_cmp_le_ph(v2q15_a, v2q15_b);
// CHECK: call void @llvm.mips.cmp.le.ph

  v4i8_r = __builtin_mips_pick_qb(v4i8_a, v4i8_b);
// CHECK: call <4 x i8> @llvm.mips.pick.qb
  v2q15_a = (v2q15) {0x1111, 0x1234};
  v2q15_b = (v2q15) {0x4444, 0x1234};
  __builtin_mips_cmp_eq_ph(v2q15_a, v2q15_b);
// CHECK: call void @llvm.mips.cmp.eq.ph
  v2q15_r = __builtin_mips_pick_ph(v2q15_a, v2q15_b);
// CHECK: call <2 x i16> @llvm.mips.pick.ph

  a64_a = 0x1234567887654321LL;
  i32_b = 0x11112222;
  __builtin_mips_wrdsp(0, 1);
// CHECK: call void @llvm.mips.wrdsp
  a64_r = __builtin_mips_mthlip(a64_a, i32_b);
// CHECK: call i64 @llvm.mips.mthlip

  // MIPS DSP Rev 2

  v2i16_a = (v2i16) {0xffff, 0x2468};
  v2i16_b = (v2i16) {0x1234, 0x1111};
  v2i16_r = __builtin_mips_mul_ph(v2i16_a, v2i16_b);
// CHECK: call <2 x i16> @llvm.mips.mul.ph
  v2i16_a = (v2i16) {0x8000, 0x7fff};
  v2i16_b = (v2i16) {0x1234, 0x1111};
  v2i16_r = __builtin_mips_mul_s_ph(v2i16_a, v2i16_b);
// CHECK: call <2 x i16> @llvm.mips.mul.s.ph

  v2i16_a = (v2i16) {0x1357, 0x2468};
  v2i16_r = __builtin_mips_shrl_ph(v2i16_a, 4);
// CHECK: call <2 x i16> @llvm.mips.shrl.ph
  v2i16_a = (v2i16) {0x1357, 0x2468};
  i32_b = 8;
  v2i16_r = __builtin_mips_shrl_ph (v2i16_a, i32_b);
// CHECK: call <2 x i16> @llvm.mips.shrl.ph

//  a64_a = 0xFFFFF81230000000LL;
///  i32_r = __builtin_mips_extr_s_h(a64_a, 4);
// CHxxECK: call i32 @llvm.mips.extr.s.h
  a64_a = 0x8123456712345678LL;
  i32_r = __builtin_mips_extr_w(a64_a, 31);
// CHECK: call i32 @llvm.mips.extr.w
  i32_r = __builtin_mips_extr_rs_w(a64_a, 31);
// CHECK: call i32 @llvm.mips.extr.rs.w
  i32_r = __builtin_mips_extr_r_w(a64_a, 31);
// CHECK: call i32 @llvm.mips.extr.r.w
  a64_a = 0x1234567887654321LL;
  i32_r = __builtin_mips_extp(a64_a, 3);
// CHECK: call i32 @llvm.mips.extp
  a64_a = 0x123456789ABCDEF0LL;
  i32_r = __builtin_mips_extpdp(a64_a, 7);
// CHECK: call i32 @llvm.mips.extpdp

  i32_a = 0xFFFFFFFF;
  i32_b = 0x12345678;
  __builtin_mips_wrdsp((16<<7) + 4, 3);
// CHECK: call void @llvm.mips.wrdsp
  i32_r = __builtin_mips_insv(i32_a, i32_b);
// CHECK: call i32 @llvm.mips.insv
  i32_a = 0x1234;
  i32_r = __builtin_mips_bitrev(i32_a);
// CHECK: call i32 @llvm.mips.bitrev

  v2q15_a = (v2q15) {0x1111, 0x2222};
  v2q15_b = (v2q15) {0x3333, 0x4444};
  v2q15_r = __builtin_mips_packrl_ph(v2q15_a, v2q15_b);
// CHECK: call <2 x i16> @llvm.mips.packrl.ph

  __builtin_mips_wrdsp(2052, 3);
// CHECK: call void @llvm.mips.wrdsp
  i32_r = __builtin_mips_rddsp(3);
// CHECK: call i32 @llvm.mips.rddsp
  __builtin_mips_wrdsp((16<<7) + 4, 3);
// CHECK: call void @llvm.mips.wrdsp
  __builtin_mips_wrdsp(0, 1);
// CHECK: call void @llvm.mips.wrdsp

}

void test_eh_return_data_regno()
{
  volatile int res;
  res = __builtin_eh_return_data_regno(0);  // CHECK: store volatile i32 7
  res = __builtin_eh_return_data_regno(1);  // CHECK: store volatile i32 6
}
