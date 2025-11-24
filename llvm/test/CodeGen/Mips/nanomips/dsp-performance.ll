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

; CHECK-LABEL: madd_loop:
; CHECK: mult $ac{{[0-3]}}, $zero, $zero
; CHECK: This Inner Loop Header
; CHECK-DAG: madd $
; CHECK-NOT: mfhi $
; CHECK-NOT: mflo $
; CHECK: bnezc $
; CHECK-DAG: mflo $
; CHECK-DAG: mfhi $

; Function Attrs: nofree nosync nounwind memory(argmem: read)
define dso_local i64 @madd_loop(ptr nocapture noundef readonly %a, ptr nocapture noundef readonly %b, i32 noundef signext %n) local_unnamed_addr #2 {
entry:
  %cmp6 = icmp sgt i32 %n, 0
  br i1 %cmp6, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.body, %entry
  %acc.0.lcssa = phi i64 [ 0, %entry ], [ %2, %for.body ]
  ret i64 %acc.0.lcssa

for.body:                                         ; preds = %entry, %for.body
  %i.08 = phi i32 [ %inc, %for.body ], [ 0, %entry ]
  %acc.07 = phi i64 [ %2, %for.body ], [ 0, %entry ]
  %arrayidx = getelementptr inbounds i32, ptr %a, i32 %i.08
  %0 = load i32, ptr %arrayidx, align 4
  %arrayidx1 = getelementptr inbounds i32, ptr %b, i32 %i.08
  %1 = load i32, ptr %arrayidx1, align 4
  %2 = tail call i64 @llvm.mips.madd(i64 %acc.07, i32 %0, i32 %1)
  %inc = add nuw nsw i32 %i.08, 1
  %exitcond.not = icmp eq i32 %inc, %n
  br i1 %exitcond.not, label %for.cond.cleanup, label %for.body
}

; similar to mad_loop but order of usages of madd result
; prevented optimization in older code
; CHECK-LABEL: madd_loop_variant:
; CHECK: mult $ac{{[0-3]}}, $zero, $zero
; CHECK: This Inner Loop Header
; CHECK-DAG: madd $
; CHECK-NOT: mfhi $
; CHECK-NOT: mflo $
; CHECK: bltc $
; CHECK-DAG: mflo $
; CHECK-DAG: mfhi $

define dso_local i32 @madd_loop_variant(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1, i32 noundef signext %2, ptr nocapture noundef readnone %3, ptr nocapture noundef writeonly %4) local_unnamed_addr #0 {
  %6 = icmp sgt i32 %2, 0
  br i1 %6, label %7, label %10

7:                                                ; preds = %5
  %8 = getelementptr i8, ptr %0, i32 12
  %9 = getelementptr i8, ptr %1, i32 12
  br label %15

10:                                               ; preds = %15, %5
  %11 = phi i64 [ 0, %5 ], [ %23, %15 ]
  %12 = trunc i64 %11 to i32
  %13 = lshr i64 %11, 32
  %14 = trunc i64 %13 to i32
  store i32 %14, ptr %4, align 4
  ret i32 %12

15:                                               ; preds = %7, %15
  %16 = phi i64 [ %23, %15 ], [ 0, %7 ]
  %17 = phi i32 [ %24, %15 ], [ 0, %7 ]
  %18 = shl i32 %17, 2
  %19 = getelementptr i8, ptr %8, i32 %18
  %20 = load i32, ptr %19, align 4
  %21 = getelementptr i8, ptr %9, i32 %18
  %22 = load i32, ptr %21, align 4
  %23 = tail call i64 @llvm.mips.madd(i64 %16, i32 %20, i32 %22)
  %24 = add nuw i32 %17, 4
  %25 = icmp slt i32 %24, %2
  br i1 %25, label %15, label %10
}

; CHECK-LABEL: acc_phi:
; CHECK-DAG: mtlo $
; CHECK-DAG: mthi $
; CHECK: madd
; CHECK-NOT: mtlo
; CHECK-NOT: mthi
; CHECK-NOT: mflo
; CHECK-NOT: mfhi
; CHECK: maddu
; CHECK-DAG: mflo $
; CHECK-DAG: mfhi $

; Function Attrs: nofree nosync nounwind memory(none)
define dso_local i64 @acc_phi(i32 noundef signext %0, i32 noundef signext %1, i64 noundef signext %2, i32 noundef signext %3) local_unnamed_addr #0 {
  %5 = icmp eq i32 %3, 0
  br i1 %5, label %8, label %6

6:                                                ; preds = %4
  %7 = tail call i64 @llvm.mips.madd(i64 %2, i32 %0, i32 %1)
  br label %10

8:                                                ; preds = %4
  %9 = tail call i64 @llvm.mips.msub(i64 %2, i32 %0, i32 %1)
  br label %10

10:                                               ; preds = %8, %6
  %11 = phi i64 [ %7, %6 ], [ %9, %8 ]
  %12 = tail call i64 @llvm.mips.maddu(i64 %11, i32 %0, i32 %1)
  ret i64 %12
}

; regression tests
; TODO: optimize compiler to hoist and merge the ACC composes
define i32 @fan_out_phi(ptr %0, i32 %1, i1 %2) {
  call void @llvm.memcpy.p0.p0.i32(ptr %0, ptr null, i32 96, i1 false)
  br i1 %2, label %4, label %5

4:                                                ; preds = %3
  store i8 0, ptr %0, align 1
  br label %5

5:                                                ; preds = %4, %3
  %6 = phi i64 [ 1, %4 ], [ 0, %3 ]
  %.not = icmp eq i32 %1, 0
  br i1 %.not, label %7, label %9

common.ret:                                       ; preds = %9, %7
  ret i32 0

7:                                                ; preds = %5
  %8 = call i64 @llvm.mips.mthlip(i64 %6, i32 1)
  br label %common.ret

9:                                                ; preds = %5
  %10 = call i64 @llvm.mips.mthlip(i64 %6, i32 0)
  br label %common.ret
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i32(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i32, i1 immarg)

define i32 @phi_opnds_in_multiple_bbs(i1 %cond, ptr %0, i1 %cmp.i5066, i1 %cmp.i5082) {
entry:
  br i1 %cond, label %for.body2003, label %common.ret

common.ret:                                       ; preds = %safe_rshift_func_uint64_t_u_u.exit, %entry
  ret i32 0

for.body2003:                                 ; preds = %safe_rshift_func_uint64_t_u_u.exit, %entry
  br i1 %cmp.i5066, label %cond.true.i5067, label %safe_rshift_func_uint64_t_u_u.exit

cond.true.i5067:                              ; preds = %for.body2003
  store i8 0, ptr %0, align 1
  br label %safe_rshift_func_uint64_t_u_u.exit

safe_rshift_func_uint64_t_u_u.exit:           ; preds = %cond.true.i5067, %for.body2003
  %cond.i5071 = phi i64 [ 1, %cond.true.i5067 ], [ 0, %for.body2003 ]
  %1 = call i64 @llvm.mips.dpsq.sa.l.w(i64 %cond.i5071, i32 0, i32 0)
  br i1 %cmp.i5082, label %common.ret, label %for.body2003
}

; Function Attrs: nounwind
declare i64 @llvm.mips.dpsq.sa.l.w(i64, i32, i32)


; this case was degrading by triggering phi conversion
; CHECK-LABEL: dont_optimize:
; CHECK: mtlo
; CHECK-NEXT: mthi
; CHECK-NEXT: mthlip
; CHECK-NEXT: mfhi
; CHECK-NEXT: mflo

@crc32_context = internal global i32 -1
@.str.50 = private constant [15 x i8] c"checksum = %X\0A\00"
define i32 @dont_optimize(i32 %0, i32 %1, i32 %2, i1 %3, ptr %g_388) {
  br i1 %3, label %7, label %5

5:                                                ; preds = %4
  %6 = tail call i64 @llvm.mips.mthlip(i64 1, i32 0)
  br label %7

7:                                                ; preds = %5, %4
  %8 = phi i64 [ %6, %5 ], [ 0, %4 ]
  store i64 %8, ptr %g_388, align 8
  %9 = icmp eq i64 %8, 0
  %10 = zext i1 %9 to i64
  tail call void @transparent_crc(i64 %10)
  %11 = load i32, ptr @crc32_context, align 4
  %12 = xor i32 %11, 1
  %13 = tail call i32 (ptr, ...) @printf(ptr @.str.50, i32 %12)
  ret i32 0
}

declare void @transparent_crc(i64)
declare i32 @printf(ptr, ...)
declare void @crc32_byte(i8)
