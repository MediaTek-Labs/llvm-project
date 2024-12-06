; RUN: llc -mcpu=nanomips -mattr=+dsp -stop-after=phi-node-elimination -o - < %s | FileCheck %s
; CHECK-NOT: PHI

@g_422 = external dso_local global i64
@g_244 = external dso_local global i1
@g = external dso_local global [2 x i8]

define i32 @main() {
  %1 = alloca i32, align 4
  call void @llvm.lifetime.start.p0(i64 4, ptr nonnull %1)
  store volatile i32 -371570654, ptr %1, align 4
  %2 = load i64, ptr @g_422, align 8
  %3 = load i1, ptr @g_244, align 4
  br label %4

4:                                                ; preds = %40, %0
  %5 = phi i64 [ -646177478195389849, %0 ], [ %38, %40 ]
  %6 = phi i32 [ 1, %0 ], [ 0, %40 ]
  %7 = phi i64 [ %2, %0 ], [ %35, %40 ]
  %8 = phi i1 [ %3, %0 ], [ true, %40 ]
  br label %9

9:                                                ; preds = %9, %4
  %10 = phi i64 [ %5, %4 ], [ %38, %9 ]
  %11 = phi i1 [ true, %4 ], [ false, %9 ]
  %12 = phi i32 [ 0, %4 ], [ 1, %9 ]
  %13 = phi i64 [ %7, %4 ], [ %35, %9 ]
  %14 = phi i1 [ %8, %4 ], [ true, %9 ]
  %15 = getelementptr inbounds [2 x i8], ptr @g, i32 0, i32 %12
  %16 = sub i64 0, %10
  %17 = add i64 %13, 1
  %18 = icmp uge i64 %17, %16
  %19 = zext i1 %18 to i64
  %20 = tail call i64 @llvm.mips.dpsx.w.ph(i64 %19, <2 x i16> zeroinitializer, <2 x i16> zeroinitializer)
  %21 = load volatile i32, ptr %1, align 4
  %22 = select i1 %14, i32 -3, i32 0
  %23 = bitcast i32 %22 to <2 x i16>
  %24 = or i8 0, 0
  %25 = sub i64 0, %20
  %26 = add i64 %13, 2
  %27 = icmp uge i64 %26, %25
  %28 = zext i1 %27 to i64
  %29 = tail call i64 @llvm.mips.dpsx.w.ph(i64 %28, <2 x i16> zeroinitializer, <2 x i16> zeroinitializer)
  %30 = load volatile i32, ptr %1, align 4
  %31 = tail call i64 @llvm.mips.dpax.w.ph(i64 0, <2 x i16> <i16 -3, i16 -1>, <2 x i16> <i16 -3, i16 -1>)
  %32 = trunc i64 %31 to i8
  %33 = or i8 %24, %32
  %34 = sub i64 0, %29
  %35 = add i64 %13, 3
  store i64 %35, ptr @g_422, align 8
  %36 = icmp uge i64 %35, %34
  %37 = zext i1 %36 to i64
  %38 = tail call i64 @llvm.mips.dpsx.w.ph(i64 %37, <2 x i16> zeroinitializer, <2 x i16> zeroinitializer)
  %39 = load volatile i32, ptr %1, align 4
  store i8 %33, ptr %15, align 1
  store i1 true, ptr @g_244, align 4
  br i1 %11, label %9, label %40

40:                                               ; preds = %9
  br i1 false, label %41, label %4

41:                                               ; preds = %40
  ret i32 0
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #0

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #0

; Function Attrs: nounwind memory(none)
declare i64 @llvm.mips.dpsx.w.ph(i64, <2 x i16>, <2 x i16>) #1

; Function Attrs: nounwind memory(none)
declare i64 @llvm.mips.dpax.w.ph(i64, <2 x i16>, <2 x i16>) #1

; uselistorder directives
uselistorder ptr @llvm.mips.dpsx.w.ph, { 2, 1, 0 }

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #1 = { nounwind memory(none) }
