; RUN: llc -mcpu=nanomips -mattr=+dsp  < %s
define i32 @phi_partly_beneficial(i1 %0) {
  br label %2

2:                                                ; preds = %8, %1
  %3 = phi i32 [ 0, %1 ], [ %10, %8 ]
  %4 = phi i64 [ 0, %1 ], [ %9, %8 ]
  store i32 %3, ptr null, align 4
  br label %6

5:                                                ; preds = %6
  br i1 %0, label %8, label %11

6:                                                ; preds = %6, %2
  %7 = call i64 @llvm.mips.maq.sa.w.phr(i64 %4, <2 x i16> zeroinitializer, <2 x i16> zeroinitializer)
  br i1 %0, label %5, label %6

8:                                                ; preds = %11, %5
  %9 = phi i64 [ 0, %11 ], [ 1, %5 ]
  %10 = load i32, ptr null, align 4
  br label %2

11:                                               ; preds = %5
  br label %8
}

; Function Attrs: nounwind
declare i64 @llvm.mips.maq.sa.w.phr(i64, <2 x i16>, <2 x i16>)

define i32 @extract_hi_only() {
  %1 = call i64 @llvm.mips.mthlip(i64 0, i32 0)
  br label %2

2:                                                ; preds = %0
  %3 = icmp slt i64 %1, 0
  br i1 %3, label %4, label %5

4:                                                ; preds = %2
  ret i32 0

5:                                                ; preds = %2
  ret i32 0
}

define i32 @reuse_new_phi() {
  %1 = call fastcc i8 @func_409(i1 false)
  ret i32 0
}

define internal fastcc i8 @func_409(i1 %0) {
  br i1 %0, label %4, label %2

2:                                                ; preds = %1
  %3 = tail call i64 @llvm.mips.mthlip(i64 0, i32 0)
  br label %6

4:                                                ; preds = %1
  %5 = tail call i64 @llvm.mips.mthlip(i64 0, i32 0)
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i64 [ %5, %4 ], [ %3, %2 ]
  %8 = icmp sgt i64 %7, 0
  %9 = zext i1 %8 to i32
  store i32 %9, ptr null, align 4
  ret i8 0
}

; Function Attrs: nounwind
declare i64 @llvm.mips.mthlip(i64, i32)

; case in which high part requires compose but not low part
define i32 @main(i1 %0, i64 %1) {
  %3 = call i64 @llvm.mips.msubu(i64 0, i32 0, i32 0)
  %4 = or i64 %3, 1
  br i1 %0, label %7, label %5

5:                                                ; preds = %2
  %6 = srem i64 1, %1
  br label %7

7:                                                ; preds = %5, %2
  %8 = phi i64 [ %6, %5 ], [ %4, %2 ]
  %9 = call i32 @llvm.mips.extr.s.h(i64 %8, i32 0)
  ret i32 0
}

; Function Attrs: nounwind
declare i32 @llvm.mips.extr.s.h(i64, i32) #0

; Function Attrs: nounwind memory(none)
declare i64 @llvm.mips.msubu(i64, i32, i32) #1
