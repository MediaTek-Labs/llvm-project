; RUN: clang --target=nanomips %s -Os -mno-gpopt -S -o - | FileCheck %s
; ModuleID = 'reduced.bc'
target datalayout = "e-m:e-p:32:32-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "nanomips-unknown-unknown-elf"

@g_408 = external global ptr
@g_244 = external global ptr

define i32 @main(ptr %g_357) {
entry:
  call void @func_48()
  %call = call ptr @func_41()
  store ptr %call, ptr %g_357, align 4
  ret i32 0
}

define void @func_48() {
entry:
  %p_49.addr111111 = alloca [0 x [0 x [0 x [0 x [0 x [0 x i64]]]]]], i32 0, align 8
  store ptr %p_49.addr111111, ptr @g_244, align 4
  ret void
}

define i64 @safe_div_func_int64_t_s_s(i64 %si1, i64 %si2) {
entry:
  %si1.fr = freeze i64 %si1
  %cmp = icmp eq i64 %si2, 0
  %cmp1 = icmp eq i64 %si1.fr, 0
  %or.cond = or i1 %cmp, %cmp1
  br i1 %or.cond, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  store i8 0, ptr inttoptr (i32 1 to ptr), align 1
  br label %cond.end

cond.false:                                       ; preds = %entry
  %0 = or i64 %si1, 1
  %1 = icmp ult i64 %0, 3
  %div = zext i1 %1 to i64
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i64 [ 0, %cond.true ], [ %div, %cond.false ]
  ret i64 %cond
}

; CHECK: func_41:
; CHECK: save{{.*}}$ra
; CHECK: balc   safe_div_func_int64_t_s_s
; CHECK: restore{{.*}}$ra
; CHECK: .end   func_41
define internal ptr @func_41() {
entry:
  %0 = load volatile i16, ptr null, align 2
  %conv1 = zext i16 %0 to i64
  %call2 = call i64 @safe_div_func_int64_t_s_s(i64 0, i64 %conv1)
  %1 = load ptr, ptr @g_408, align 4
  ret ptr %1
}

define i16 @func_37(ptr %g_415) {
entry:
  %l_414111111 = alloca [0 x [0 x [0 x [0 x [0 x [0 x ptr]]]]]], i32 0, align 4
  store ptr %l_414111111, ptr %g_415, align 4
  %call = call ptr @func_41()
  ret i16 0
}
