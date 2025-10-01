;; RUN: llc -O2 -o - -mcpu=nanomips %s | FileCheck %s
target datalayout = "e-m:e-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S128"
target triple = "nanomips-unknown-unknown-elf"

;; Make sure we have one and only one BRSC
;; CHECK: lapc{{.*}}LJTI0_0
;; CHECK: brsc
;; CHECK-NOT: brsc


define i32 @switchy(i32 %x, ptr %ys, i1 %cmp, i32 %.sink22) {
entry:
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  store i32 0, ptr null, align 4
  br label %if.end

if.else:                                          ; preds = %entry
  %arrayidx3 = getelementptr i32, ptr %ys, i32 3
  %0 = load i32, ptr %arrayidx3, align 4
  br label %if.end
if.end:                                           ; preds = %if.else, %if.then
  switch i32 %x, label %if.end.unreachabledefault [
    i32 0, label %sw.bb
    i32 1, label %sw.epilog.sink.split
    i32 2, label %sw.epilog
    i32 3, label %sw.epilog.sink.split
  ]

sw.bb:                                            ; preds = %if.end
  store i32 0, ptr null, align 4
  br label %sw.epilog.sink.split

if.end.unreachabledefault:                        ; preds = %if.end
  unreachable

sw.epilog.sink.split:                             ; preds = %sw.bb, %if.end, %if.end
  %arrayidx18 = getelementptr i32, ptr %ys, i32 %.sink22
  %1 = load i32, ptr %arrayidx18, align 4
  br label %sw.epilog

sw.epilog:                                        ; preds = %sw.epilog.sink.split, %if.end
  ret i32 0
}
