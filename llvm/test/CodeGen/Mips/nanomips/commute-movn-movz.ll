;; RUN: llc -mtriple=nanomips < %s | FileCheck %s

;; CHECK-LABEL: max:
define dso_local i32 @max(i32* nocapture %0, i32* nocapture readnone %1, i32* nocapture readnone %2) local_unnamed_addr {
  br label %5

4:                                                ; preds = %5
  ret i32 undef


5:                                                ; preds = %3, %5
  %.018 = phi i32 [ 0, %3 ], [ %8, %5 ]
  %scevgep = getelementptr i32, i32* %0, i32 %.018

;; CHECK: lwxs
;; CHECK-NEXT: slti
;; CHECK-NEXT: movn

%6 = load i32, i32* %scevgep, align 4
  %7 = icmp sgt i32 %6, 10
  %. = select i1 %7, i32 %6, i32 10
  store i32 %., i32* %scevgep, align 4
  %8 = add nuw nsw i32 %.018, 1
  %exitcond.not = icmp eq i32 %8, 1024
  br i1 %exitcond.not, label %4, label %5
}
