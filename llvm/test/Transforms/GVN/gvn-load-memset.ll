; RUN: opt < %s -passes=gvn -S | FileCheck %s

@b = dso_local global [1024 x i16] zeroinitializer, align 4

declare void @llvm.memset.p0.i32(ptr nocapture writeonly, i8, i32, i1 immarg)


;; Dereference an array, can optimize
define signext i16 @rz(i8 %i) {
entry:
  %a = alloca [256 x i16], align 2
  call void @llvm.memset.p0.i32(ptr noundef nonnull align 2 dereferenceable(512) %a, i8 0, i32 512, i1 false)
  %idxprom = zext i8 %i to i32
  %arrayidx = getelementptr inbounds [256 x i16], ptr %a, i32 0, i32 %idxprom
  %0 = load i16, ptr %arrayidx, align 2
  ret i16 %0
}

;; CHECK-LABEL: define signext i16 @rz
;; CHECK: ret i16 0



;; Pointer is deferenced by an unknown index. We have no idea how
;; large the memory object referenced actually is, so cannot prove
;; that the memset covers the whole thing, so we cannot optimise.
define signext i16 @ix(ptr %ptr, i32  %i)  {
entry:
  call void @llvm.memset.p0.i32(ptr align 2 %ptr, i8 0, i32 2048, i1 false)
  %arrayidx1 = getelementptr inbounds i16, ptr %ptr, i32 %i
  %0 = load i16, ptr %arrayidx1, align 2
  ret i16 %0
}

;; CHECK-LABEL: define signext i16 @ix
;; CHECK-NOT: ret i16 0
;; CHECK: ret i16 %0

;; Global object, all of which is written by memset. The memset itself
;; will still be needed as the value of the array is global, but the
;; value read will be known to be zero.
define signext i16 @ig(i32 %i) {
entry:
  call void @llvm.memset.p0.i32(ptr align 2 @b, i8 0, i32 2048, i1 false)
  %arrayidx1 = getelementptr inbounds [1024 x i16], ptr @b, i32 0, i32 %i
  %0 = load i16, ptr %arrayidx1, align 2
  ret i16 %0
}

;; CHECK-LABEL: define signext i16 @ig
;; CHECK: ret i16 0



