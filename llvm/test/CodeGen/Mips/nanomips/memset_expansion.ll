; RUN: sed 's/SIZE/21/g' %s | llc -mtriple=nanomips | FileCheck %s --check-prefix=CALL
; RUN: sed 's/SIZE/20/g' %s | llc -mtriple=nanomips | FileCheck %s

@str = dso_local local_unnamed_addr global [44 x i8] c"almost every programmer should know memset!\00", align 4
; CHECK-LABEL: call_memset:
define dso_local void @call_memset() local_unnamed_addr #0 {
; CALL: bc memset
; CHECK-DAG sw [[VAL:\$[a-z0-9]+]], [0-9]+([[PTR:\$[a-z0-9]+]])
; CHECK-DAG sw [[VAL]], [0-9]+([[PTR]])
; CHECK-DAG sw [[VAL]], [0-9]+([[PTR]])
; CHECK-DAG sw [[VAL]], [0-9]+([[PTR]])
; CHECK-DAG sw [[VAL]], [0-9]+([[PTR]])
  tail call void @llvm.memset.p0.i32(ptr noundef nonnull align 4 dereferenceable(SIZE) @str, i8 45, i32 SIZE, i1 false)
  ret void
}
; CHECK-LABEL: .end call_memset
declare void @llvm.memset.p0.i32(ptr nocapture writeonly, i8, i32, i1 immarg)
attributes #0 = { optsize }
