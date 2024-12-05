;; RUN: llc -mtriple=nanomips -O1 < %s | FileCheck %s

define void @TEST(i16 %0) #0 {

switch i16 %0, label %return [
                              i16 94, label %5
                              i16 93, label %4
                              i16 92, label %3
                              i16 91, label %2
                              ]

;; bugz133: with 'return' as the next block after the switch jump, rather than
;; the redundant 'switch' destination, the compiler fails to insert a branch
;; to the 'case' block, allowing the entry block to fall through to the 'return'
;; in ALL cases.

return:
  ret void

;; CHECK: break
2:
  tail call void @llvm.ubsantrap(i8 66)
  unreachable

3:
  tail call void @llvm.ubsantrap(i8 66)
  unreachable

4:
  tail call void @llvm.ubsantrap(i8 66)
  unreachable

5:
  tail call void @llvm.ubsantrap(i8 66)
  unreachable
}

attributes #0 = { null_pointer_is_valid }
declare void @llvm.ubsantrap(i8 immarg)
