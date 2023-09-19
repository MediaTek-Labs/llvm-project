; RUN: opt -passes=jump-threading -S < %s | FileCheck %s

; Check that an "unqualified" early operand in a phi node will not prevent
; backpropagation from a "qualified" later operand. Here "(un)qualified" means
; we cannot propagate the branch weight in updatePredecessorProfileMetadata,
; which more specifically, returns nullptr in GetPredOutEdge.

; In the first test, the phi node is in bb3 and the first operand is
; unqualified because it has multiple predecessors(diamond.l/diamond.r)

; CHECK-LABEL: test_with_diamond_structure
define void @test_with_diamond_structure(i32 %n, i32 %x) {
entry:
  %0 = icmp sgt i32 %n, 0
  br i1 %0, label %diamond.head, label %bb1

diamond.head:                                     ; preds = %entry
  %1 = icmp sgt i32 %x, 2
  br i1 %1, label %diamond.l, label %diamond.r

diamond.l:                                        ; preds = %diamond.head
  call void @aa()
  br label %diamond.end

diamond.r:                                        ; preds = %diamond.head
  call void @bb()
  br label %diamond.end

diamond.end:                                      ; preds = %diamond.l, %diamond,r
  %2 = phi i32 [ %n, %diamond.l ], [ %x, %diamond.r ]
  call void @cc(i32 %2)
  br label %bb3

bb1:                                              ; preds = %entry, %diamond.end
  %tmp = call i32 @a()
  %tmp1 = icmp eq i32 %tmp, 1
  br i1 %tmp1, label %bb3, label %bb2
; CHECK: br i1 %tmp1,{{.*}} !prof ![[PROF1:[0-9]+]]

bb2:                                              ; preds = %bb1
  %tmp3 = call i32 @b()
  %tmp4 = icmp ne i32 %tmp3, 1
  br label %bb3
; CHECK: br i1 %tmp4,{{.*}} !prof ![[PROF2:[0-9]+]]

bb3:                                              ; preds = %diamond.end, %bb1, %bb2
  %tmp6 = phi i1 [ false, %diamond.end ], [ false, %bb1 ], [ %tmp4, %bb2 ]
  br i1 %tmp6, label %bb4, label %bb5, !prof !0

bb4:                                              ; preds = %bb3
  call void @bar()
  br label %bb5

bb5:                                              ; preds = %bb3, %bb4
  ret void
}


; In the second test, the phi node is in bb5 and the first operand is
; unqualified because its predecessor's terminator is not a BranchInst

; CHECK-LABEL: test_with_unreachable_loop
define void @test_with_unreachable_loop(i32 %n, i32 %x) {
bb:
  %tmp = call i32 @a()
  %tmp1 = icmp eq i32 %tmp, 1
  br i1 %tmp1, label %bb5, label %bb2
; CHECK: br i1 %tmp1,{{.*}} !prof ![[PROF1:[0-9]+]]

bb2:                                              ; preds = %bb
  %tmp3 = call i32 @b()
  %tmp4 = icmp ne i32 %tmp3, 1
  br label %bb5
; CHECK: br i1 %tmp4,{{.*}} !prof ![[PROF2:[0-9]+]]

; unreachable single bb loop.
bb3:                                              ; preds = %bb3
  %tmp7 = icmp ne i32 %tmp, 1
  switch i1 %tmp7, label %bb3 [
  i1 0, label %bb5
  i1 1, label %bb8
  ]

bb5:                                              ; preds = %bb5, %bb, %bb2
  %tmp6 = phi i1 [ false, %bb3 ], [ false, %bb ], [ %tmp4, %bb2 ]
  br i1 %tmp6, label %bb8, label %bb7, !prof !0

bb7:                                              ; preds = %bb5
  call void @bar()
  br label %bb8

bb8:                                              ; preds = %bb7, %bb5
  ret void
}

declare void @bar()

declare void @aa()

declare void @bb()

declare void @cc(i32)

declare i32 @a()

declare i32 @b()

!0 = !{!"branch_weights", i32 2146410443, i32 1073205}
;CHECK: ![[PROF1]] = !{!"branch_weights", i32 1073205, i32 2146410443}
;CHECK: ![[PROF2]] = !{!"branch_weights", i32 2146410443, i32 1073205}
