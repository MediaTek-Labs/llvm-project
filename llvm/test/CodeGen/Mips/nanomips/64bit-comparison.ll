; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck %s

; CHECK: bltc	$a1, $a3, .LBB0_2
; CHECK: bnec	$a1, $a3, .LBB0_1
; CHECK: bltuc	$a0, $a2, .LBB0_2
; CHECK: .LBB0_1:
; CHECK: .LBB0_2:

; Function Attrs: optsize
define void @test(i64 signext %a, i64 signext %b) #0 {
entry:
  %cmp.not = icmp slt i64 %a, %b
  br i1 %cmp.not, label %if.else, label %if.then

if.then:                                          ; preds = %entry
  tail call void bitcast (void (...)* @aa to void ()*)()
  br label %if.end

if.else:                                          ; preds = %entry
  tail call void bitcast (void (...)* @bb to void ()*)() 
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret void
}

declare void @aa(...)

declare void @bb(...)

attributes #0 = { optsize }
