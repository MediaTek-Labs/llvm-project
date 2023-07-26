; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck %s

; Check if stack realignment is done using INS instruction 
; if function needs it.
declare void @callee(i32*)
define void @test() {
entry:
  ; CHECK: ins $sp, $zero, 0, 6
  %0 = alloca i32, align 64
  call void @callee(i32 *%0)
  ret void
}

