; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck --check-prefix=CHECK-ALL --check-prefix=CHECK %s
; RUN: llc -mtriple=nanomips -asm-show-inst -mattr=+fix-hw110880 -verify-machineinstrs < %s | FileCheck --check-prefix=CHECK-ALL --check-prefix=CHECK-FIX %s
define i32 @test_addiu0(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, 1
  %added = add i32 %a, 1
  ret i32 %added
}

define i32 @test_addiu1(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, 65535
  %added = add i32 %a, 65535
  ret i32 %added
}

define i32 @test_addiu2(i32 %a) {
; CHECK-NOT: addiu $a0, $a0, 65536
  %added = add i32 %a, 65536
  ret i32 %added
}

define i32 @test_addiu3(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, -2048
  %added = add i32 %a, -2048
  ret i32 %added
}

define i32 @test_addiu4(i32 %a) {
; CHECK-NOT: addiu $a0, $a0, -2049
  %added = add i32 %a, -2049
  ret i32 %added
}

define i32 @test_addiu5(i32 %a) {
; CHECK-ALL: addiu[48] $a0, $a0, 2147483647
  %added = add i32 %a, 2147483647
  ret i32 %added
}

define i32 @test_addiu6(i32 %a) {
; CHECK-ALL: addiu[48] $a0, $a0, -2147483648
  %added = add i32 %a, -2147483648
  ret i32 %added
}

define i32 @test_addiu7(i32 %a, i32 %b) {
; CHECK-ALL: move $a0, $a1
; CHECK-ALL: addiu[48] $a0, $a0, -2049
  %added = add i32 %b, -2049
  ret i32 %added
}

define i32 @test_addiu8(i32 %a) {
; CHECK-ALL: addiu[48] $a0, $a0, -2147483648
  %added = add i32 %a, 2147483648
  ret i32 %added
}

define i32 @test_addiu9(i32 %a) {
; CHECK: addiu[48] $a0, $a0, 1375806708
; CHECK-FIX: addiu[48] $a0, $a0, .Lzero+0x520124f4
  %added = add i32 %a, 1375806708
  ret i32 %added
}