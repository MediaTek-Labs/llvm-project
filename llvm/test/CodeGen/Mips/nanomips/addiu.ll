; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck --check-prefix=CHECK-ALL --check-prefix=CHECK %s
; RUN: llc -mtriple=nanomips -asm-show-inst -mattr=+fix-hw110880 -verify-machineinstrs < %s | FileCheck --check-prefix=CHECK-ALL --check-prefix=CHECK-FIX %s
define i32 @test_addiu0(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, 1 # <MCInst #{{.*}} ADDIU_NM
  %added = add i32 %a, 1
  ret i32 %added
}

define i32 @test_addiu1(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, 65535 # <MCInst #{{.*}} ADDIU_NM
  %added = add i32 %a, 65535
  ret i32 %added
}

define i32 @test_addiu2(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, 65536 # <MCInst #{{.*}} ADDIU48_NM
  %added = add i32 %a, 65536
  ret i32 %added
}

define i32 @test_addiu3(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, -1 # <MCInst #{{.*}} ADDIUNEG_NM
  %added = add i32 %a, -1
  ret i32 %added
}

define i32 @test_addiu4(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, -4095 # <MCInst #{{.*}} ADDIUNEG_NM
  %added = add i32 %a, -4095
  ret i32 %added
}

define i32 @test_addiu5(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, 2147483647 # <MCInst #{{.*}} ADDIU48_NM
  %added = add i32 %a, 2147483647
  ret i32 %added
}

define i32 @test_addiu6(i32 %a) {
; CHECK-ALL: addiu $a0, $a0, -2147483648 # <MCInst #{{.*}} ADDIU48_NM
  %added = add i32 %a, -2147483648
  ret i32 %added
}

define i32 @test_addiu7(i32 %a, i32 %b) {
; CHECK-ALL: addiu $a0, $a0, -4096 # <MCInst #{{.*}} ADDIU48_NM
  %added = add i32 %b, -4096
  ret i32 %added
}

define i32 @test_addiu8(i32 %a) {
; CHECK: addiu $a0, $a0, 1375806708
; CHECK-FIX: addiu $a0, $a0, 3735936685
  %added = add i32 %a, 1375806708
  ret i32 %added
}
