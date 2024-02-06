; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck %s

define i32 @foo0() nounwind readnone {
; CHECK-LABEL: foo0
entry:
; CHECK: li $a0, 12345
; CHECK: ADDIU_NM
  ret i32 12345
}

define i32 @foo1() nounwind readnone {
; CHECK-LABEL: foo1
entry:
; CHECK: li $a0, -2147483647
; CHECK: LI48_NM
  ret i32 -2147483647
}

define i32 @foo2() nounwind readnone {
; CHECK-LABEL: foo2
entry:
; CHECK: li $a0, 2147483647
; CHECK: LI48_NM
  ret i32 2147483647
}

define i32 @foo4() nounwind readnone {
; CHECK-LABEL: foo4
entry:
; CHECK: li $a0, -1
; CHECK: LI16_NM
  ret i32 -1
}

define i32 @foo5() nounwind readnone {
; CHECK-LABEL: foo5
entry:
; CHECK: li $a0, -4095
; CHECK: ADDIUNEG_NM
  ret i32 -4095
}
