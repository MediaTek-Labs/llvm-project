; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck --check-prefix=CHECK-ALL --check-prefix=CHECK %s
; RUN: llc -mtriple=nanomips -asm-show-inst -mattr=+fix-hw110880 -verify-machineinstrs < %s | FileCheck --check-prefix=CHECK-ALL --check-prefix=CHECK-FIX %s

define i32 @foo0() nounwind readnone {
; CHECK-LABEL: foo0
entry:
; CHECK-ALL: li $a0, 12345
; CHECK-ALL: Li_NM
  ret i32 12345
}

define i32 @foo1() nounwind readnone {
; CHECK-LABEL: foo1
entry:
; CHECK-ALL: li $a0, -2147483648
; CHECK-ALL: Li_NM
  ret i32 -2147483648
}

define i32 @foo2() nounwind readnone {
; CHECK-LABEL: foo2
entry:
; CHECK-ALL: li $a0, 2147483647
; CHECK-ALL: Li_NM
  ret i32 2147483647
}

define i32 @foo3() nounwind readnone {
; CHECK-LABEL: foo3
entry:
; CHECK-ALL: li $a0, 2147479552
; CHECK-ALL: Li_NM
  ret i32 2147479552
}

define i32 @foo4() nounwind readnone {
; CHECK-LABEL: foo4
entry:
; CHECK: li $a0, 1375806708
; CHECK-FIX: li $a0, .Lzero+0x520124f4
; CHECK-ALL: Li_NM
  ret i32 1375806708
}
