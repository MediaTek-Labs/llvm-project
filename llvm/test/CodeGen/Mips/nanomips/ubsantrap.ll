; RUN: llc -mtriple=nanomips %s -nmips-ubsantrap-sankind-codes -o - | FileCheck %s -check-prefixes=CHECK,BASE0
; RUN: llc -mtriple=nanomips %s -nmips-ubsantrap-sankind-codes -nmips-ubsantrap-code-base=100 -o - | FileCheck %s -check-prefixes=CHECK,BASE100

define void @test_ubsantrap() {
; CHECK-LABEL: test_ubsantrap
; BASE0: break 0xc
; BASE100: break 0x70

  call void @llvm.ubsantrap(i8 12)
  ret void
}

declare void @llvm.ubsantrap(i8)

