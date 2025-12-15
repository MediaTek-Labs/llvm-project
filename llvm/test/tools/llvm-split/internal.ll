; RUN: llvm-split -o %t %s
; RUN: llvm-dis -o - %t0 | FileCheck --check-prefix=CHECK0 %s
; RUN: llvm-dis -o - %t1 | FileCheck --check-prefix=CHECK1 %s
; RUN: llvm-split -o %t %s -split-module-unique-internal-names
; RUN: ( llvm-dis -o - %t0 ; llvm-dis -o - %t1 ) | FileCheck --check-prefix=CHECK-UNIQUE %s

; CHECK0: define hidden void @foo()
; CHECK1: declare hidden void @foo()
; CHECK-UNIQUE-DAG: declare hidden void @foo.__uniq.{{[a-f0-9]*}}()
define internal void @foo() {
  call void @bar()
  ret void
}

; CHECK0: declare void @bar()
; CHECK1: define void @bar()
; CHECK-UNIQUE-DAG: define void @bar()
define void @bar() {
  call void @foo()
  ret void
}
