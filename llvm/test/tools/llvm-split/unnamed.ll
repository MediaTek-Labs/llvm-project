; RUN: llvm-split -o %t %s
; RUN: llvm-dis -o - %t0 | FileCheck --check-prefix=CHECK0 %s
; RUN: llvm-dis -o - %t1 | FileCheck --check-prefix=CHECK1 %s
; RUN: llvm-split -o %t %s -split-module-unique-internal-names
; RUN: ( llvm-dis -o - %t0 ; llvm-dis -o - %t1 ) | \
; RUN: FileCheck --check-prefix=CHECK-UNIQUE %s

; CHECK0: declare hidden void @__llvmsplit_unnamed()
; CHECK1: define hidden void @__llvmsplit_unnamed()
; CHECK-UNIQUE-DAG: define hidden void @__llvmsplit_unnamed.__uniq.{{[a-f0-9]*}}()
define internal void @0() {
  ; CHECK1: call void @foo()
  call void @foo()
  ret void
}

; CHECK0: declare hidden void @__llvmsplit_unnamed.1()
; CHECK1: define hidden void @__llvmsplit_unnamed.1()
; CHECK-UNIQUE-DAG: define hidden void @__llvmsplit_unnamed.__uniq.{{[a-f0-9]*}}.1()
define internal void @1() {
  ; CHECK1: call void @foo()
  ; CHECK1: call void @foo()
  call void @foo()
  call void @foo()
  ret void
}

; CHECK0: define void @foo()
; CHECK1: declare void @foo()
; CHECK-UNIQUE-DAG: declare void @foo()
define void @foo() {
  ; CHECK0: call void @__llvmsplit_unnamed.1()
  ; CHECK0: call void @__llvmsplit_unnamed()
  call void @1()
  call void @0()
  ret void
}
