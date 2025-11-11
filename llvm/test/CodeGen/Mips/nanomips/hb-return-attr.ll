;; RUN: llc -o - -mcpu=nanomips %s | FileCheck %s

target datalayout = "e-m:e-p:32:32-i8:8:32-i16:16:32-i64:64-n32:64-S128"
target triple = "nanomips-unknown-unknown-elf"

define dso_local void @foo() #0 {
entry:
  ret void
; CHECK: jrc $ra
}
attributes #0 = { "target-features"="+nanomips" }

define dso_local void @bar() #1 {
entry:
  ret void
; CHECK: jrc.hb $ra
}

attributes #1 = { "target-features"="+nanomips" "use-hazard-barrier-return" }
