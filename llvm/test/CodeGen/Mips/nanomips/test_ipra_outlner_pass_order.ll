;; RUN: llc -enable-ipra -march nanomips %s -o /dev/null
;; target triple = "nanomips"

define internal fastcc void @callee3() unnamed_addr  {
entry:
  ret void
}

