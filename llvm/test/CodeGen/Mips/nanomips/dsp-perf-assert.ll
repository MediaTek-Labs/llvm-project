; RUN: llc -mcpu=nanomips -mattr=+dsp  < %s
define i32 @foo() {
entry:
  %0 = call i32 asm "# dummy", "=d"()
  ret i32 %0
}
