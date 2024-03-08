; RUN: llc -mtriple=nanomips -verify-machineinstrs < %s | FileCheck %s

@foo = external dso_local global i32, align 4
@bar = external dso_local global i32, align 4

define dso_local i32 @test() local_unnamed_addr {
entry:
; CHECK: lapc.b  $a0, bar
; CHECK: addiu   $a0, $a0, foo
  %0 = load i32, i32* inttoptr (i32 add (i32 ptrtoint (i32* @foo to i32), i32 ptrtoint (i32* @bar to i32)) to i32*), align 4
  ret i32 %0
}
