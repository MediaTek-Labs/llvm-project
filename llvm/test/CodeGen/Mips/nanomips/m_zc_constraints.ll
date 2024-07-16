; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck %s

define void @test(i8* %p) {
entry:
; Don't allow any offsets when 'm'.
  tail call void asm sideeffect "ll $$zero, $0", "*m,~{$1}"(ptr elementtype (i8) %p)
; CHECK: ll $zero, 0($a0)
  %add.ptr = getelementptr inbounds i8, i8* %p, i32 8
  tail call void asm sideeffect "ll $$zero, $0", "*m,~{$1}"(ptr nonnull elementtype (i8) %add.ptr)
; CHECK: addiu  $a1, $a0, 8
; CHECK: ll $zero, 0($a1)

; Allow 9bit 4byte aligned offsets when 'ZC'.
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(ptr elementtype (i8) %p)
; CHECK: ll $zero, 0($a0)
  %add.ptr5 = getelementptr inbounds i8, i8* %p, i32 256
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(ptr nonnull elementtype (i8) %add.ptr5)
; CHECK: addiu  $a1, $a0, 256
; CHECK: ll $zero, 0($a1)
  %add.ptr6 = getelementptr inbounds i8, i8* %p, i32 -257
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(ptr nonnull elementtype (i8) %add.ptr6)
; CHECK: addiu  $a1, $a0, -257
; CHECK: ll $zero, 0($a1)
  %add.ptr7 = getelementptr inbounds i8, i8* %p, i32 252
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(ptr nonnull elementtype (i8) %add.ptr7)
; CHECK: ll $zero, 252($a0)
  %add.ptr8 = getelementptr inbounds i8, i8* %p, i32 -252
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(ptr nonnull elementtype (i8) %add.ptr8)
; CHECK: ll $zero, -252($a0)
  %add.ptr9 = getelementptr inbounds i8, i8* %p, i32 9
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(ptr nonnull elementtype (i8) %add.ptr9)
; CHECK: addiu  $a0, $a0, 9
; CHECK: ll $zero, 0($a0)
  ret void
}
