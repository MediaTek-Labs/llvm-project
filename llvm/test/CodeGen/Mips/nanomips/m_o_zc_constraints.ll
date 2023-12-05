; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck %s

define void @test(i8* %p) {
entry:
; Don't allow any offsets when 'm'.
; CHECK: ll $zero, 0($a0)
; CHECK: addiu	$a1, $a0, 8
; CHECK: ll $zero, 0($a1)
  tail call void asm sideeffect "ll $$zero, $0", "*m,~{$1}"(i8* %p)
  %add.ptr = getelementptr inbounds i8, i8* %p, i32 8
  tail call void asm sideeffect "ll $$zero, $0", "*m,~{$1}"(i8* nonnull %add.ptr)
; Allow 13bit offsets when 'o'.
; CHECK: ll $zero, 0($a0)
; CHECK: addiu	$a1, $a0, 4096
; CHECK: ll $zero, 0($a1)
; CHECK: addiu[48]	$a1, $a1, -4097
; CHECK: ll $zero, 0($a1)
; CHECK: ll $zero, 4095($a0)
; CHECK: ll $zero, -4095($a0)
  tail call void asm sideeffect "ll $$zero, $0", "*o,~{$1}"(i8* %p)
  %add.ptr1 = getelementptr inbounds i8, i8* %p, i32 4096
  tail call void asm sideeffect "ll $$zero, $0", "*o,~{$1}"(i8* nonnull %add.ptr1)
  %add.ptr2 = getelementptr inbounds i8, i8* %p, i32 -4097
  tail call void asm sideeffect "ll $$zero, $0", "*o,~{$1}"(i8* nonnull %add.ptr2)
  %add.ptr3 = getelementptr inbounds i8, i8* %p, i32 4095
  tail call void asm sideeffect "ll $$zero, $0", "*o,~{$1}"(i8* nonnull %add.ptr3)
  %add.ptr4 = getelementptr inbounds i8, i8* %p, i32 -4095
  tail call void asm sideeffect "ll $$zero, $0", "*o,~{$1}"(i8* nonnull %add.ptr4)
; Allow 9bit 4byte aligned offsets when 'ZC'.
; CHECK: ll $zero, 0($a0)
; CHECK: addiu	$a1, $a0, 256
; CHECK: ll $zero, 0($a1)
; CHECK: addiu	$a1, $a0, -257
; CHECK: ll $zero, 0($a1)
; CHECK: ll $zero, 252($a0)
; CHECK: ll $zero, -252($a0)
; CHECK: addiu	$a0, $a0, 9
; CHECK: ll $zero, 0($a0)
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(i8* %p)
  %add.ptr5 = getelementptr inbounds i8, i8* %p, i32 256
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(i8* nonnull %add.ptr5)
  %add.ptr6 = getelementptr inbounds i8, i8* %p, i32 -257
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(i8* nonnull %add.ptr6)
  %add.ptr7 = getelementptr inbounds i8, i8* %p, i32 252
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(i8* nonnull %add.ptr7)
  %add.ptr8 = getelementptr inbounds i8, i8* %p, i32 -252
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(i8* nonnull %add.ptr8)
  %add.ptr9 = getelementptr inbounds i8, i8* %p, i32 9
  tail call void asm sideeffect "ll $$zero, $0", "*^ZC,~{$1}"(i8* nonnull %add.ptr9)
  ret void
}
