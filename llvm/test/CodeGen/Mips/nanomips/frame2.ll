; RUN: llc -mtriple=nanomips-unknown=elf --frame-pointer=none -verify-machineinstrs < %s | FileCheck %s

define dso_local void @foo() local_unnamed_addr {
; CHECK-LABEL: foo:
; CHECK-NOT:    addiu ${{[ats][0-9]}}, $sp, {{.*}}
; CHECK:        sw $zero, {{.*}}($sp)
entry:
  %__pad0 = alloca [100 x i32], align 4
  %local = alloca i32, align 4
  %__pad1 = alloca [100 x i32], align 4
  %0 = bitcast [100 x i32]* %__pad0 to i8*
  %1 = bitcast i32* %local to i8*
  %2 = bitcast [100 x i32]* %__pad1 to i8*
  store i32 0, i32* %local, align 4
  call void asm sideeffect "", "*m,*m,*m,~{$1}"([100 x i32]* nonnull %__pad0, i32* nonnull %local, [100 x i32]* nonnull %__pad1)
  ret void
}

