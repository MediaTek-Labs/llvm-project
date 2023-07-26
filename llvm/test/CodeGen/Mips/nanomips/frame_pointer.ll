; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck %s

; Check if fp is set correctly if function wants to use it. 
; We want it to point to -4096 from the beginning of the stack.
define void @test1(i32 %a, i32 %b, i32 %c, i32 %d, i32 %e, i32 %f) #0 {
entry:
  ; CHECK: save	64, $fp, $ra, $s0, $s1, $s2, $s3, $s4
  ; CHECK: addiu	$fp, $sp, -4032
  ; CHECK: sw	$a0, 32($sp)
  ; CHECK: restore.jrc	64, $fp, $ra, $s0, $s1, $s2, $s3, $s4 
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %c.addr = alloca i32, align 4
  %d.addr = alloca i32, align 4
  %e.addr = alloca i32, align 4
  %f.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  store i32 %b, i32* %b.addr, align 4
  store i32 %c, i32* %c.addr, align 4
  store i32 %d, i32* %d.addr, align 4
  store i32 %e, i32* %e.addr, align 4
  store i32 %f, i32* %f.addr, align 4
  call void asm sideeffect "", "~{$16},~{$17},~{$18},~{$19},~{$20},~{$1}"()
  
  ret void
}

; Check if offsets after fp setup are relative to fp if varible-sized 
; objects are present in function.
declare void @callee2(i8*)
define void @test2(i32 %a, i32 %b, i32 %c, i32 %d, i32 %e, i32 %f) #0 {
entry:
  ; CHECK: save	64, $fp, $ra, $s0, $s1, $s2, $s3, $s4
  ; CHECK: addiu	$fp, $sp, -4032
  ; CHECK: sw	$a0, 4064($fp)
  ; CHECK: restore.jrc	64, $fp, $ra, $s0, $s1, $s2, $s3, $s4
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %c.addr = alloca i32, align 4
  %d.addr = alloca i32, align 4
  %e.addr = alloca i32, align 4
  %f.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  store i32 %b, i32* %b.addr, align 4
  store i32 %c, i32* %c.addr, align 4
  store i32 %d, i32* %d.addr, align 4
  store i32 %e, i32* %e.addr, align 4
  store i32 %f, i32* %f.addr, align 4
  
  %0 = alloca i8, i32 %a  
  call void @callee2(i8* %0)

  call void asm sideeffect "", "~{$16},~{$17},~{$18},~{$19},~{$20},~{$1}"()
  
  ret void
}

; Check if offsets after fp setup stays relative to sp if 
; function needs stack realignment
declare void @callee3(i32*)
define void @test3(i32 %a, i32 %b, i32 %c, i32 %d, i32 %e, i32 %f) #0 {
entry:
  ; CHECK: save	64, $fp, $ra, $s0, $s1, $s2, $s3, $s4
  ; CHECK: addiu	$fp, $sp, -4032
  ; CHECK: sw	$a0, 32($sp)
  ; CHECK: restore.jrc	64, $fp, $ra, $s0, $s1, $s2, $s3, $s4
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %c.addr = alloca i32, align 4
  %d.addr = alloca i32, align 4
  %e.addr = alloca i32, align 4
  %f.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  store i32 %b, i32* %b.addr, align 4
  store i32 %c, i32* %c.addr, align 4
  store i32 %d, i32* %d.addr, align 4
  store i32 %e, i32* %e.addr, align 4
  store i32 %f, i32* %f.addr, align 4
  
  %0 = alloca i32, align 64
  call void @callee3(i32 *%0)

  call void asm sideeffect "", "~{$16},~{$17},~{$18},~{$19},~{$20},~{$1}"()
  
  ret void
}

; Check if offsets after fp setup are relative to BasePtr if varible-sized 
; objects are present in function and function needs stack realignment
declare void @callee4(i8*, i32*)
define void @test4(i32 %a, i32 %b, i32 %c, i32 %d, i32 %e, i32 %f) #0 {
entry:
  ; CHECK: save	192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7
  ; CHECK: addiu	$fp, $sp, -3904
  ; CHECK: sw	$a0, 148($s7)
  ; CHECK: restore.jrc	192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %c.addr = alloca i32, align 4
  %d.addr = alloca i32, align 4
  %e.addr = alloca i32, align 4
  %f.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  store i32 %b, i32* %b.addr, align 4
  store i32 %c, i32* %c.addr, align 4
  store i32 %d, i32* %d.addr, align 4
  store i32 %e, i32* %e.addr, align 4
  store i32 %f, i32* %f.addr, align 4
  
  %0 = alloca i8, i32 %a
  %1 = alloca i32, align 64
  call void @callee4(i8* %0, i32 *%1)

  call void asm sideeffect "", "~{$16},~{$17},~{$18},~{$19},~{$20},~{$1}"()
  
  ret void
}

attributes #0 = {"frame-pointer"="all"}
!llvm.module.flags = !{!0}
!0 = !{i32 7, !"frame-pointer", i32 2}
