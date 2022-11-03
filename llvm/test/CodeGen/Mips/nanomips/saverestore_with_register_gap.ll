; RUN: llc -mtriple=nanomips -asm-show-inst -verify-machineinstrs < %s | FileCheck %s

; Make sure that SAVE/RESTORE instructions are working even though there 
; is a gap in the callee-saved register sequence.
define void @test1() {
; CHECK: save 32, $s0, $s1, $s2, $s3, $s4
  call void asm sideeffect "", "~{$16},~{$18},~{$20},~{$1}"() ret void
; CHECK: restore.jrc 32, $s0, $s1, $s2, $s3, $s4
}

define void @test2() {
; CHECK: save	48, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7
  call void asm sideeffect "", "~{$16},~{$23},~{$30},~{$1}"() ret void
; CHECK: restore.jrc	48, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7
}

; Make sure to generate correct SAVE/RESTOR sp offset in case there was a gap
; in callee-saved register sequence and there are more things to be stored on 
; stack after storing values from callee-saved registers. For example values 
; from ax registers, used to pass function arguments, can be stored on stack 
; after values from callee-saved registers.
define void @test3(i32 %a, i32 %b, i32 %c, i32 %d, i32 %e, i32 %f, i32 %g, i32 %h) {
  
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %c.addr = alloca i32, align 4
  %d.addr = alloca i32, align 4
  %e.addr = alloca i32, align 4
  %f.addr = alloca i32, align 4
  %g.addr = alloca i32, align 4
  %h.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  store i32 %b, i32* %b.addr, align 4
  store i32 %c, i32* %c.addr, align 4
  store i32 %d, i32* %d.addr, align 4
  store i32 %e, i32* %e.addr, align 4
  store i32 %f, i32* %f.addr, align 4
  store i32 %g, i32* %g.addr, align 4
  store i32 %h, i32* %h.addr, align 4  
	; CHECK: save	64, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7
  call void asm sideeffect "", "~{$16},~{$23},~{$1}"() ret void  
	; CHECK: restore.jrc	64, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7
}