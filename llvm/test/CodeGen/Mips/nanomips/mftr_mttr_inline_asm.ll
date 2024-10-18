; RUN: llc -mtriple=nanomips -mattr=+mt -asm-show-inst -verify-machineinstrs < %s | FileCheck %s
; The COP register in mftr/mttr should be emitted as $0 and not $zero

define void @test() {
  call void asm sideeffect "mftlo $$a5", ""()
; CHECK: mftr $a5, $0, 1, 1, 0
  call void asm sideeffect "mftlo $$a5,$$ac0", ""()
; CHECK: mftr $a5, $0, 1, 1, 0
  call void asm sideeffect "mttlo $$a5", ""()
; CHECK: mttr $a5, $0, 1, 1, 0
  call void asm sideeffect "mttlo $$a5,$$ac0", ""()
; CHECK: mttr $a5, $0, 1, 1, 0
  ret void 
}
