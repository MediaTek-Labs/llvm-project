; RUN: llc -march=mips < %s | FileCheck %s
; RUN: llc -march=mips %s -filetype=obj -o - | llvm-readelf -s - | FileCheck --check-prefixes=CHECK-OBJECT %s

define dso_local void @test1() local_unnamed_addr #0 {
  tail call void asm sideeffect ".weak foo", "~{$1}"() #1
; CHECK: .weak{{.*}}foo
  tail call void asm sideeffect ".set foo, 0xdeadbeef", "~{$1}"() #1
; CHECK: .set{{ *foo *, *(0xdeadbeef|3735928559)}}
; CHECK-OBJECT: {{deadbeef.*WEAK.*foo}}
  ret void
}
