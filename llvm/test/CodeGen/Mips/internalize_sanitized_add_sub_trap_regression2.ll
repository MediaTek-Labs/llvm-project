; RUN: llc -enable-nmips-addsubtrap=1 %s
; crashed since chain operand and result were not created in lowering to machine code

target datalayout = "e-m:e-p:32:32-i8:8:32-i16:16:32-i64:64-n32:64-S128"
target triple = "nanomips-unknown-unknown-elf"

%struct.pf_link_t = type { i16, %struct.list_node}
%struct.list_node = type { ptr }

define void @_remove_pf_link(ptr %0) {
  call void @llvm.dbg.value(metadata ptr %0, metadata !3, metadata !DIExpression()), !dbg !6
  %2 = getelementptr %struct.pf_link_t, ptr %0, i32 0, i32 1
  %3 = tail call ptr null(ptr null, ptr %2)
  %4 = load i32, ptr null, align 4
  %5 = tail call i32 @llvm.mips.sadd.trap.on.overflow.i32(i32 %4, i32 1)
  store i32 %5, ptr null, align 4
  ret void
}
declare void @llvm.dbg.value(metadata,metadata,metadata)
declare i32 @llvm.mips.sadd.trap.on.overflow.i32(i32, i32)

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2}
!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1)
!1 = !DIFile(filename: "a.c", directory: ".")
!2 = !{i32 2, !"Debug Info Version", i32 3}
!3 = !DILocalVariable(name: "p_pf_link", arg: 1, scope: !4)
!4 = distinct !DISubprogram(name: "_remove_pf_link", scope: !5, spFlags: DISPFlagDefinition, unit: !0)
!5 = !DIFile(filename: "a.h", directory: ".")
!6 = !DILocation(scope: !4)
