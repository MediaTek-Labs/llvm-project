;RUN: llc  -mcpu=nanomips -mattr=+dsp  < %s

define i32 @func(ptr %c) {
entry:
  %0 = load i16, ptr %c, align 2, !dbg !4
  %conv.i = sext i16 %0 to i32
  %1 = bitcast i32 %conv.i to <2 x i16>
  %2 = tail call i64 @llvm.mips.maq.s.w.phr(i64 0, <2 x i16> %1, <2 x i16> zeroinitializer)
  ; simplified case in which multiple llvm.dbg.value refer the same instruction
  call void @llvm.dbg.value(metadata i64 %2, metadata !14, metadata !DIExpression()), !dbg !17
  call void @llvm.dbg.value(metadata i64 %2, metadata !14, metadata !DIExpression()), !dbg !17
  %3 = tail call i64 @llvm.mips.maq.s.w.phr(i64 %2, <2 x i16> zeroinitializer, <2 x i16> zeroinitializer)
  ret i32 0
}

; Function Attrs: nounwind
declare i64 @llvm.mips.maq.s.w.phr(i64, <2 x i16>, <2 x i16>) #0

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; uselistorder directives
uselistorder ptr @llvm.mips.maq.s.w.phr, { 1, 0 }
uselistorder ptr @llvm.dbg.value, { 1, 0 }

attributes #0 = { nounwind }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "crash.c", directory: "")
!2 = !{}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !DILocation(line: 24, column: 32, scope: !5)
!5 = distinct !DILexicalBlock(scope: !7, file: !6, line: 23, column: 28)
!6 = !DIFile(filename: "", directory: "")
!7 = distinct !DILexicalBlock(scope: !8, file: !6, line: 23, column: 5)
!8 = distinct !DILexicalBlock(scope: !9, file: !6, line: 23, column: 5)
!9 = distinct !DILexicalBlock(scope: !10, file: !6, line: 22, column: 26)
!10 = distinct !DILexicalBlock(scope: !11, file: !6, line: 22, column: 3)
!11 = distinct !DILexicalBlock(scope: !12, file: !6, line: 22, column: 3)
!12 = distinct !DISubprogram(name: "func", scope: !6, file: !6, line: 19, type: !13, scopeLine: 19, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !2)
!13 = !DISubroutineType(types: !2)
!14 = !DILocalVariable(name: "L40_s0", scope: !12, file: !6, line: 21, type: !15)
!15 = !DIDerivedType(tag: DW_TAG_typedef, name: "Word40", file: !6, line: 5, baseType: !16)
!16 = !DIBasicType(name: "long long", size: 64, encoding: DW_ATE_signed)
!17 = !DILocation(line: 0, scope: !12)
