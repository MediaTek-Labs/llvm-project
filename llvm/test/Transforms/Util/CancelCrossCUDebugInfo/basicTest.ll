; RUN: opt -passes=cancel-cross-cu-debug-info -S < %s | FileCheck %s

; CHECK: !24 = !DILocalVariable(name: "i", arg: 1, scope: !25, file: !1, line: 4, type: !16)
; CHECK-NEXT: !25 = distinct !DISubprogram(name: "foo1", linkageName: "foo1", scope: !1, file: !1, line: 4, type: !19, scopeLine: 4, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !3, retainedNodes: !26)
; CHECK-NEXT: !26 = !{!24}

@.str = private unnamed_addr constant [19 x i8] c"Hi from inline %d\0A\00", align 1

declare void @llvm.dbg.value(metadata, metadata, metadata) #0

declare dso_local noundef i32 @printf(i8* nocapture noundef readonly, ...) local_unnamed_addr #1

define dso_local i32 @main() local_unnamed_addr #2 !dbg !13 {
entry:
  call void @llvm.dbg.value(metadata i32 3, metadata !17, metadata !DIExpression()) #3, !dbg !22
  call void @llvm.dbg.value(metadata i32 3, metadata !24, metadata !DIExpression()) #3, !dbg !27
  %call.i = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([19 x i8], [19 x i8]* @.str, i64 0, i64 0), i32 3) #3, !dbg !29
  ret i32 0, !dbg !30
}

attributes #0 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #1 = { nofree nounwind "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nofree nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { nounwind }

!llvm.dbg.cu = !{!0, !3}
!llvm.ident = !{!5, !5}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 13.0.0", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test3.c", directory: "")
!2 = !{}
!3 = distinct !DICompileUnit(language: DW_LANG_C99, file: !4, producer: "clang version 13.0.0", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!4 = !DIFile(filename: "test2.c", directory: "")
!5 = !{!"clang version 13.0.0"}
!6 = !{i32 7, !"Dwarf Version", i32 4}
!7 = !{i32 2, !"Debug Info Version", i32 3}
!8 = !{i32 1, !"wchar_size", i32 4}
!9 = !{i32 7, !"uwtable", i32 1}
!10 = !{i32 1, !"ThinLTO", i32 0}
!11 = !{i32 1, !"EnableSplitLTOUnit", i32 1}
!12 = !{i32 1, !"LTOPostLink", i32 1}
!13 = distinct !DISubprogram(name: "main", scope: !4, file: !4, line: 13, type: !14, scopeLine: 13, flags: DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !3, retainedNodes: !2)
!14 = !DISubroutineType(types: !15)
!15 = !{!16}
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !DILocalVariable(name: "t", arg: 1, scope: !18, file: !4, line: 9, type: !16)
!18 = distinct !DISubprogram(name: "foo5", scope: !4, file: !4, line: 9, type: !19, scopeLine: 9, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !3, retainedNodes: !21)
!19 = !DISubroutineType(types: !20)
!20 = !{null, !16}
!21 = !{!17}
!22 = !DILocation(line: 0, scope: !18, inlinedAt: !23)
!23 = distinct !DILocation(line: 14, column: 1, scope: !13)
!24 = !DILocalVariable(name: "i", arg: 1, scope: !25, file: !1, line: 4, type: !16)
!25 = distinct !DISubprogram(name: "foo1", scope: !1, file: !1, line: 4, type: !19, scopeLine: 4, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !26)
!26 = !{!24}
!27 = !DILocation(line: 0, scope: !25, inlinedAt: !28)
!28 = distinct !DILocation(line: 10, column: 5, scope: !18, inlinedAt: !23)
!29 = !DILocation(line: 5, column: 6, scope: !25, inlinedAt: !28)
!30 = !DILocation(line: 15, column: 1, scope: !13)
