; add inert attribute on the fly
; RUN: (cat %s; echo "attributes #0 = { nounwind }")  | \
; RUN:   llc -mcpu=nanomips -mattr=+dsp  | FileCheck %s --check-prefix DEF
; add size optimization attribute on the fly
; RUN: (cat %s; echo "attributes #0 = { nounwind optsize }")  | \
; RUN: llc -mcpu=nanomips -mattr=+dsp | \
; RUN:    FileCheck %s --check-prefix SIZE

; DEF-LABEL: L40_mac_loop:
; DEF-DAG: mtlo $
; DEF-DAG: mthi $
; DEF: This Inner Loop Header
; DEF-DAG: maq_s.w.phr $
; DEF-NOT: mfhi $
; DEF-NOT: mflo $
; DEF: bneic $
; by default extracting twice out of the loop is better
; than extracting once in the loop
; DEF: mflo $
; DEF: mfhi $
; DEF: mflo $
; DEF: mfhi $

; SIZE-LABEL: L40_mac_loop:
; SIZE-DAG: mtlo $
; SIZE-DAG: mthi $
; SIZE: This Inner Loop Header
; SIZE: test.c:7:25
; SIZE-DAG: maq_s.w.phr $
; SIZE: mfhi $
; SIZE: mflo $
; SIZE-DAG: bneic $
; extracting once in the loop is better than
; extracting twice out of the loop if size importance is increase
; SIZE-NOT: test.c:7:25
; SIZE-NOT: mfhi $
; SIZE-NOT: mflo $

define dso_local i64 @L40_mac_loop(i64 noundef signext %L40_var1,
                                   ptr nocapture noundef readonly %var2,
                                   ptr nocapture noundef readonly %var3,
                                   ptr nocapture noundef writeonly %p)
                                   local_unnamed_addr #0 !dbg !10 {
entry:
  br label %for.body

for.cond.cleanup:                                 ; preds = %for.body
  store i64 %2, ptr %p, align 8
  ret i64 %2

for.body:                                         ; preds = %entry, %for.body
  %i.08 = phi i32 [ 0, %entry ], [ %inc, %for.body ]
  %L40_var1.addr.07 = phi i64 [ %L40_var1, %entry ], [ %2, %for.body ]
  %arrayidx = getelementptr inbounds <2 x i16>, ptr %var2, i32 %i.08
  %0 = load <2 x i16>, ptr %arrayidx, align 4
  %arrayidx1 = getelementptr inbounds <2 x i16>, ptr %var3, i32 %i.08
  %1 = load <2 x i16>, ptr %arrayidx1, align 4
  %2 = tail call i64 @llvm.mips.maq.s.w.phr(i64 %L40_var1.addr.07, <2 x i16> %0, <2 x i16> %1), !dbg !41
  call void @llvm.dbg.value(metadata i64 %2, metadata !21, metadata !DIExpression()), !dbg !28
  %inc = add nuw nsw i32 %i.08, 1
  %exitcond.not = icmp eq i32 %inc, 16
  br i1 %exitcond.not, label %for.cond.cleanup, label %for.body, !dbg !30
}

; Function Attrs: nounwind
declare i64 @llvm.mips.maq.s.w.phr(i64, <2 x i16>, <2 x i16>) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #2

attributes #1 = { nounwind }
attributes #2 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!6, !7}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1,
                             producer: "Vendor clang version 16.0.6 ...)",
                             isOptimized: true, runtimeVersion: 0,
                             emissionKind: FullDebug, retainedTypes: !2,
                             splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "st.c", directory: "tools")
!2 = !{!3}
!3 = !DIDerivedType(tag: DW_TAG_typedef, name: "Word40", file: !4, line: 2, baseType: !5)
!4 = !DIFile(filename: "test.c", directory: "path/to")
!5 = !DIBasicType(name: "long long", size: 64, encoding: DW_ATE_signed)
!6 = !{i32 7, !"Dwarf Version", i32 4}
!7 = !{i32 2, !"Debug Info Version", i32 3}
!10 = distinct !DISubprogram(name: "L40_mac_loop", scope: !4, file: !4, line: 5,
                             type: !11, scopeLine: 5,
                             flags: DIFlagPrototyped | DIFlagAllCallsDescribed,
                             spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!11 = !DISubroutineType(types: !12)
!12 = !{}
!21 = !DILocalVariable(name: "L40_var1", arg: 1, scope: !10, file: !4, line: 5, type: !3)
!26 = distinct !DILexicalBlock(scope: !10, file: !4, line: 6, column: 3)
!27 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!28 = !DILocation(line: 0, scope: !10)
!30 = !DILocation(line: 6, column: 3, scope: !26)
!38 = distinct !DILexicalBlock(scope: !26, file: !4, line: 6, column: 3)
!41 = !DILocation(line: 7, column: 25, scope: !38)
