; RUN: llc -march=mips < %s >  /dev/null 2>&1
; RUN: llc -march=nanomips < %s > /dev/null 2>&1

define dso_local i32 @foo(ptr nocapture noundef readonly %0) local_unnamed_addr {
  %2 = load i288, ptr%0, align 4
  %3 = lshr i288 %2, 32
  %4 = trunc i288 %3 to i32
  %5 = and i32 %4, 8191
  %6 = lshr i288 %2, 45
  %7 = trunc i288 %6 to i32
  %8 = and i32 %7, 8191
  %9 = add nuw nsw i32 %5, %8
  ret i32 %9
}
