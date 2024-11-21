; RUN: llc -mtriple=nanomips -nmips-fix-hw110880 -asm-show-inst < %s | FileCheck --check-prefixes=CHECK,ENABLED %s
; RUN: llc -mtriple=nanomips -nmips-hw110880-guard-function-ends=false -asm-show-inst < %s | FileCheck --check-prefixes=CHECK,DISABLED %s


;; CHECK-LABEL: ends_with_break:
define i32 @ends_with_break(i32 noundef signext %i, i32 noundef signext %j) #0 {
entry:
  %add = add nsw i32 %j, 3
  %cmp = icmp sgt i32 %i, %add
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %mul = mul nsw i32 %i, %j
  ret i32 %mul

;; CHECK: break
;; ENABLED: bc{{.*}}Ltmp
;; DISABLED-NOT: bc{{.*}}Ltmp

if.else:                                          ; preds = %entry
  call void @llvm.ubsantrap(i8 18)
  unreachable

}

;; CHECK-LABEL: tail_callee:
define i32 @tail_callee(i32 signext %i, i32 signext %j) #1 {
  %add = add i32 %i, %j;
  ret i32 %add;

;; A simple return doesn't need a guard
;; CHECK: jrc
;; ENABLED-NOT: bc{{.*}}Ltmp
;; DISABLED-NOT: bc{{.*}}Ltmp
}

;; CHECK-LABEL: tail_caller:
define i32 @tail_caller(i32 signext %i, i32 signext %j) #0 {
  %add1 = add i32 %i, 33;
  %add2 = add i32 %i, 22;
  %call = call i32 @tail_callee(i32 %add1, i32 %add2);
  ret i32 %call;

;; Tail call doesn't need a guard
;; CHECK: balc
;; ENABLED-NOT: bc{{.*}}Ltmp
;; DISABLED-NOT: bc{{.*}}Ltmp
}


;; CHECK-LABEL: call_assert_fail
define i32 @call_assert_fail(i32 %i) #0 {
  %cmp = icmp eq i32 %i, 0
  br i1 %cmp, label %if.then, label %if.end
if.then:
  call void @__assert_fail();
  unreachable
if.end:
  ret i32 %i

;; A noreturn function call must be guarded as the hardware return
;; stack can redirect prefetch.
;; CHECK: balc
;; ENABLED: bc{{.*}}Ltmp
;; DISABLED-NOT: bc{{.*}}Ltmp
}

declare void @__assert_fail(...) #2

declare void @llvm.ubsantrap(i8 immarg)


attributes #0 = { "target-features"="+fix-hw110880" }
attributes #1 = { noinline "target-features"="+fix-hw110880" }
attributes #2 = { noreturn "target-features"="+fix-hw110880" }
