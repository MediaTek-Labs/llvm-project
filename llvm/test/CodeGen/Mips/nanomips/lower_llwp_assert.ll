; RUN: llc -mcpu=nanomips < %s
define void @__atomic_load_c() {
entry:
  %0 = load atomic i64, ptr null monotonic, align 2147483648
  ret void
}

define void @__atomic_store( ptr noundef %dest)  {
entry:
  store atomic i64 1, ptr %dest seq_cst, align 8
  ret void
}

@x = dso_local global i32 1, align 4
define dso_local void @f() {
entry:
  store volatile i32 0, ptr @x, align 4
  ret void
}
