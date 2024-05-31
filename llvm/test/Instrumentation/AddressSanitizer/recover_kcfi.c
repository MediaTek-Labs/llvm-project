// the optimizer (middle end) pass should generate the following handlers
// for sanitizer violations:
//   if (trap): trap
//   else lib handler + continue

// RUN: clang %s -S -fsanitize=kcfi -fsanitize-trap=kcfi -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix TRAP

// RUN: clang %s -S -fsanitize=kcfi -fsanitize-trap -fno-sanitize-trap=kcfi \
// RUN: -g -emit-llvm -o - | FileCheck %s --check-prefix RECOVER

// RUN: clang %s -S -fsanitize=kcfi -fno-sanitize-trap -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix RECOVER

// RUN: clang %s -S -fsanitize=kcfi -fsanitize-trap -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix TRAP

// RUN: clang %s -S -fsanitize=kcfi -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix TRAP


// TRAP: call void @llvm.debugtrap()
// TRAP-NEXT: unreachable
// RECOVER: [[VAR1:@[0-9]+]] = private constant {{.*}}c"{{.*}}/recover_kcfi.c\00"
// RECOVER: [[VAR2:@[0-9]+]] = private constant { ptr, i32, i32 } { ptr [[VAR1]], i32 100, i32 3 }
// RECOVER: call void @__ubsan_handle_kcfi(ptr [[VAR2]])
// RECOVER-NEXT: br label
typedef int (*callback1_t)(int);
void assign_func(callback1_t *p);
int main() {
  callback1_t callback;
  assign_func(&callback);
#line 100
  callback(42);
  return 0;
}
