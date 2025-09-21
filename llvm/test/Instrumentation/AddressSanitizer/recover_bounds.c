// the optimizer (middle end) pass should generate the following handlers
// for sanitizer violations:
//   if (trap): trap
//   else if (recover): lib handler + continue
//   else : lib handler + abort
// In non trapping cases, the frontend handlers are still generated according to "recover" scheme

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds \
// RUN:   -fsanitize-trap=bounds  -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix TRAP

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds -fsanitize-trap \
// RUN:   -mllvm -explicit-param-bounds-checking-handler \
// RUN:   -fno-sanitize-trap=bounds -fsanitize-recover=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix RECOVER-EXP

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds -fno-sanitize-trap \
// RUN:   -mllvm -explicit-param-bounds-checking-handler \
// RUN:   -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix ABORT-EXP

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds -fsanitize-trap \
// RUN:   -mllvm -explicit-param-bounds-checking-handler \
// RUN:   -fno-sanitize-trap=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix ABORT-EXP

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds -fsanitize-trap \
// RUN:   -fno-sanitize-trap=bounds -fsanitize-recover=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix RECOVER-IMP

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds -fno-sanitize-trap \
// RUN:   -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix ABORT-IMP

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds -fsanitize-trap \
// RUN:   -fno-sanitize-trap=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix ABORT-IMP

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds \
// RUN:   -mllvm -explicit-param-bounds-checking-handler \
// RUN:   -fsanitize-trap=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix TRAP

// TRAP: call void @llvm.ubsantrap
// TRAP-NEXT: unreachable

// RECOVER-EXP: [[VAR:@[0-9]+]] = private constant {{.*}}c"{{.*}}/recover_bounds.c\00"
// RECOVER-EXP: [[VAR2:@[0-9]+]] = private constant { ptr, i32, i32 } { ptr [[VAR]], i32 100, i32 10 }
// RECOVER-EXP-LABEL: trap:
// RECOVER-EXP: call void @__ubsan_handle_local_out_of_bounds_explicit_param(ptr [[VAR2]])
// RECOVER-EXP-NEXT: br label

// RECOVER-IMP-LABEL: trap:
// RECOVER-IMP: call void @__ubsan_handle_local_out_of_bounds()
// RECOVER-IMP-NEXT: br label

// ABORT-EXP: [[VAR:@[0-9]+]] = private constant {{.*}}c"{{.*}}/recover_bounds.c\00"
// ABORT-EXP: [[VAR2:@[0-9]+]] = private constant { ptr, i32, i32 } { ptr [[VAR]], i32 100, i32 10 }
// ABORT-EXP-LABEL: trap:
// ABORT-EXP: call void @__ubsan_handle_local_out_of_bounds_abort_explicit_param(ptr [[VAR2]])
// ABORT-EXP-NEXT: unreachable

// ABORT-IMP-LABEL: trap:
// ABORT-IMP: call void @__ubsan_handle_local_out_of_bounds_abort()
// ABORT-IMP-NEXT: unreachable

int arr[2];
int foo(int n) {
#line 100
  return arr[n];
}
