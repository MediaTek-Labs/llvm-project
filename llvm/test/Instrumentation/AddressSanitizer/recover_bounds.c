// the optimizer (middle end) pass should generate the following handlers
// for sanitizer violations:
//   if (trap): trap
//   else if (recover): lib handler + continue
//   else : lib handler + abort
// In non trapping cases, the frontend handlers are still generated according to "recover" scheme

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds \
// RUN:   -fsanitize-trap=bounds  -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix TRAP

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds \
// RUN:   -fsanitize-recover=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefixes HANDLER,RECOVER --implicit-check-not DEFAULT

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds -fsanitize-trap \
// RUN:   -fno-sanitize-trap=bounds -fsanitize-recover=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefixes HANDLER,RECOVER --implicit-check-not DEFAULT

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds -fno-sanitize-trap \
// RUN:   -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefixes HANDLER,DEFAULT

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds -fsanitize-trap \
// RUN:   -fno-sanitize-trap=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefixes HANDLER,DEFAULT

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds \
// RUN:   -fsanitize-trap=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefix TRAP

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds \
// RUN:   -fno-sanitize-recover -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefixes HANDLER,DEFAULT

// RUN: clang %s -S -Wno-array-bounds -fsanitize=bounds \
// RUN:   -fsanitize-recover -fno-sanitize-recover=bounds -g -emit-llvm -o - | \
// RUN:   FileCheck %s --check-prefixes HANDLER,DEFAULT

// TRAP: call void @llvm.trap()
// TRAP-NEXT: unreachable
// HANDLER: [[VAR1:@[0-9]+]] = private constant {{.*}}c"{{.*}}/recover_bounds.c\00"
// HANDLER: [[VAR2:@[0-9]+]] = private constant { ptr, i32, i32 } { ptr [[VAR1]], i32 100, i32 10 }
// RECOVER: call void @__ubsan_handle_out_of_bounds_minimal(ptr [[VAR2]])
// RECOVER-NEXT: br label
// DEFAULT: call void @__ubsan_handle_out_of_bounds_minimal_abort(ptr [[VAR2]])
// DEFAULT-NEXT: unreachable
int arr[2];
int foo(int n) {
#line 100
  return arr[n];
}
