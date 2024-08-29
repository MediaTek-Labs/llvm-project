// RUN: %clang_cc1 -emit-llvm -o %t %s -fsanitize=kcfi 2>&1 | FileCheck %s
// RUN: FileCheck --check-prefixes=LLVM %s < %t
typedef volatile struct { int a; int b; } A; 
typedef struct { int a; int b; } B;
extern void (*callee1)(A *a);
extern void (*callee2)(B *b);
int main(int argc, char *argv[]) {
  A a;
  B b;
// CHECK: :[[@LINE+1]]:3: warning: indirect call of function with anonymous stucture reference in type is unsupported by kCFI
  callee1(&a);
// LLVM: [[P1:.*]] = load ptr{{.*}}@callee1
// LLVM-NEXT: call {{.*}}[[P1]]
// CHECK-NOT: {{.*}}: warning: indirect call of function with anonymous stucture reference in type is unsupported by kCFI
  callee2(&b);
// LLVM: [[P2:.*]] = load ptr{{.*}}@callee2
// LLVM-NEXT: getelementptr{{.*}}[[P2]]
  return 0;
}


