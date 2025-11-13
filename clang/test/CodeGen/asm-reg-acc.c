// REQUIRES: mips-registered-target
// RUN: %clang -mdsp %s -O3 -S -o - | FileCheck %s
typedef long long a64;

// CHECK-LABEL: op_only
// CHECK: mtlo	$a{{[0-9]+}}, [[ACC:\$ac[0-3]]]
// CHECK: mthi	$a{{[0-9]+}}, [[ACC]]
// CHECK: #APP
// CHECK: maddu   [[ACC]], $a0, $a1
// CHECK: # read [[ACC]]
// CHECK: #NO_APP
// CHECK:extr.w  $a0, [[ACC]], 31
int op_only(int b, int c) {
  int ret1, ret2;
   a64 a64_a = 0LL;
   a64_a = b; a64_a += c;
  __asm__ __volatile__ ("maddu $ac0, %[op1], %[op2]; // read %[op3] \n\t"
      :
      : [op1] "r" (b), [op2] "r" (c), [op3] "A" (a64_a)
      :
  );
  ret2 =  __builtin_mips_extr_w(a64_a, 31);
  return ret2;
}

// CHECK-LABEL: op_result
// CHECK: mtlo ${{.*}}, [[ACC:\$ac[0-3]]]
// CHECK: mthi $zero, [[ACC]]
// CHECK: #APP
// CHECK: maddu   [[ACC]], $a0, $a1
// CHECK: # read $ac0
// CHECK: #NO_APP
// CHECK:extr.w  $a0, [[ACC]], 31
int op_result(int b, int c) {
  int ret1, ret2;
   a64 a64_a = 1LL;
  __asm__ __volatile__ ("maddu %[dst], %[op1], %[op2]; // read %[op3] \n\t"
      : [dst] "=A" (a64_a)
      : [op1] "r" (b), [op2] "r" (c), [op3] "A" (a64_a)
      :
  );
  ret2 =  __builtin_mips_extr_w(a64_a, 31);
  return ret2;
}
