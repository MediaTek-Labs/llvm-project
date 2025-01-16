// REQUIRES: mips-registered-target
// RUN: %clang -mdsp %s -O3 -S -o - | FileCheck %s

typedef long long a64;
#define CLOBBER_EXCEPT(CL0,CL1,CL2,CL3) \
{                                       \
    __asm__ __volatile__ (              \
     "# mark clobber\n\t"               \
     ::                                 \
     :CL0,CL1,CL2,CL3                   \
    );                                  \
}
#define CLOBBER_EXCEPT_AC1 CLOBBER_EXCEPT("$ac0","$ac0","$ac2","$ac3")
#define CLOBBER_EXCEPT_AC2 CLOBBER_EXCEPT("$ac0","$ac1","$ac1","$ac3")
#define CLOBBER_ALL        CLOBBER_EXCEPT("$ac0","$ac1","$ac2","$ac3")

#define FORCE(NAME,CLOBBER_ACC) \
int force_##NAME(int int_b, int int_c) {              \
  int ret1, ret2;                                     \
  a64 a64_r, a64_a;                                   \
  a64_a = 0x0LL;                                      \
  a64_r = __builtin_mips_madd(a64_a, 0x1234, 0x5678); \
  CLOBBER_ACC                                         \
  a64_r = __builtin_mips_msub(a64_r, 0x8765, 0x4321); \
  ret2 =  __builtin_mips_extr_w(a64_r, 31);           \
  return ret1 + ret2;                                 \
}

// CHECK-DAG: force_ac1
// CHECK: madd $ac1
// CHECK: msub $ac1
// CHECK-NOT: mflo
// CHECK-NOT: mtlo
// CHECK-DAG: end force_ac1
FORCE(ac1, CLOBBER_EXCEPT_AC1)

// CHECK-DAG: force_ac2
// CHECK: madd $ac2
// CHECK: msub $ac2
// CHECK-NOT: mflo
// CHECK-NOT: mtlo
// CHECK-DAG: end force_ac2
FORCE(ac2, CLOBBER_EXCEPT_AC2)

// CHECK-DAG: force_spill
// CHECK: mflo [[REG1:\$[0-9]]], $ac
// CHECK: sw [[REG1]]{{.*}}sp
// CHECK: mfhi [[REG2:\$[0-9]]], $ac
// CHECK: sw [[REG2]]{{.*}}sp
// CHECK: # mark clobber
// CHECK: lw [[REG3:\$[0-9]]]{{.*}}sp
// CHECK: mtlo [[REG3]], $ac
// CHECK: lw [[REG4:\$[0-9]]]{{.*}}sp
// CHECK: mthi [[REG4]], $ac
// CHECK-DAG: end force_spill
FORCE(spill, CLOBBER_ALL)
