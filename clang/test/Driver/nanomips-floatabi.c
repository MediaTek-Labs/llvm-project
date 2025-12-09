// Test that target feature ite is implemented and available correctly

// RUN: not %clang -### -target nanomips-elf -mhard-float %s 2>&1 | FileCheck %s --check-prefix=HARDFLOAT

// RUN: %clang -### -target nanomips-elf -msoft-float %s 2>&1 | FileCheck %s

// RUN: %clang -### -target nanomips-elf %s 2>&1 | FileCheck %s

// HARDFLOAT: error: invalid float ABI '-mhard-float'
// CHECK: "-target-feature" "+soft-float"
