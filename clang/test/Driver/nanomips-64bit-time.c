// Check passing Mips ABI options to the backend.
//
// RUN: %clang -target nanomips-elf -c -muse-64bit-time_t %s
//
// RUN: not %clang -target nanomips-elf -c -mno-use-64bit-time_t %s 2>&1 | FileCheck --check-prefix=ERROR %s
// RUN: not %clang -target nanomips-elf -c %s 2>&1 | FileCheck --check-prefix=ERROR %s
// ERROR: error: "FAIL: __nanomips_64bit_time_t__ not defined"

#ifndef __nanomips_64bit_time_t__
#error "FAIL: __nanomips_64bit_time_t__ not defined"
#endif
