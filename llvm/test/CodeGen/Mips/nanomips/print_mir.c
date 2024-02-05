// RUN: clang -O3 --target=nanomips -S %s -mllvm -print-after-all |& FileCheck %s
// CHECK: IR Dump After nanoMIPS Register Re-allocation pass (nmregrealloc)
// CHECK: IR Dump After nanoMIPS load/store optimization pass (nmloadstoreopt)
// CHECK: IR Dump After nanoMIPS move optimization pass (nmmoveopt)
void foo() { }
