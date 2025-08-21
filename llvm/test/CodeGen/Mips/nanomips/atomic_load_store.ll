; RUN: llc -mcpu=nanomips < %s | tee %t.s | FileCheck %s
; RUN: clang --target=nanomips -c %t.s

; regression test for swapped operands
; CHECK-DAG: large_value_store32:
; CHECK: addiu [[VAL:\$a[0-9]+]],{{.*}}, 4096
; CHECK: sw [[VAL]], 0
; CHECK-DAG: end large_value_store32
define void @large_value_store32(i32 %0) {
  %add.offset = add i32 %0, 4096
  store atomic i32 %add.offset, ptr null monotonic, align 4
  ret void
}

; CHECK-DAG: small_offset_store32:
; CHECK: sw {{.*}}, 4095
; CHECK-DAG: end small_offset_store32
define void @small_offset_store32(i32 %0, i32 %1) {
  %add.offset = add i32 %0, 4095
  %addr = inttoptr i32 %add.offset to ptr
  store atomic i32 %1, ptr %addr monotonic, align 4
  ret void
}

; CHECK-DAG: large_offset_store32:
; CHECK: addiu [[PTR:\$a[0-9]+]],{{.*}}, 4096
; CHECK: sw {{.*}}, 0([[PTR]])
; CHECK-DAG: end large_offset_store32
define void @large_offset_store32(i32 %0, i32 %1) {
  %add.offset = add i32 %0, 4096
  %addr = inttoptr i32 %add.offset to ptr
  store atomic i32 %1, ptr %addr monotonic, align 4
  ret void
}

; CHECK-DAG: small_offset_load32:
; CHECK: lw {{.*}}, 4095
; CHECK-DAG: end small_offset_load32
define noundef i32 @small_offset_load32(i32 %0) {
  %add.offset = add i32 %0, 4095
  %addr = inttoptr i32 %add.offset to ptr
  %2 = load atomic i32, ptr %addr monotonic, align 4
  ret i32 %2
}

; CHECK-DAG: large_offset_load32:
; CHECK: addiu [[PTR:\$a[0-9]+]],{{.*}}, 4096
; CHECK: lw {{.*}}, 0([[PTR]])
; CHECK-DAG: end large_offset_load32
define noundef i32 @large_offset_load32(i32 %0) {
  %add.offset = add i32 %0, 4096
  %addr = inttoptr i32 %add.offset to ptr
  %2 = load atomic i32, ptr %addr monotonic, align 4
  ret i32 %2
}

; CHECK-DAG: small_offset_store16:
; CHECK: sh {{.*}}, 4095
; CHECK-DAG: end small_offset_store16
define void @small_offset_store16(i32 %0, i16 %1) {
  %add.offset = add i32 %0, 4095
  %addr = inttoptr i32 %add.offset to ptr
  store atomic i16 %1, ptr %addr monotonic, align 2
  ret void
}

; CHECK-DAG: large_offset_store16:
; CHECK: addiu [[PTR:\$a[0-9]+]],{{.*}}, 4096
; CHECK: sh {{.*}}, 0([[PTR]])
; CHECK-DAG: end large_offset_store16
define void @large_offset_store16(i16 %0, i32 %1) {
  %add.offset = add i32 %1, 4096
  %addr = inttoptr i32 %add.offset to ptr
  store atomic i16 %0, ptr %addr monotonic, align 2
  ret void
}

; CHECK-DAG: small_offset_load16:
; CHECK: lh {{.*}}, 4095
; CHECK-DAG: end small_offset_load16
define noundef i16 @small_offset_load16(i32 %0) {
  %add.offset = add i32 %0, 4095
  %addr = inttoptr i32 %add.offset to ptr
  %2 = load atomic i16, ptr %addr monotonic, align 2
  ret i16 %2
}

; CHECK-DAG: large_offset_load16:
; CHECK: addiu [[PTR:\$a[0-9]+]],{{.*}}, 4096
; CHECK: lh {{.*}}, 0([[PTR]])
; CHECK-DAG: end large_offset_load16
define noundef i16 @large_offset_load16(i32 %0) {
  %add.offset = add i32 %0, 4096
  %addr = inttoptr i32 %add.offset to ptr
  %2 = load atomic i16, ptr %addr monotonic, align 2
  ret i16 %2
}

; CHECK-DAG: small_offset_store8:
; CHECK: sb {{.*}}, 4095
; CHECK-DAG: end small_offset_store8
define void @small_offset_store8(i8 %0, i32 %1) {
  %add.offset = add i32 %1, 4095
  %addr = inttoptr i32 %add.offset to ptr
  store atomic i8 %0, ptr %addr monotonic, align 1
  ret void
}

; CHECK-DAG: large_offset_store8:
; CHECK: addiu [[PTR:\$a[0-9]+]],{{.*}}, 4096
; CHECK: sb {{.*}}, 0([[PTR]])
; CHECK-DAG: end large_offset_store8
define void @large_offset_store8(i8 %0, i32 %1) {
  %add.offset = add i32 %1, 4096
  %addr = inttoptr i32 %add.offset to ptr
  store atomic i8 %0, ptr %addr monotonic, align 1
  ret void
}

; CHECK-DAG: small_offset_load8:
; CHECK: lb {{.*}}, 4095
; CHECK-DAG: end small_offset_load8
define noundef i8 @small_offset_load8(i32 %0) {
  %add.offset = add i32 %0, 4095
  %addr = inttoptr i32 %add.offset to ptr
  %2 = load atomic i8, ptr %addr monotonic, align 1
  ret i8 %2
}

; CHECK-DAG: large_offset_load8:
; CHECK: addiu [[PTR:\$a[0-9]+]],{{.*}}, 4096
; CHECK: lb {{.*}}, 0([[PTR]])
; CHECK-DAG: end large_offset_load8
define noundef i8 @large_offset_load8(i32 %0) {
  %add.offset = add i32 %0, 4096
  %addr = inttoptr i32 %add.offset to ptr
  %2 = load atomic i8, ptr %addr monotonic, align 1
  ret i8 %2
}
