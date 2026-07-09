# ELF header flags for nanoMIPS

# Default header flags
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -o - < %s \
# RUN:  | llvm-readelf -h - | FileCheck %s

# CHECK: ELF Header:
# CHECK: Class: ELF32
# CHECK: OS/ABI: Standalone App
# CHECK: ABI Version: 0
# CHECK: Type: REL (Relocatable file)
# CHECK: Machine: nanoMIPS
# CHECK: Version: 0x1
# CHECK: Flags: 0x1000, unknown CPU, p32, nanomips32r6

# Relaxable:
# RUN: llvm-mc -filetype=obj -triple nanomips-elf --mattr=+relax -o - < %s \
# RUN:  | llvm-readelf -h - | FileCheck -check-prefix=CHECK-RELAX %s
# CHECK-RELAX: Flags: 0x1001, relaxable, unknown CPU, p32, nanomips32r6

# PC-relative:
# RUN: llvm-mc -filetype=obj -triple nanomips-elf --mattr=+pcrel -o - < %s \
# RUN:  | llvm-readelf -h - | FileCheck -check-prefix=CHECK-PCREL %s
# CHECK-PCREL: Flags: 0x1010, PC-relative, unknown CPU, p32, nanomips32r6

# Relaxable PC-relative
# RUN: llvm-mc -filetype=obj -triple nanomips-elf --mattr=+pcrel,+relax -o - < %s \
# RUN:  | llvm-readelf -h - | FileCheck -check-prefix=CHECK-RPC %s
# CHECK-RPC: Flags: 0x1011, relaxable, PC-relative, unknown CPU, p32, nanomips32r6
