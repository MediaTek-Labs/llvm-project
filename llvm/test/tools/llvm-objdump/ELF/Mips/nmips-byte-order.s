# RUN: llvm-mc -filetype=obj -triple nanomips-elf < %s | \
# RUN:     llvm-objdump  --nmips-little-endian-order -d - | \
# RUN:     FileCheck %s --check-prefixes=CHECK_ALT_ORDER

# RUN: llvm-mc -filetype=obj -triple nanomips-elf < %s | \
# RUN:     llvm-objdump -d - | \
# RUN:     FileCheck %s

foo:
  li $a0, -1              // 16-bit
# CHECK_ALT_ORDER:            0: 7f d2         li      $a0, -0x1
# CHECK:  0: d27f         li      $a0, -0x1

  addiu $a5, $a6, 4095    // 32-bit
# CHECK_ALT_ORDER:            2: 2a 01 ff 0f   addiu   $a5, $a6, 0xfff
# CHECK:  2: 012a 0fff    addiu   $a5, $a6, 0xfff

  li $t0, 0x6789abcd      // 48-bit
# CHECK_ALT_ORDER:            6: 80 61 cd ab 89 67     li      $t0, 0x6789abcd
# CHECK:  6: 6180  abcd  6789      li      $t0, 0x6789abcd
