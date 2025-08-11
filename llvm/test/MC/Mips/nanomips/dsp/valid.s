# RUN: llvm-mc -show-encoding -triple=nanomips-elf -mattr=dsp %s | FileCheck %s
#
  .set noat
  addu.qb         $a1, $a0, $t5   # CHECK: addu.qb         $a1, $a0,  $t5   # encoding: [0x64,0x20,0xcd,0x28]
  addu_s.qb       $t5, $a0, $a1   # CHECK: addu_s.qb       $t5, $a0,  $a1   # encoding: [0xa4,0x20,0xcd,0x1c]
  subu.qb         $t5, $t4, $at   # CHECK: subu.qb         $t5, $t4,  $at   # encoding: [0x22,0x20,0xcd,0x1a]
  subu_s.qb       $t5, $a5, $a4   # CHECK: subu_s.qb       $t5, $a5,  $a4   # encoding: [0x09,0x21,0xcd,0x1e]
  addq.ph         $a5, $a3, $a0   # CHECK: addq.ph         $a5, $a3,  $a0   # encoding: [0x87,0x20,0x0d,0x48]
  addq_s.ph       $at, $t4, $t5   # CHECK: addq_s.ph       $at, $t4,  $t5   # encoding: [0x62,0x20,0x0d,0x0c]
  subq.ph         $a2, $a0, $t4   # CHECK: subq.ph         $a2, $a0,  $t4   # encoding: [0x44,0x20,0x0d,0x32]
  subq_s.ph       $a1, $t5, $at   # CHECK: subq_s.ph       $a1, $t5,  $at   # encoding: [0x23,0x20,0x0d,0x2e]
  mul.ph          $a1, $t5, $at   # CHECK: mul.ph          $a1, $t5,  $at   # encoding: [0x23,0x20,0x2d,0x28]
  mul_s.ph        $a7, $sp, $zero # CHECK: mul_s.ph        $a7, $sp,  $zero # encoding: [0x1d,0x20,0x2d,0x5c]
  precrq.ph.w     $at, $t5, $a1   # CHECK: precrq.ph.w     $at, $t5,  $a1   # encoding: [0xa3,0x20,0xed,0x08]
  shll.qb         $t4, $a1,  7    # CHECK: shll.qb         $t4, $a1,  7     # encoding: [0x45,0x20,0x7f,0xe8]
  shrl.qb         $t4, $a2,  6    # CHECK: shrl.qb         $t4, $a2,  6     # encoding: [0x46,0x20,0x7f,0xd8]
  shrl.ph         $s5, $t0, 15    # CHECK: shrl.ph         $s5, $t0, 15     # encoding: [0xac,0x22,0xff,0xf3]
  shll.ph         $ra, $sp, 13    # CHECK: shll.ph         $ra, $sp, 13     # encoding: [0xfd,0x23,0xb5,0xd3]
  shll_s.ph       $t3, $s1,  3    # CHECK: shll_s.ph       $t3, $s1,  3     # encoding: [0xf1,0x21,0xb5,0x3b]
# 0b001000 0b01010 0b01110  0b10010 0bx 0b0011000 0b101
  cmpgu.eq.qb     $s2, $t2, $a6   # CHECK: cmpgu.eq.qb     $s2, $t2, $a6    # encoding: [0x4e,0x21,0xc5,0x90]
# 0b001000 0b10000 0b10011 0b10100 0bx 0b0100000 0b101
  cmpgu.lt.qb     $s4, $s3, $s0   # CHECK: cmpgu.lt.qb     $s4, $s3, $s0    # encoding: [0x13,0x22,0x05,0xa1]
# 0b001000 0b11110 0b11101 0b11100 0bx 0b0101000 0b101
  cmpgu.le.qb     $gp, $sp, $fp   # CHECK: cmpgu.le.qb     $gp, $sp, $fp    # encoding: [0xdd,0x23,0x45,0xe1]
# 0b001000 0b11100 0b11101 0bxxxxxx 0b0000000 0b101
  cmp.eq.ph       $sp, $gp        # CHECK: cmp.eq.ph       $sp, $gp         # encoding: [0x9d,0x23,0x05,0x00]
# 0b001000 0b11110 0b00011 0bxxxxxx 0b0010000 0b101
  cmp.le.ph       $t5, $fp        # CHECK: cmp.le.ph       $t5, $fp         # encoding: [0xc3,0x23,0x85,0x00]
# 0b001000 0b10110 0b01110 0bxxxxxx 0b0001000 0b101
  cmp.lt.ph       $t2, $s6        # CHECK: cmp.lt.ph       $t2, $s6         # encoding: [0xce,0x22,0x45,0x00]
# 0b001000 0b00111 0b00110 0b01 0b00 0b101 0b010 0b111 0b111
  madd            $ac1, $a2, $a3  # CHECK: madd            $ac1, $a2, $a3   # encoding: [0xe6,0x20,0xbf,0x4a]
# 0b001000 0b01110 0bxxxxx 0b01 0b00 0b000 0b001 0b111 0b111
  mfhi            $t2, $ac1       # CHECK: mfhi            $t2, $ac1        # encoding: [0xc0,0x21,0x7f,0x40]
# 0b001000 0b01111 0bxxxxx 0b00 0b01 0b000 0b001 0b111 0b111
  mflo             $t3, $ac0      # CHECK: mflo            $t3, $ac0        # encoding: [0xe0,0x21,0x7f,0x10]
# 0b001000 0bxxxxx 0b10000 0b11 0b10 0b000 0b001 0b111 0b111
# 0b00100000 ,0b00010000 ,0b11100000 ,0b01111111
  mthi              $s0, $ac3       # CHECK: mthi               $s0, $ac3       # encoding: [0x10,0x20,0x7f,0xe0]
# 0b001000 0bxxxxx 0b10001 0b10 0b11 0b000 0b001 0b111 0b111
# 0b00100000 ,0b00010001 ,0b10110000 ,0b01111111
  mtlo              $s1, $ac2       # CHECK: mtlo               $s1, $ac2       # encoding: [0x11,0x20,0x7f,0xb0]
# 0b001000 0b10111 0b01101 0b00011 0bx 0b0111101 0b101
  pick.qb         $t5, $t1, $s7   # CHECK: pick.qb         $t5, $t1, $s7    # encoding: [0xed,0x22,0xed,0x19]
# 0b001000 0b00111 0b10001 0b11011 0bx 0b1000101 0b101
  pick.ph         $k1, $s1, $a3   # CHECK: pick.ph         $k1, $s1, $a3    # encoding: [0xf1,0x20,0x2d,0xda]
  rddsp           $s2, 69         # CHECK: rddsp           $s2, 69          # encoding: [0x51,0x22,0x7f,0x46]
# 0b001000 0b01010 0b1100011 0b01 0b011 0b001 0b111 0b111
  wrdsp           $a6, 99         # CHECK: wrdsp           $a6, 99          # encoding: [0x58,0x21,0x7f,0xd6]
