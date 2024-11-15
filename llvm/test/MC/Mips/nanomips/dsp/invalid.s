# RUN: not llvm-mc %s -arch=nanomips -show-encoding -mattr=dsp 2>%t1
# RUN: FileCheck %s < %t1

  shll.ph $3, $4, 16       # CHECK: :[[@LINE]]:19: error: expected 4-bit unsigned immediate
  shll.ph $3, $4, -1       # CHECK: :[[@LINE]]:19: error: expected 4-bit unsigned immediate
  shll_s.ph $3, $4, 16     # CHECK: :[[@LINE]]:21: error: expected 4-bit unsigned immediate
  shll_s.ph $3, $4, -1     # CHECK: :[[@LINE]]:21: error: expected 4-bit unsigned immediate
  shll.qb $3, $4, 8        # CHECK: :[[@LINE]]:19: error: expected 3-bit unsigned immediate
  shll.qb $3, $4, -1       # CHECK: :[[@LINE]]:19: error: expected 3-bit unsigned immediate
  shrl.qb $3, $4, 8        # CHECK: :[[@LINE]]:19: error: expected 3-bit unsigned immediate
  shrl.qb $3, $4, -1       # CHECK: :[[@LINE]]:19: error: expected 3-bit unsigned immediate
  rddsp $2, -1             # CHECK: :[[@LINE]]:13: error: expected 7-bit unsigned immediate
  rddsp $2, 1024           # CHECK: :[[@LINE]]:13: error: expected 7-bit unsigned immediate
  wrdsp $5, -1             # CHECK: :[[@LINE]]:13: error: expected 7-bit unsigned immediate
  wrdsp $5, 1024           # CHECK: :[[@LINE]]:13: error: expected 7-bit unsigned immediate
  shrl.ph $3, $4, 16       # CHECK: :[[@LINE]]:19: error: expected 4-bit unsigned immediate
  shrl.ph $3, $4, -1       # CHECK: :[[@LINE]]:19: error: expected 4-bit unsigned immediate
  balign $a0, $a1, 4       # CHECK: :[[@LINE]]:20: error: expected 2-bit unsigned immediate
  prepend $a0, $a1, 32     # CHECK: :[[@LINE]]:21: error: expected 5-bit unsigned immediate
