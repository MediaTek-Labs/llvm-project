# RUN: llvm-mca -mtriple=nanomips -mcpu=i7200 -iterations=300 -enable-misched -misched-postra < %s | FileCheck %s

break 5
cache 0, 0($a1)
deret
di $a0
ehb
ei $a1
eret
eretnc
extw $a0, $a1, $a2, 5
ginvi $a0
ginvt $a3, 2
mfc0 $a4, $3, 2
mfhc0 $a4, $3, 2
mtc0 $a4, $3, 2
mthc0 $a4, $3, 2
pause
pref 0, 1($a5)
rdpgpr $a0, $a1
sdbbp 2
sigrie 2
sync
synci 5($a6)
syscall 123
tlbinv
tlbinvf
tlbp
tlbr
tlbwi
tlbwr
wait
wrpgpr $a0, $a1


# CHECK:      Iterations:        300
# CHECK-NEXT: Instructions:      9300
# CHECK-NEXT: Total Cycles:      12901
# CHECK-NEXT: Total uOps:        9300

# CHECK:      Dispatch Width:    2
# CHECK-NEXT: uOps Per Cycle:    0.72
# CHECK-NEXT: IPC:               0.72
# CHECK-NEXT: Block RThroughput: 43.0


# CHECK:      Instruction Info:
# CHECK-NEXT: [1]: #uOps
# CHECK-NEXT: [2]: Latency
# CHECK-NEXT: [3]: RThroughput
# CHECK-NEXT: [4]: MayLoad
# CHECK-NEXT: [5]: MayStore
# CHECK-NEXT: [6]: HasSideEffects (U)

# CHECK:      [1]    [2]    [3]    [4]    [5]    [6]    Instructions:
# CHECK-NEXT:  1      1     1.00                  U     break	5
# CHECK-NEXT:  1      2     1.00                  U     cache	0, 0($a1)
# CHECK-NEXT:  1      1     1.00                  U     deret
# CHECK-NEXT:  1      2     2.00                  U     di	$a0
# CHECK-NEXT:  1      2     2.00                  U     ehb
# CHECK-NEXT:  1      2     2.00                  U     ei	$a1
# CHECK-NEXT:  1      1     1.00                  U     eret
# CHECK-NEXT:  1      1     1.00                  U     eretnc
# CHECK-NEXT:  1      1     0.50                  U     extw	$a0, $a1, $a2, 5
# CHECK-NEXT:  1      1     1.00                  U     ginvi	$a0
# CHECK-NEXT:  1      2     2.00                  U     ginvt	$a3, 2
# CHECK-NEXT:  1      2     2.00                  U     mfc0	$a4, $3, 2
# CHECK-NEXT:  1      2     2.00                  U     mfhc0	$a4, $3, 2
# CHECK-NEXT:  1      2     2.00                  U     mtc0	$a4, $3, 2
# CHECK-NEXT:  1      2     2.00                  U     mthc0	$a4, $3, 2
# CHECK-NEXT:  1      2     2.00                  U     pause
# CHECK-NEXT:  1      2     1.00                  U     pref	0, 1($a5)
# CHECK-NEXT:  1      1     0.50                  U     rdpgpr	$a0, $a1
# CHECK-NEXT:  1      1     1.00                  U     sdbbp	2
# CHECK-NEXT:  1      1     1.00                  U     sigrie	2
# CHECK-NEXT:  1      1     1.00                  U     sync
# CHECK-NEXT:  1      2     1.00                  U     synci	5($a6)
# CHECK-NEXT:  1      1     1.00                  U     syscall	123
# CHECK-NEXT:  1      2     2.00                  U     tlbinv
# CHECK-NEXT:  1      2     2.00                  U     tlbinvf
# CHECK-NEXT:  1      2     2.00                  U     tlbp
# CHECK-NEXT:  1      2     2.00                  U     tlbr
# CHECK-NEXT:  1      2     2.00                  U     tlbwi
# CHECK-NEXT:  1      2     2.00                  U     tlbwr
# CHECK-NEXT:  1      2     2.00                  U     wait
# CHECK-NEXT:  1      1     0.50                  U     wrpgpr	$a0, $a1


# CHECK:      Resources:
# CHECK-NEXT: [0]   - I7200Atomic
# CHECK-NEXT: [1]   - i7200Agen
# CHECK-NEXT: [2]   - i7200Alu0
# CHECK-NEXT: [3]   - i7200Alu1
# CHECK-NEXT: [4]   - i7200Control
# CHECK-NEXT: [5]   - i7200Ctu
# CHECK-NEXT: [6]   - i7200GpMulDiv
# CHECK-NEXT: [7]   - i7200Lsu


# CHECK:      Resource pressure per iteration:
# CHECK-NEXT: [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    
# CHECK-NEXT:  -     3.00   1.50   1.50   43.00  7.00    -     4.00   

# CHECK:      Resource pressure by instruction:
# CHECK-NEXT: [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    Instructions:
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     break	5
# CHECK-NEXT:  -     1.00    -      -     1.00    -      -     1.00   cache	0, 0($a1)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     deret
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     di	$a0
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     ehb
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     ei	$a1
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     eret
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     eretnc
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     extw	$a0, $a1, $a2, 5
# CHECK-NEXT:  -      -      -      -     1.00    -      -      -     ginvi	$a0
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     ginvt	$a3, 2
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     mfc0	$a4, $3, 2
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     mfhc0	$a4, $3, 2
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     mtc0	$a4, $3, 2
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     mthc0	$a4, $3, 2
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     pause
# CHECK-NEXT:  -     1.00    -      -     1.00    -      -     1.00   pref	0, 1($a5)
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     rdpgpr	$a0, $a1
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     sdbbp	2
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     sigrie	2
# CHECK-NEXT:  -      -      -      -      -      -      -     1.00   sync
# CHECK-NEXT:  -     1.00    -      -     1.00    -      -     1.00   synci	5($a6)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     syscall	123
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     tlbinv
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     tlbinvf
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     tlbp
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     tlbr
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     tlbwi
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     tlbwr
# CHECK-NEXT:  -      -      -      -     2.00    -      -      -     wait
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     wrpgpr	$a0, $a1
