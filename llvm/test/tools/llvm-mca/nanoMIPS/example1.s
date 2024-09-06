# RUN: llvm-mca -mtriple=nanomips -mcpu=i7200 -iterations=300 -enable-misched -misched-postra < %s | FileCheck %s

	.text
	.linkrelax
	.module	softfloat
	.module	pcrel
	.file	"primer1.c"
	.globl	f                               # -- Begin function f
	.p2align	1
	.type	f,@function
	.ent	f
f:                                      # @f
	.frame	$sp,16,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# %bb.0:
	addiu	$sp, $sp, -16
	sw	$a0, 12($sp)
	move	$a0, $zero
	sw	$a0, 4($sp)
	li	$a0, 1
	sw	$a0, 8($sp)
	bc	.LBB0_1
.LBB0_1:                                # =>This Inner Loop Header: Depth=1
	lw	$a0, 8($sp)
	lw	$a1, 12($sp)
	bgec	$a0, $a1, .LBB0_4
	bc	.LBB0_2
.LBB0_2:                                #   in Loop: Header=BB0_1 Depth=1
	lw	$a1, 8($sp)
	lw	$a0, 4($sp)
	addu	$a0, $a0, $a1
	sw	$a0, 4($sp)
	bc	.LBB0_3
.LBB0_3:                                #   in Loop: Header=BB0_1 Depth=1
	lw	$a0, 8($sp)
	addiu	$a0, $a0, 1
	sw	$a0, 8($sp)
	bc	.LBB0_1
.LBB0_4:
	lw	$a0, 4($sp)
	addiu	$sp, $sp, 16
	jrc	$ra
	.set	at
	.set	macro
	.set	reorder
	.end	f
.Lfunc_end0:
	.size	f, .Lfunc_end0-f
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	1
	.type	main,@function
	.ent	main
main:                                   # @main
	.frame	$sp,16,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# %bb.0:
	addiu	$sp, $sp, -16
	sw	$ra, 12($sp)                    # 4-byte Folded Spill
	move	$a0, $zero
	sw	$a0, 4($sp)                     # 4-byte Folded Spill
	sw	$a0, 8($sp)
	lapc.b	$a0, .L.str
	balc	printf
	li	$a0, 25
	balc	f
	move	$a1, $a0
	lapc.b	$a0, .L.str.1
	balc	printf
                                        # kill: def $a1_nm killed $a0_nm
	lw	$a0, 4($sp)                     # 4-byte Folded Reload
	lw	$ra, 12($sp)                    # 4-byte Folded Reload
	addiu	$sp, $sp, 16
	jrc	$ra
	.set	at
	.set	macro
	.set	reorder
	.end	main
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.4,"aMS",@progbits,1
	.p2align	2
.L.str:
	.asciz	"Hello world!\n"
	.size	.L.str, 14

	.type	.L.str.1,@object                # @.str.1
	.p2align	2
.L.str.1:
	.asciz	"%d\n"
	.size	.L.str.1, 4

	.ident	"clang version 13.0.0 (https://github.com/MediaTek-Labs/llvm-project.git 591da1c539e7fb4a859b47668b4445d963f6f4f1)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym f
	.addrsig_sym printf
	.text

# CHECK:      Iterations:        300
# CHECK-NEXT: Instructions:      11700
# CHECK-NEXT: Total Cycles:      96902
# CHECK-NEXT: Total uOps:        11700

# CHECK:      Dispatch Width:    2
# CHECK-NEXT: uOps Per Cycle:    0.12
# CHECK-NEXT: IPC:               0.12
# CHECK-NEXT: Block RThroughput: 19.5


# CHECK:      Instruction Info:
# CHECK-NEXT: [1]: #uOps
# CHECK-NEXT: [2]: Latency
# CHECK-NEXT: [3]: RThroughput
# CHECK-NEXT: [4]: MayLoad
# CHECK-NEXT: [5]: MayStore
# CHECK-NEXT: [6]: HasSideEffects (U)

# CHECK:      [1]    [2]    [3]    [4]    [5]    [6]    Instructions:
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, -16
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 12($sp)
# CHECK-NEXT:  1      1     0.50                  U     move	$a0, $zero
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 4($sp)
# CHECK-NEXT:  1      1     0.50                  U     li	$a0, 1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 8($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB0_1
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 8($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 12($sp)
# CHECK-NEXT:  1      1     1.00                        bgec	$a0, $a1, .LBB0_4
# CHECK-NEXT:  1      1     1.00                        bc	.LBB0_2
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 8($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 4($sp)
# CHECK-NEXT:  1      1     0.50                  U     addu	$a0, $a0, $a1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 4($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB0_3
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 8($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$a0, $a0, 1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 8($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB0_1
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 4($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, 16
# CHECK-NEXT:  1      1     1.00                  U     jrc	$ra
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, -16
# CHECK-NEXT:  1      1     1.00                  U     sw	$ra, 12($sp)
# CHECK-NEXT:  1      1     0.50                  U     move	$a0, $zero
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 4($sp)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 8($sp)
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str
# CHECK-NEXT:  1      1     1.00                        balc	printf
# CHECK-NEXT:  1      1     0.50                  U     li	$a0, 25
# CHECK-NEXT:  1      1     1.00                        balc	f
# CHECK-NEXT:  1      1     0.50                  U     move	$a1, $a0
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str.1
# CHECK-NEXT:  1      1     1.00                        balc	printf
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 4($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$ra, 12($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, 16
# CHECK-NEXT:  1      1     1.00                  U     jrc	$ra


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
# CHECK-NEXT:  -     16.00  6.50   6.50   10.00  10.00   -     16.00  

# CHECK:      Resource pressure by instruction:
# CHECK-NEXT: [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    Instructions:
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     addiu	$sp, $sp, -16
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 12($sp)
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     move	$a0, $zero
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 4($sp)
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     li	$a0, 1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 8($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB0_1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 8($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 12($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bgec	$a0, $a1, .LBB0_4
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB0_2
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 8($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 4($sp)
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     addu	$a0, $a0, $a1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 4($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB0_3
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 8($sp)
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     addiu	$a0, $a0, 1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 8($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB0_1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 4($sp)
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     addiu	$sp, $sp, 16
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     jrc	$ra
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     addiu	$sp, $sp, -16
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$ra, 12($sp)
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     move	$a0, $zero
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 4($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 8($sp)
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     lapc.b	$a0, .L.str
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printf
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     li	$a0, 25
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	f
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     move	$a1, $a0
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     lapc.b	$a0, .L.str.1
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printf
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 4($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$ra, 12($sp)
# CHECK-NEXT:  -      -     0.50   0.50    -      -      -      -     addiu	$sp, $sp, 16
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     jrc	$ra
