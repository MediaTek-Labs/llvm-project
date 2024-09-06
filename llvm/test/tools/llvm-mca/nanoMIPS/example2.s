# RUN: llvm-mca -mtriple=nanomips -mcpu=i7200 -iterations=300 -enable-misched -misched-postra < %s | FileCheck %s

	.text
	.linkrelax
	.module	softfloat
	.module	pcrel
	.file	"primer4.c"
	.globl	fillArray                       # -- Begin function fillArray
	.p2align	1
	.type	fillArray,@function
	.ent	fillArray
fillArray:                              # @fillArray
	.frame	$sp,16,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# %bb.0:
	addiu	$sp, $sp, -16
	sw	$ra, 12($sp)                    # 4-byte Folded Spill
	sw	$a0, 8($sp)
	sw	$a1, 4($sp)
	move	$a0, $zero
	sw	$a0, 0($sp)
	bc	.LBB0_1
.LBB0_1:                                # =>This Inner Loop Header: Depth=1
	lw	$a0, 0($sp)
	lw	$a1, 4($sp)
	bgec	$a0, $a1, .LBB0_4
	bc	.LBB0_2
.LBB0_2:                                #   in Loop: Header=BB0_1 Depth=1
	balc	rand
	li	$a1, 274877907
	muh	$a1, $a0, $a1
	srl	$a2, $a1, 31
	sra	$a1, $a1, 6
	addu	$a2, $a1, $a2
	sll	$a1, $a2, 4
	lsa	$a1, $a2, $a1, 3
	sll	$a2, $a2, 10
	subu	$a1, $a1, $a2
	addu	$a0, $a0, $a1
	lw	$a1, 8($sp)
	lw	$a2, 0($sp)
	swxs	$a0, $a2($a1)
	bc	.LBB0_3
.LBB0_3:                                #   in Loop: Header=BB0_1 Depth=1
	lw	$a0, 0($sp)
	addiu	$a0, $a0, 1
	sw	$a0, 0($sp)
	bc	.LBB0_1
.LBB0_4:
	lw	$ra, 12($sp)                    # 4-byte Folded Reload
	addiu	$sp, $sp, 16
	jrc	$ra
	.set	at
	.set	macro
	.set	reorder
	.end	fillArray
.Lfunc_end0:
	.size	fillArray, .Lfunc_end0-fillArray
                                        # -- End function
	.globl	printArray                      # -- Begin function printArray
	.p2align	1
	.type	printArray,@function
	.ent	printArray
printArray:                             # @printArray
	.frame	$sp,16,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# %bb.0:
	addiu	$sp, $sp, -16
	sw	$ra, 12($sp)                    # 4-byte Folded Spill
	sw	$a0, 8($sp)
	sw	$a1, 4($sp)
	move	$a0, $zero
	sw	$a0, 0($sp)
	bc	.LBB1_1
.LBB1_1:                                # =>This Inner Loop Header: Depth=1
	lw	$a0, 0($sp)
	lw	$a1, 4($sp)
	bgec	$a0, $a1, .LBB1_4
	bc	.LBB1_2
.LBB1_2:                                #   in Loop: Header=BB1_1 Depth=1
	lw	$a0, 8($sp)
	lw	$a1, 0($sp)
	lwxs	$a1, $a1($a0)
	lapc.b	$a0, .L.str.6
	balc	printf
	bc	.LBB1_3
.LBB1_3:                                #   in Loop: Header=BB1_1 Depth=1
	lw	$a0, 0($sp)
	addiu	$a0, $a0, 1
	sw	$a0, 0($sp)
	bc	.LBB1_1
.LBB1_4:
	lapc.b	$a0, .L.str.7
	balc	printf
	lw	$ra, 12($sp)                    # 4-byte Folded Reload
	addiu	$sp, $sp, 16
	jrc	$ra
	.set	at
	.set	macro
	.set	reorder
	.end	printArray
.Lfunc_end1:
	.size	printArray, .Lfunc_end1-printArray
                                        # -- End function
	.globl	sortArray                       # -- Begin function sortArray
	.p2align	1
	.type	sortArray,@function
	.ent	sortArray
sortArray:                              # @sortArray
	.frame	$sp,32,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# %bb.0:
	addiu	$sp, $sp, -32
	sw	$a0, 28($sp)
	sw	$a1, 24($sp)
	move	$a0, $zero
	sw	$a0, 20($sp)
	bc	.LBB2_1
.LBB2_1:                                # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_3 Depth 2
	lw	$a0, 20($sp)
	lw	$a1, 24($sp)
	addiu	$a1, $a1, -1
	bgec	$a0, $a1, .LBB2_10
	bc	.LBB2_2
.LBB2_2:                                #   in Loop: Header=BB2_1 Depth=1
	move	$a0, $zero
	sw	$a0, 16($sp)
	bc	.LBB2_3
.LBB2_3:                                #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	lw	$a0, 16($sp)
	lw	$a2, 24($sp)
	lw	$a1, 20($sp)
	not	$a1, $a1
	addu	$a1, $a1, $a2
	bgec	$a0, $a1, .LBB2_8
	bc	.LBB2_4
.LBB2_4:                                #   in Loop: Header=BB2_3 Depth=2
	lw	$a1, 28($sp)
	lw	$a2, 16($sp)
	lsa	$a0, $a2, $a1, 2
	lwxs	$a1, $a2($a1)
	lw	$a0, 4($a0)
	bgec	$a0, $a1, .LBB2_6
	bc	.LBB2_5
.LBB2_5:                                #   in Loop: Header=BB2_3 Depth=2
	lw	$a0, 28($sp)
	lw	$a1, 16($sp)
	lwxs	$a0, $a1($a0)
	sw	$a0, 12($sp)
	lw	$a1, 28($sp)
	lw	$a2, 16($sp)
	lsa	$a0, $a2, $a1, 2
	lw	$a0, 4($a0)
	swxs	$a0, $a2($a1)
	lw	$a0, 12($sp)
	lw	$a2, 28($sp)
	lw	$a1, 16($sp)
	lsa	$a1, $a1, $a2, 2
	sw	$a0, 4($a1)
	bc	.LBB2_6
.LBB2_6:                                #   in Loop: Header=BB2_3 Depth=2
	bc	.LBB2_7
.LBB2_7:                                #   in Loop: Header=BB2_3 Depth=2
	lw	$a0, 16($sp)
	addiu	$a0, $a0, 1
	sw	$a0, 16($sp)
	bc	.LBB2_3
.LBB2_8:                                #   in Loop: Header=BB2_1 Depth=1
	bc	.LBB2_9
.LBB2_9:                                #   in Loop: Header=BB2_1 Depth=1
	lw	$a0, 20($sp)
	addiu	$a0, $a0, 1
	sw	$a0, 20($sp)
	bc	.LBB2_1
.LBB2_10:
	addiu	$sp, $sp, 32
	jrc	$ra
	.set	at
	.set	macro
	.set	reorder
	.end	sortArray
.Lfunc_end2:
	.size	sortArray, .Lfunc_end2-sortArray
                                        # -- End function
	.globl	binarySearch                    # -- Begin function binarySearch
	.p2align	1
	.type	binarySearch,@function
	.ent	binarySearch
binarySearch:                           # @binarySearch
	.frame	$sp,32,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# %bb.0:
	addiu	$sp, $sp, -32
	sw	$a0, 24($sp)
	sw	$a1, 20($sp)
	sw	$a2, 16($sp)
	move	$a0, $zero
	sw	$a0, 12($sp)
	lw	$a0, 20($sp)
	addiu	$a0, $a0, -1
	sw	$a0, 8($sp)
	bc	.LBB3_1
.LBB3_1:                                # =>This Inner Loop Header: Depth=1
	lw	$a1, 12($sp)
	lw	$a0, 8($sp)
	bltc	$a0, $a1, .LBB3_8
	bc	.LBB3_2
.LBB3_2:                                #   in Loop: Header=BB3_1 Depth=1
	lw	$a0, 12($sp)
	lw	$a1, 8($sp)
	subu	$a1, $a1, $a0
	srl	$a2, $a1, 31
	addu	$a1, $a1, $a2
	sra	$a1, $a1, 1
	addu	$a0, $a0, $a1
	sw	$a0, 4($sp)
	lw	$a0, 24($sp)
	lw	$a1, 4($sp)
	lwxs	$a0, $a1($a0)
	lw	$a1, 16($sp)
	bnec	$a0, $a1, .LBB3_4
	bc	.LBB3_3
.LBB3_3:
	lw	$a0, 4($sp)
	sw	$a0, 28($sp)
	bc	.LBB3_9
.LBB3_4:                                #   in Loop: Header=BB3_1 Depth=1
	lw	$a0, 24($sp)
	lw	$a1, 4($sp)
	lwxs	$a0, $a1($a0)
	lw	$a1, 16($sp)
	bgec	$a0, $a1, .LBB3_6
	bc	.LBB3_5
.LBB3_5:                                #   in Loop: Header=BB3_1 Depth=1
	lw	$a0, 4($sp)
	addiu	$a0, $a0, 1
	sw	$a0, 12($sp)
	bc	.LBB3_7
.LBB3_6:                                #   in Loop: Header=BB3_1 Depth=1
	lw	$a0, 4($sp)
	addiu	$a0, $a0, -1
	sw	$a0, 8($sp)
	bc	.LBB3_7
.LBB3_7:                                #   in Loop: Header=BB3_1 Depth=1
	bc	.LBB3_1
.LBB3_8:
	li	$a0, -1
	sw	$a0, 28($sp)
	bc	.LBB3_9
.LBB3_9:
	lw	$a0, 28($sp)
	addiu	$sp, $sp, 32
	jrc	$ra
	.set	at
	.set	macro
	.set	reorder
	.end	binarySearch
.Lfunc_end3:
	.size	binarySearch, .Lfunc_end3-binarySearch
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	1
	.type	main,@function
	.ent	main
main:                                   # @main
	.frame	$sp,432,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# %bb.0:
	addiu	$sp, $sp, -432
	sw	$ra, 428($sp)                   # 4-byte Folded Spill
	move	$a0, $zero
	addiu	$a1, $sp, 424
	sw	$a0, 0($a1)
	balc	time
	balc	srand
	addiu	$a0, $sp, 24
	sw	$a0, 12($sp)                    # 4-byte Folded Spill
	li	$a1, 100
	sw	$a1, 8($sp)                     # 4-byte Folded Spill
	balc	fillArray
	lapc.b	$a0, .L.str
	balc	printf
	lw	$a1, 8($sp)                     # 4-byte Folded Reload
                                        # kill: def $a2_nm killed $a0_nm
	lw	$a0, 12($sp)                    # 4-byte Folded Reload
	balc	printArray
	lw	$a0, 12($sp)                    # 4-byte Folded Reload
	lw	$a1, 8($sp)                     # 4-byte Folded Reload
	balc	sortArray
	lapc.b	$a0, .L.str.1
	balc	printf
	lw	$a1, 8($sp)                     # 4-byte Folded Reload
                                        # kill: def $a2_nm killed $a0_nm
	lw	$a0, 12($sp)                    # 4-byte Folded Reload
	balc	printArray
	lapc.b	$a0, .L.str.2
	balc	printf
	lapc.b	$a0, .L.str.3
	addiu	$a1, $sp, 20
	balc	__isoc99_scanf
	lw	$a1, 8($sp)                     # 4-byte Folded Reload
                                        # kill: def $a2_nm killed $a0_nm
	lw	$a0, 12($sp)                    # 4-byte Folded Reload
	lw	$a2, 20($sp)
	balc	binarySearch
	sw	$a0, 16($sp)
	lw	$a0, 16($sp)
	li	$a1, -1
	beqc	$a0, $a1, .LBB4_2
	bc	.LBB4_1
.LBB4_1:
	lw	$a1, 20($sp)
	lw	$a2, 16($sp)
	lapc.b	$a0, .L.str.4
	balc	printf
	bc	.LBB4_3
.LBB4_2:
	lw	$a1, 20($sp)
	lapc.b	$a0, .L.str.5
	balc	printf
	bc	.LBB4_3
.LBB4_3:
	move	$a0, $zero
	lw	$ra, 428($sp)                   # 4-byte Folded Reload
	addiu	$sp, $sp, 432
	jrc	$ra
	.set	at
	.set	macro
	.set	reorder
	.end	main
.Lfunc_end4:
	.size	main, .Lfunc_end4-main
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.4,"aMS",@progbits,1
	.p2align	2
.L.str:
	.asciz	"Original Array:\n"
	.size	.L.str, 17

	.type	.L.str.1,@object                # @.str.1
	.p2align	2
.L.str.1:
	.asciz	"\nSorted Array:\n"
	.size	.L.str.1, 16

	.type	.L.str.2,@object                # @.str.2
	.p2align	2
.L.str.2:
	.asciz	"\nEnter a number to search for: "
	.size	.L.str.2, 32

	.type	.L.str.3,@object                # @.str.3
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.3:
	.asciz	"%d"
	.size	.L.str.3, 3

	.type	.L.str.4,@object                # @.str.4
	.section	.rodata.str1.4,"aMS",@progbits,1
	.p2align	2
.L.str.4:
	.asciz	"Number %d found at index %d.\n"
	.size	.L.str.4, 30

	.type	.L.str.5,@object                # @.str.5
	.p2align	2
.L.str.5:
	.asciz	"Number %d not found in the array.\n"
	.size	.L.str.5, 35

	.type	.L.str.6,@object                # @.str.6
	.p2align	2
.L.str.6:
	.asciz	"%d "
	.size	.L.str.6, 4

	.type	.L.str.7,@object                # @.str.7
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.7:
	.asciz	"\n"
	.size	.L.str.7, 2

	.ident	"clang version 13.0.0 (https://github.com/MediaTek-Labs/llvm-project.git 591da1c539e7fb4a859b47668b4445d963f6f4f1)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym srand
	.addrsig_sym time
	.addrsig_sym fillArray
	.addrsig_sym printf
	.addrsig_sym printArray
	.addrsig_sym sortArray
	.addrsig_sym __isoc99_scanf
	.addrsig_sym binarySearch
	.addrsig_sym rand
	.text

# CHECK:      Iterations:        300
# CHECK-NEXT: Instructions:      65400
# CHECK-NEXT: Total Cycles:      525902
# CHECK-NEXT: Total uOps:        65400

# CHECK:      Dispatch Width:    2
# CHECK-NEXT: uOps Per Cycle:    0.12
# CHECK-NEXT: IPC:               0.12
# CHECK-NEXT: Block RThroughput: 109.0


# CHECK:      Instruction Info:
# CHECK-NEXT: [1]: #uOps
# CHECK-NEXT: [2]: Latency
# CHECK-NEXT: [3]: RThroughput
# CHECK-NEXT: [4]: MayLoad
# CHECK-NEXT: [5]: MayStore
# CHECK-NEXT: [6]: HasSideEffects (U)

# CHECK:      [1]    [2]    [3]    [4]    [5]    [6]    Instructions:
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, -16
# CHECK-NEXT:  1      1     1.00                  U     sw	$ra, 12($sp)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 8($sp)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a1, 4($sp)
# CHECK-NEXT:  1      1     0.50                  U     move	$a0, $zero
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 0($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB0_1
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 0($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 4($sp)
# CHECK-NEXT:  1      1     1.00                        bgec	$a0, $a1, .LBB0_4
# CHECK-NEXT:  1      1     1.00                        bc	.LBB0_2
# CHECK-NEXT:  1      1     1.00                        balc	rand
# CHECK-NEXT:  1      1     0.50                        li	$a1, 274877907
# CHECK-NEXT:  1      5     1.00                        muh	$a1, $a0, $a1
# CHECK-NEXT:  1      1     0.50                        srl	$a2, $a1, 31
# CHECK-NEXT:  1      1     0.50                        sra	$a1, $a1, 6
# CHECK-NEXT:  1      1     0.50                  U     addu	$a2, $a1, $a2
# CHECK-NEXT:  1      1     0.50                  U     sll	$a1, $a2, 4
# CHECK-NEXT:  1      1     0.50                        lsa	$a1, $a2, $a1, 3
# CHECK-NEXT:  1      1     0.50                        sll	$a2, $a2, 10
# CHECK-NEXT:  1      1     0.50                  U     subu	$a1, $a1, $a2
# CHECK-NEXT:  1      1     0.50                  U     addu	$a0, $a0, $a1
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 8($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a2, 0($sp)
# CHECK-NEXT:  1      1     1.00           *            swxs	$a0, $a2($a1)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB0_3
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 0($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$a0, $a0, 1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 0($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB0_1
# CHECK-NEXT:  1      2     1.00                  U     lw	$ra, 12($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, 16
# CHECK-NEXT:  1      1     1.00                  U     jrc	$ra
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, -16
# CHECK-NEXT:  1      1     1.00                  U     sw	$ra, 12($sp)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 8($sp)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a1, 4($sp)
# CHECK-NEXT:  1      1     0.50                  U     move	$a0, $zero
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 0($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB1_1
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 0($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 4($sp)
# CHECK-NEXT:  1      1     1.00                        bgec	$a0, $a1, .LBB1_4
# CHECK-NEXT:  1      1     1.00                        bc	.LBB1_2
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 8($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 0($sp)
# CHECK-NEXT:  1      2     1.00                  U     lwxs	$a1, $a1($a0)
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str.6
# CHECK-NEXT:  1      1     1.00                        balc	printf
# CHECK-NEXT:  1      1     1.00                        bc	.LBB1_3
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 0($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$a0, $a0, 1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 0($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB1_1
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str.7
# CHECK-NEXT:  1      1     1.00                        balc	printf
# CHECK-NEXT:  1      2     1.00                  U     lw	$ra, 12($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, 16
# CHECK-NEXT:  1      1     1.00                  U     jrc	$ra
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, -32
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 28($sp)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a1, 24($sp)
# CHECK-NEXT:  1      1     0.50                  U     move	$a0, $zero
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 20($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_1
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 20($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 24($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$a1, $a1, -1
# CHECK-NEXT:  1      1     1.00                        bgec	$a0, $a1, .LBB2_10
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_2
# CHECK-NEXT:  1      1     0.50                  U     move	$a0, $zero
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 16($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_3
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 16($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a2, 24($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 20($sp)
# CHECK-NEXT:  1      1     0.50                        not	$a1, $a1
# CHECK-NEXT:  1      1     0.50                  U     addu	$a1, $a1, $a2
# CHECK-NEXT:  1      1     1.00                        bgec	$a0, $a1, .LBB2_8
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_4
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 28($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a2, 16($sp)
# CHECK-NEXT:  1      1     0.50                        lsa	$a0, $a2, $a1, 2
# CHECK-NEXT:  1      2     1.00                  U     lwxs	$a1, $a2($a1)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 4($a0)
# CHECK-NEXT:  1      1     1.00                        bgec	$a0, $a1, .LBB2_6
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_5
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 28($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 16($sp)
# CHECK-NEXT:  1      2     1.00                  U     lwxs	$a0, $a1($a0)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 12($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 28($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a2, 16($sp)
# CHECK-NEXT:  1      1     0.50                        lsa	$a0, $a2, $a1, 2
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 4($a0)
# CHECK-NEXT:  1      1     1.00           *            swxs	$a0, $a2($a1)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 12($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a2, 28($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 16($sp)
# CHECK-NEXT:  1      1     0.50                        lsa	$a1, $a1, $a2, 2
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 4($a1)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_6
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_7
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 16($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$a0, $a0, 1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 16($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_3
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_9
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 20($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$a0, $a0, 1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 20($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB2_1
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, 32
# CHECK-NEXT:  1      1     1.00                  U     jrc	$ra
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, -32
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 24($sp)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a1, 20($sp)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a2, 16($sp)
# CHECK-NEXT:  1      1     0.50                  U     move	$a0, $zero
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 12($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 20($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$a0, $a0, -1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 8($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB3_1
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 12($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 8($sp)
# CHECK-NEXT:  1      1     1.00                        bltc	$a0, $a1, .LBB3_8
# CHECK-NEXT:  1      1     1.00                        bc	.LBB3_2
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 12($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 8($sp)
# CHECK-NEXT:  1      1     0.50                  U     subu	$a1, $a1, $a0
# CHECK-NEXT:  1      1     0.50                        srl	$a2, $a1, 31
# CHECK-NEXT:  1      1     0.50                  U     addu	$a1, $a1, $a2
# CHECK-NEXT:  1      1     0.50                        sra	$a1, $a1, 1
# CHECK-NEXT:  1      1     0.50                  U     addu	$a0, $a0, $a1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 4($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 24($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 4($sp)
# CHECK-NEXT:  1      2     1.00                  U     lwxs	$a0, $a1($a0)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 16($sp)
# CHECK-NEXT:  1      1     1.00                  U     bnec	$a0, $a1, .LBB3_4
# CHECK-NEXT:  1      1     1.00                        bc	.LBB3_3
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 4($sp)
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 28($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB3_9
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 24($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 4($sp)
# CHECK-NEXT:  1      2     1.00                  U     lwxs	$a0, $a1($a0)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 16($sp)
# CHECK-NEXT:  1      1     1.00                        bgec	$a0, $a1, .LBB3_6
# CHECK-NEXT:  1      1     1.00                        bc	.LBB3_5
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 4($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$a0, $a0, 1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 12($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB3_7
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 4($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$a0, $a0, -1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 8($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB3_7
# CHECK-NEXT:  1      1     1.00                        bc	.LBB3_1
# CHECK-NEXT:  1      1     0.50                  U     li	$a0, -1
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 28($sp)
# CHECK-NEXT:  1      1     1.00                        bc	.LBB3_9
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 28($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, 32
# CHECK-NEXT:  1      1     1.00                  U     jrc	$ra
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, -432
# CHECK-NEXT:  1      1     1.00           *            sw	$ra, 428($sp)
# CHECK-NEXT:  1      1     0.50                  U     move	$a0, $zero
# CHECK-NEXT:  1      1     0.50                        addiu	$a1, $sp, 424
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 0($a1)
# CHECK-NEXT:  1      1     1.00                        balc	time
# CHECK-NEXT:  1      1     1.00                        balc	srand
# CHECK-NEXT:  1      1     0.50                  U     addiu	$a0, $sp, 24
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 12($sp)
# CHECK-NEXT:  1      1     0.50                  U     li	$a1, 100
# CHECK-NEXT:  1      1     1.00                  U     sw	$a1, 8($sp)
# CHECK-NEXT:  1      1     1.00                        balc	fillArray
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str
# CHECK-NEXT:  1      1     1.00                        balc	printf
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 8($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 12($sp)
# CHECK-NEXT:  1      1     1.00                        balc	printArray
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 12($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 8($sp)
# CHECK-NEXT:  1      1     1.00                        balc	sortArray
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str.1
# CHECK-NEXT:  1      1     1.00                        balc	printf
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 8($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 12($sp)
# CHECK-NEXT:  1      1     1.00                        balc	printArray
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str.2
# CHECK-NEXT:  1      1     1.00                        balc	printf
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str.3
# CHECK-NEXT:  1      1     0.50                  U     addiu	$a1, $sp, 20
# CHECK-NEXT:  1      1     1.00                        balc	__isoc99_scanf
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 8($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 12($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a2, 20($sp)
# CHECK-NEXT:  1      1     1.00                        balc	binarySearch
# CHECK-NEXT:  1      1     1.00                  U     sw	$a0, 16($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a0, 16($sp)
# CHECK-NEXT:  1      1     0.50                  U     li	$a1, -1
# CHECK-NEXT:  1      1     1.00                  U     beqc	$a1, $a0, .LBB4_2
# CHECK-NEXT:  1      1     1.00                        bc	.LBB4_1
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 20($sp)
# CHECK-NEXT:  1      2     1.00                  U     lw	$a2, 16($sp)
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str.4
# CHECK-NEXT:  1      1     1.00                        balc	printf
# CHECK-NEXT:  1      1     1.00                        bc	.LBB4_3
# CHECK-NEXT:  1      2     1.00                  U     lw	$a1, 20($sp)
# CHECK-NEXT:  1      1     0.50                        lapc.b	$a0, .L.str.5
# CHECK-NEXT:  1      1     1.00                        balc	printf
# CHECK-NEXT:  1      1     1.00                        bc	.LBB4_3
# CHECK-NEXT:  1      1     0.50                  U     move	$a0, $zero
# CHECK-NEXT:  1      2     1.00    *                   lw	$ra, 428($sp)
# CHECK-NEXT:  1      1     0.50                        addiu	$sp, $sp, 432
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
# CHECK-NEXT:  -     100.00 29.00  29.00  60.00  60.00  1.00   99.00  

# CHECK:      Resource pressure by instruction:
# CHECK-NEXT: [0]    [1]    [2]    [3]    [4]    [5]    [6]    [7]    Instructions:
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$sp, $sp, -16
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$ra, 12($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 8($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a1, 4($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     move	$a0, $zero
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 0($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB0_1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 0($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 4($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bgec	$a0, $a1, .LBB0_4
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB0_2
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	rand
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     li	$a1, 274877907
# CHECK-NEXT:  -     1.00    -      -      -      -     1.00    -     muh	$a1, $a0, $a1
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     srl	$a2, $a1, 31
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     sra	$a1, $a1, 6
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addu	$a2, $a1, $a2
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     sll	$a1, $a2, 4
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     lsa	$a1, $a2, $a1, 3
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     sll	$a2, $a2, 10
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     subu	$a1, $a1, $a2
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addu	$a0, $a0, $a1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 8($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a2, 0($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   swxs	$a0, $a2($a1)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB0_3
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 0($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$a0, $a0, 1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 0($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB0_1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$ra, 12($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$sp, $sp, 16
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     jrc	$ra
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$sp, $sp, -16
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$ra, 12($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 8($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a1, 4($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     move	$a0, $zero
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 0($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB1_1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 0($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 4($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bgec	$a0, $a1, .LBB1_4
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB1_2
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 8($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 0($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lwxs	$a1, $a1($a0)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     lapc.b	$a0, .L.str.6
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printf
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB1_3
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 0($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$a0, $a0, 1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 0($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB1_1
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     lapc.b	$a0, .L.str.7
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printf
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$ra, 12($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$sp, $sp, 16
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     jrc	$ra
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$sp, $sp, -32
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 28($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a1, 24($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     move	$a0, $zero
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 20($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 20($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 24($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$a1, $a1, -1
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bgec	$a0, $a1, .LBB2_10
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_2
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     move	$a0, $zero
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 16($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_3
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 16($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a2, 24($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 20($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     not	$a1, $a1
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addu	$a1, $a1, $a2
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bgec	$a0, $a1, .LBB2_8
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_4
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 28($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a2, 16($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     lsa	$a0, $a2, $a1, 2
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lwxs	$a1, $a2($a1)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 4($a0)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bgec	$a0, $a1, .LBB2_6
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_5
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 28($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 16($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lwxs	$a0, $a1($a0)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 12($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 28($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a2, 16($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     lsa	$a0, $a2, $a1, 2
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 4($a0)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   swxs	$a0, $a2($a1)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 12($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a2, 28($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 16($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     lsa	$a1, $a1, $a2, 2
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 4($a1)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_6
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_7
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 16($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$a0, $a0, 1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 16($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_3
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_9
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 20($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$a0, $a0, 1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 20($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB2_1
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$sp, $sp, 32
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     jrc	$ra
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$sp, $sp, -32
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 24($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a1, 20($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a2, 16($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     move	$a0, $zero
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 12($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 20($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$a0, $a0, -1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 8($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB3_1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 12($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 8($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bltc	$a0, $a1, .LBB3_8
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB3_2
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 12($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 8($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     subu	$a1, $a1, $a0
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     srl	$a2, $a1, 31
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addu	$a1, $a1, $a2
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     sra	$a1, $a1, 1
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addu	$a0, $a0, $a1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 4($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 24($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 4($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lwxs	$a0, $a1($a0)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 16($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bnec	$a0, $a1, .LBB3_4
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB3_3
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 4($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 28($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB3_9
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 24($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 4($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lwxs	$a0, $a1($a0)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 16($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bgec	$a0, $a1, .LBB3_6
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB3_5
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 4($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$a0, $a0, 1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 12($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB3_7
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 4($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$a0, $a0, -1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 8($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB3_7
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB3_1
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     li	$a0, -1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 28($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB3_9
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 28($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$sp, $sp, 32
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     jrc	$ra
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$sp, $sp, -432
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$ra, 428($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     move	$a0, $zero
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$a1, $sp, 424
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 0($a1)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	time
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	srand
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$a0, $sp, 24
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 12($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     li	$a1, 100
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a1, 8($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	fillArray
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     lapc.b	$a0, .L.str
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printf
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 8($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 12($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printArray
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 12($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 8($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	sortArray
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     lapc.b	$a0, .L.str.1
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printf
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 8($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 12($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printArray
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     lapc.b	$a0, .L.str.2
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printf
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     lapc.b	$a0, .L.str.3
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     addiu	$a1, $sp, 20
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	__isoc99_scanf
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 8($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 12($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a2, 20($sp)
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	binarySearch
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   sw	$a0, 16($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a0, 16($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     li	$a1, -1
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     beqc	$a1, $a0, .LBB4_2
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB4_1
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 20($sp)
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a2, 16($sp)
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     lapc.b	$a0, .L.str.4
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printf
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB4_3
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$a1, 20($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     lapc.b	$a0, .L.str.5
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     balc	printf
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     bc	.LBB4_3
# CHECK-NEXT:  -      -      -     1.00    -      -      -      -     move	$a0, $zero
# CHECK-NEXT:  -     1.00    -      -      -      -      -     1.00   lw	$ra, 428($sp)
# CHECK-NEXT:  -      -     1.00    -      -      -      -      -     addiu	$sp, $sp, 432
# CHECK-NEXT:  -      -      -      -     1.00   1.00    -      -     jrc	$ra