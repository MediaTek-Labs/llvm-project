# Test for relocations for CIE/FDE length in .eh_frame section
# Presence of relocations indicates an unusable .eh_frame that will cause
# llvm-dwarfdump to signal an error.

# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s -o %t
# RUN: llvm-readelf -r %t | FileCheck -check-prefix=CHECK-RELOC %s
# RUN: llvm-dwarfdump --eh-frame %t | FileCheck -check-prefix=CHECK-DWARF %s

	.text
	.linkrelax
	.globl	main
	.type	main,@function
	.ent	main
main:
	.cfi_startproc
	.cfi_personality 128, DW.ref.__gxx_personality_v0
	.cfi_lsda 0, .Lexception0
	.frame	$sp,32,$ra
# %bb.0:
	addiu	$sp, $sp, -32
	.cfi_def_cfa_offset 32
	sw	$ra, 28($sp)                    # 4-byte Folded Spill
	.cfi_offset 31, -4
	jr $ra
	.end	main
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2, 0x0
GCC_except_table0:
.Lexception0:
	.byte	0

# CHECK-DWARF: 00000000 00000018 00000000 CIE
# CHECK-DWARF: 0000001c 0000001c 00000020 FDE

# CHECK-RELOC-NOT: {{0+}}0 {{.+}} R_NANOMIPS_NEG {{0+}}0 .Ltmp{{[0-9]+}} - 4
# CHECK-RELOC-NOT: {{0+}}0 {{.+}} R_NANOMIPS_32 {{0+}}1c .Ltmp{{[0-9]+}} + 0
# CHECK-RELOC-NOT: {{0+}}1c {{.+}} R_NANOMIPS_NEG {{0+}}20 .Ltmp{{[0-9]+}} + 0
# CHECK-RELOC-NOT: {{0+}}1c {{.+}} R_NANOMIPS_32 {{0+}}3c .Ltmp{{[0-9]+}} + 0
