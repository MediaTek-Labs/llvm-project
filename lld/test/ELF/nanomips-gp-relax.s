# REQUIRES: nanomips

# RUN: llvm-mc -filetype=obj -mcpu=i7200 -triple=nanomips-elf -mattr=+pcrel \
# RUN: %s -o %t.o

# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld --expand-reg 6  --relax %t.o \
# RUN: -o %t

# RUN: llvm-objdump --triple=nanomips-elf --mcpu=i7200 -d %t | FileCheck %s \
# RUN: --check-prefix=CHECK-NMF-STRICT

# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld --expand-reg 6 --relax \
# RUN: --no-strict-address-modes %t.o -o %t

# RUN: llvm-objdump --triple=nanomips-elf --mcpu=i7200 -d %t | FileCheck %s \
# RUN: --check-prefix=CHECK-NMF-NO-STRICT

# Differences from now on (between strict and no-strict address modes,
# nmf and nms)

# TODO: nanoMIPS subset ISA not yet implemented, add checks for subset
# when the subset is implemented. Full (nmf) and Subset (nms) are not
# the same. Maybe needs some changes then as well.



    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start



_start:
    # CHECK-NMF-STRICT: bf 40 fe ff lw $a1, 0x1ffffc($gp)
    lw $a1, %gp_rel(no_expand_19_s2)($gp)
    
    # CHECK-NMF-STRICT-NEXT: c2 60 fe ff 1f 00 addiu.b32 $a2, $gp, 0x1ffffe
    # CHECK-NMF-STRICT-NEXT: a6 84 00 80 lw $a1, 0x0($a2)
    # CHECK-NMF-NO-STRICT: ab 60 f4 1f 1b 00 lwpc $a1, 0x201ffe
    # Should be expanded as no_expand_19_s2 + 2 is not a multiple of 4 
    lw $a1, %gp_rel(no_expand_19_s2 + 2)($gp)

    # CHECK-NMF-STRICT-NEXT: c2 60 00 00 20 00 addiu.b32 $a2, $gp, 0x200000
    # CHECK-NMF-STRICT-NEXT: a6 84 00 80 lw $a1, 0x0($a2)
    # CHECK-NMF-NO-STRICT-NEXT: ab 60 f0 1f 1b 00 lwpc $a1, 0x202000
    lw $a1, %gp_rel(a)($gp)
    # CHECK-NMF-STRICT-NEXT: c2 60 00 10 20 00 addiu.b32 $a2, $gp, 0x201000
    # CHECK-NMF-STRICT-NEXT: a6 84 00 90 sw $a1, 0x0($a2)
    # CHECK-NMF-NO-STRICT-NEXT: af 60 ea 2f 1b 00 swpc $a1, 0x203000
    sw $a1, %gp_rel(b)($gp)
    # CHECK-NMF-STRICT-NEXT: a2 60 00 10 20 00 addiu.b32 $a1, $gp, 0x201000
    addiu.w $a1, $gp, %gp_rel(b)

    # CHECK-NMF-STRICT-NEXT: a3 44 ff ff lb $a1, 0x3ffff($gp)
    lb $a1, %gp_rel(in_range_18)($gp)
    # CHECK-NMF-STRICT-NEXT: c2 60 02 00 04 00 addiu.b32 $a2, $gp, 0x40002
    # CHECK-NMF-STRICT-NEXT: a6 84 00 00 lb $a1, 0x0($a2)
    lb $a1, %gp_rel(out_range_18)($gp)
    # CHECK-NMF-STRICT-NEXT: c2 60 02 00 04 00 addiu.b32 $a2, $gp, 0x40002
    # CHECK-NMF-STRICT-NEXT: a6 84 00 10 sb $a1, 0x0($a2)
    sb $a1, %gp_rel(out_range_18)($gp)
    # CHECK-NMF-STRICT-NEXT: c2 60 02 00 04 00 addiu.b32 $a2, $gp, 0x40002
    # CHECK-NMF-STRICT-NEXT: a6 84 00 20 lbu $a1, 0x0($a2)
    lbu $a1, %gp_rel(out_range_18)($gp)
    # CHECK-NMF-STRICT-NEXT: addiu.w $a1, $gp, 0x40004
    addiu.b $a1, $gp, %gp_rel(out_range_18 + 2)
    # CHECK-NMF-STRICT-NEXT: a2 60 02 00 04 00 addiu.b32 $a1, $gp, 0x40002
    addiu.b $a1, $gp, %gp_rel(out_range_18)
    # CHECK-NMF-STRICT-NEXT: a2 60 00 10 20 00 addiu.b32 $a1, $gp, 0x201000
    addiu.b $a1, $gp, %gp_rel(b)

    # CHECK-NMF-STRICT-NEXT: b3 44 fe ff lh $a1, 0x3fffe($gp)
    lh $a1, %gp_rel(in_range_18 - 1)($gp)
    # CHECK-NMF-STRICT-NEXT: cf 44 ff ff addiu.b $a2, $gp, 0x3ffff
    # CHECK-NMF-STRICT-NEXT: a6 84 00 40 lh $a1, 0x0($a2)
    # Should expand this one as it is not aligned on 2
    lh $a1, %gp_rel(in_range_18)($gp)
    # CHECK-NMF-STRICT-NEXT: c2 60 02 00 04 00 addiu.b32 $a2, $gp, 0x40002
    # CHECK-NMF-STRICT-NEXT: a6 84 00 60 lhu $a1, 0x0($a2)
    lhu $a1, %gp_rel(out_range_18)($gp)
    # CHECK-NMF-STRICT-NEXT: c2 60 02 00 04 00 addiu.b32 $a2, $gp, 0x40002
    # CHECK-NMF-STRICT-NEXT: a6 84 00 50 sh $a1, 0x0($a2)
    sh $a1, %gp_rel(out_range_18)($gp)

    # Relax (after expanding lw, sw)

    # CHECK-NMF-STRICT-NEXT: ff 56 lw $a1, 0x1fc($gp)
    lw $a1, %gp_rel(gprel9_relax)($gp)
    # CHECK-NMF-STRICT-NEXT: ff d6 sw $a1, 0x1fc($gp)
    sw $a1, %gp_rel(gprel9_relax)($gp)
    # CHECK-NMF-STRICT-NEXT: 80 41 fe 01 lw $t0, 0x1fc($gp)
    # No relax, not valid reg
    lw $t0, %gp_rel(gprel9_relax)($gp)

    # CHECK-NMF-STRICT-NEXT: af 44 fe ff addiu.b $a1, $gp, 0x3fffe
    # Expand then relax
    addiu.b $a1, $gp, %gp_rel(gprel_i32_18_expand_relax_sym)
    # CHECK-NMF-STRICT-NEXT: a2 60 01 00 04 00 addiu.b32 $a1, $gp, 0x40001
    # Expand no relax
    addiu.b $a1, $gp, %gp_rel(gprel_i32_18_expand_relax_sym + 3)
    # CHECK-NMF-STRICT-NEXT: bf 40 fc ff addiu.w $a1, $gp, 0x1ffffc
    # Expand then relax
    addiu.w $a1, $gp, %gp_rel(gprel_i32_21_expand_relax_sym)
    # CHECK-NMF-STRICT-NEXT: a2 60 fa ff 1f 00 addiu.b32 $a1, $gp, 0x1ffffa
    # Expand no relax
    addiu.w $a1, $gp, %gp_rel(gprel_i32_21_expand_relax_sym-2)
    # CHECK-NMF-STRICT-NEXT: a2 60 00 00 20 00 addiu.b32 $a1, $gp, 0x200000
    # Expand no relax
    addiu.w $a1, $gp, %gp_rel(gprel_i32_21_expand_relax_sym+4)

    .end _start
    .size _start, .-_start


    .section .gprel9_relax_sec, "ax", @progbits
    .globl gprel9_relax_fun
    .ent gprel9_relax_fun

gprel9_relax_fun:
    lapc.b $a1, lapc_close
    lapc.b $a1, lapc_close
gprel9_relax:
    .skip 4
gprel9_expand:


    .end gprel9_relax_fun
    .size gprel9_relax_fun, .-gprel9_relax_fun

    .section .lapc_gprel_i32_18_sec, "ax", @progbits
    .globl lapc_gprel_i32_18_fun
    .ent lapc_gprel_i32_18_fun

lapc_gprel_i32_18_fun:
    lapc $a1, lapc_far

lapc_gprel_i32_18_end:
lapc_gprel_i32_21_end:

    .section .gprel_i32_18_sec, "ax", @progbits
    .globl gprel_i32_18_fun
    .ent gprel_i32_18_fun

gprel_i32_18_fun:
    lapc.b $a1, lapc_gprel_i32_18_end - 0x1c1002

gprel_i32_18_expand_relax_sym:

    .end gprel_i32_18_fun
    .size gprel_i32_18_fun, .-gprel_i32_18_fun

    .section .gprel_i32_21_sec, "ax", @progbits
    .globl gprel_i32_21_fun
    .ent gprel_i32_21_fun

gprel_i32_21_fun:
    lapc.b $a1, lapc_gprel_i32_21_end - 0x1004

gprel_i32_21_expand_relax_sym:

    .end gprel_i32_21_fun
    .size gprel_i32_21_fun, .-gprel_i32_21_fun


