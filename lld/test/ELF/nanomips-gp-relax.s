# llvm-mc doesn't know how to parse %gprel
# REQUIRES: nanomips, nanomips-gnu

# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpcrel %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6 --relax
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMF-STRICT
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6 --no-strict-address-modes --relax
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMF-NO-STRICT
# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mpcrel %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6 --relax
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMS-STRICT
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6 --no-strict-address-modes --relax
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMS-NO-STRICT-PCREL
# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mno-pcrel %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6 --no-strict-address-modes --relax
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMS-NO-STRICT-NO-PCREL


# CHECK-NMF-STRICT: 60a3{{.*}} lapc {{.*}} <lapc_far_9>
# CHECK-NMF-STRICT: 60e3{{.*}} lapc {{.*}} <lapc_far_9>

# CHECK-NMF-STRICT: 60a3{{.*}} lapc {{.*}} <lapc_far_18>
# CHECK-NMF-STRICT: 38{{.*}} balc {{.*}} <relax_balc_after_expand_18>


# CHECK-NMF-STRICT: 40{{.*}} lw {{.*}}(gp)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} lw {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} lw {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT: 84a6{{.*}} sw {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60a2{{.*}} addiu {{.*}},gp

# CHECK-NMF-STRICT: 44{{.*}} lb {{.*}}(gp)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} lb {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} sb {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} lbu {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 40{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT-NEXT: 60a2{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT: 60a2{{.*}} addiu {{.*}},gp

# CHECK-NMF-STRICT: 44{{.*}} lh {{.*}}(gp)
# CHECK-NMF-STRICT-NEXT: 44{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} lh {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} lhu {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} sh {{.*}}0(a2)

# CHECK-NMF-STRICT: 56{{.*}} lw {{.*}}(gp)
# CHECK-NMF-STRICT-NEXT: d6{{.*}} sw {{.*}}(gp)
# CHECK-NMF-STRICT-NEXT: 41{{.*}} lw {{.*}}(gp)
# CHECK-NMF-STRICT-NEXT: 40{{.*}} lw {{.*}}(gp)

# CHECK-NMF-STRICT-NEXT: 44{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT-NEXT: 60a2{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT: 40{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT-NEXT: 60a2{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT: 60a2{{.*}} addiu {{.*}},gp

# CHECK-NMF-STRICT: 60a3{{.*}} lapc {{.*}} <lapc_far_19>
# CHECK-NMF-STRICT: 60a3{{.*}} lapc {{.*}} <lapc_far_19>
# CHECK-NMF-STRICT: 38{{.*}} balc {{.*}} <relax_balc_after_expand_19>
# CHECK-NMF-STRICT: 38{{.*}} balc {{.*}} <relax_balc_after_expand_19>




# Only differences from now on

# CHECK-NMF-NO-STRICT: 60ab{{.*}} lwpc {{.*}}<no_expand+0x2>
# CHECK-NMF-NO-STRICT: 60ab{{.*}} lwpc {{.*}}<a>
# CHECK-NMF-NO-STRICT: 60af{{.*}} swpc {{.*}}<b>



# CHECK-NMS-STRICT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu a2,a2,gp
# CHECK-NMS-STRICT-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-STRICT-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu a2,a2,gp
# CHECK-NMS-STRICT-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-STRICT-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu a2,a2,gp
# CHECK-NMS-STRICT-NEXT: 84a6{{.*}} sw {{.*}}(a2)
# CHECK-NMS-STRICT-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 80c6{{.*}} ori a2,a2
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu {{.*}},a2,gp

# CHECK-NMS-STRICT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu a2,a2,gp
# CHECK-NMS-STRICT-NEXT: 84a6{{.*}} lb {{.*}}(a2)
# CHECK-NMS-STRICT-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu a2,a2,gp
# CHECK-NMS-STRICT-NEXT: 84a6{{.*}} sb{{.*}}(a2)
# CHECK-NMS-STRICT-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu a2,a2,gp
# CHECK-NMS-STRICT-NEXT: 84a6{{.*}} lbu{{.*}}(a2)
# CHECK-NMS-STRICT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 80c6{{.*}} ori a2,a2
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu {{.*}},a2,gp
# CHECK-NMS-STRICT-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 80c6{{.*}} ori a2,a2
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu {{.*}},a2,gp

# CHECK-NMS-STRICT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu a2,a2,gp
# CHECK-NMS-STRICT-NEXT: 84a6{{.*}} lh {{.*}}(a2)
# CHECK-NMS-STRICT-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu a2,a2,gp
# CHECK-NMS-STRICT-NEXT: 84a6{{.*}} lhu{{.*}}(a2)
# CHECK-NMS-STRICT-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-STRICT-NEXT: 2386{{.*}} addu a2,a2,gp
# CHECK-NMS-STRICT-NEXT: 84a6{{.*}} sh{{.*}}(a2)


# CHECK-NMS-NO-STRICT-PCREL: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} sw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

# CHECK-NMS-NO-STRICT-PCREL: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lb {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} sb {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lbu {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

# CHECK-NMS-NO-STRICT-PCREL: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lh {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lhu {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} sh {{.*}}(a2)



# CHECK-NMS-NO-STRICT-NO-PCREL: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} sw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

# CHECK-NMS-NO-STRICT-NO-PCREL: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lb {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} sb {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lbu {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

# CHECK-NMS-NO-STRICT-NO-PCREL: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lh {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lhu {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} sh {{.*}}(a2)


    .linkrelax
    .section .text, "ax", @progbits
    .align 1
    .globl _start
    .ent _start

_start:
    lw $a1, %gprel(no_expand)($gp)
    # Should be expanded as no_expand+2 is not at a
    # multiple of 4
    lw $a1, %gprel(no_expand+2)($gp)
    lw $a1, %gprel(a)($gp)
    sw $a1, %gprel(b)($gp)
    addiu.w $a1, $gp, %gprel(b)

    lb $a1, %gprel(in_range_18)($gp)
    lb $a1, %gprel(out_range_18)($gp)
    sb $a1, %gprel(out_range_18)($gp)
    lbu $a1, %gprel(out_range_18)($gp)
    addiu.b $a1, $gp, %gprel(out_range_18 + 2)
    addiu.b $a1, $gp, %gprel(out_range_18)
    addiu.b $a1, $gp, %gprel(b)

    lh $a1, %gprel(in_range_18-1)($gp)
    # Should expand this one as it is not aligned on 2
    lh $a1, %gprel(in_range_18)($gp)
    lhu $a1, %gprel(out_range_18)($gp)
    sh $a1, %gprel(out_range_18)($gp)

    # Relax

    lw $a1, %gprel(gprel9_relax)($gp)
    sw $a1, %gprel(gprel9_relax)($gp)
    # No expand, not valid reg
    lw $t0, %gprel(gprel9_relax)($gp)
    # Relax then expand
    lw $a1, %gprel(gprel9_relax_expand_sym)($gp)

    # Expand then relax
    addiu.b $a1, $gp, %gprel(gprel_i32_18_expand_relax_sym)
    # Expand no relax
    addiu.b $a1, $gp, %gprel(gprel_i32_18_expand_relax_sym+3)
    # Expand then relax
    addiu.w $a1, $gp, %gprel(gprel_i32_19_expand_relax_sym)
    # Expand no relax
    addiu.w $a1, $gp, %gprel(gprel_i32_19_expand_relax_sym-2)
    # Expand no relax
    addiu.w $a1, $gp, %gprel(gprel_i32_19_expand_relax_sym+4)

    .end _start
    .size _start, .-_start

    .section .gprel9_relax_sec, "ax", @progbits
    .globl gprel9_relax
    .ent gprel9_relax

gprel9_relax:

    lapc $a1, lapc_far_9
    lapc $a3, lapc_far_9

gprel9_relax_expand_sym:

    .end gprel9_relax
    .size gprel9_relax, .-gprel9_relax


    .section .gprel_i32_18_lapc_sec, "ax", @progbits
    .globl gprel_i32_18_lapc
    .ent gprel_i32_18_lapc

gprel_i32_18_lapc:

    lapc $a1, lapc_far_18

relax_balc_after_expand_18:

    .end gprel_i32_18_lapc
    .size gprel_i32_18_lapc, .-gprel_i32_18_lapc



    .section .gprel_i32_18_sec, "ax", @progbits
    .globl gprel_i32_18
    .ent gprel_i32_18

gprel_i32_18:

    balc relax_balc_after_expand_18

gprel_i32_18_expand_relax_sym:

    .end gprel_i32_18
    .size gprel_i32_18, .-gprel_i32_18

    .section .gprel_i32_19_lapc_sec, "ax", @progbits
    .globl gprel_i32_19_lapc
    .ent gprel_i32_19_lapc

gprel_i32_19_lapc:

    lapc $a1, lapc_far_19
    lapc $a1, lapc_far_19

relax_balc_after_expand_19:

    .end gprel_i32_19_lapc
    .size gprel_i32_19_lapc, .-gprel_i32_19_lapc


    .section .gprel_i32_19_sec, "ax", @progbits
    .globl gprel_i32_19
    .ent gprel_i32_19

gprel_i32_19:

    balc relax_balc_after_expand_19
    balc relax_balc_after_expand_19

gprel_i32_19_expand_relax_sym:

    .end gprel_i32_19
    .size gprel_i32_19, .-gprel_i32_19
