# REQUIRES: nanomips
# RUN: %nanomips-elf-as -m32 -EL -march=32r6 -mpcrel %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMF-STRICT
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6 --no-strict-address-modes
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMF-NO-STRICT
# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mpcrel %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMS-STRICT
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6 --no-strict-address-modes
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMS-NO-STRICT-PCREL
# RUN: %nanomips-elf-as -m32 -EL -march=32r6s -mno-pcrel %s -o %t.o
# RUN: ld.lld -T %S/Inputs/nanomips-gp-relax.ld %t.o -o %t --expand-reg 6 --no-strict-address-modes
# RUN: %nanomips-elf-objdump -d %t | FileCheck %s --check-prefix=CHECK-NMS-NO-STRICT-NO-PCREL


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
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} lh {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} lhu {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu a2,gp
# CHECK-NMF-STRICT: 84a6{{.*}} sh {{.*}}0(a2)



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



# CHECK-NMS-NO-STRICT-PCREL: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} sw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

# CHECK-NMS-NO-STRICT-PCREL: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lb {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} sb {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lbu {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

# CHECK-NMS-NO-STRICT-PCREL: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lh {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lhu {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} sh {{.*}}(a2)



# CHECK-NMS-NO-STRICT-NO-PCREL: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} sw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

# CHECK-NMS-NO-STRICT-NO-PCREL: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lb {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} sb {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lbu {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

# CHECK-NMS-NO-STRICT-NO-PCREL: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lh {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lhu {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
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
    addiu.b $a1, $gp, %gprel(out_range_18)
    addiu.b $a1, $gp, %gprel(out_range_18 + 2)
    addiu.b $a1, $gp, %gprel(b)

    lh $a1, %gprel(in_range_18-1)($gp)
    # Should expand this one as it is not aligned on 2
    lh $a1, %gprel(in_range_18)($gp)
    lhu $a1, %gprel(out_range_18)($gp)
    sh $a1, %gprel(out_range_18)($gp)

    .end _start
    .size _start, .-_start
