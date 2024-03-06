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
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT: 84a6{{.*}} lw {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT: 84a6{{.*}} lw {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60c2{{.*}} addiu {{.*}},gp
# CHECK-NMF-STRICT: 84a6{{.*}} sw {{.*}}0(a2)
# CHECK-NMF-STRICT-NEXT: 60a2{{.*}} addiu {{.*}},gp


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

# CHECK-NMS-NO-STRICT-PCREL: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 84a6{{.*}} sw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-PCREL-NEXT: e0c{{.*}} aluipc a2
# CHECK-NMS-NO-STRICT-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

# CHECK-NMS-NO-STRICT-NO-PCREL: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} lw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 84a6{{.*}} sw {{.*}}(a2)
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: e0c{{.*}} lui a2,%hi
# CHECK-NMS-NO-STRICT-NO-PCREL-NEXT: 80a6{{.*}} ori {{.*}},a2

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


    .end _start
    .size _start, .-_start
