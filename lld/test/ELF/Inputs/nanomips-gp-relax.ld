SECTIONS
{
    _gp = 0x2000;

    . = 0x21f4;
    .gprel9_relax_sec : { *(.gprel9_relax_sec) }

    . = 0x2ffa;
    .lapc_gprel_i32_18_sec : { *(.lapc_gprel_i32_18_sec) }

    . = 0x41ffa;
    .gprel_i32_18_sec : { *(.gprel_i32_18_sec) }

    no_expand_19_s2 = 0x201ffc;
    a = 0x202000;
    b = 0x203000;

    in_range_18 = 0x41fff;
    out_range_18 = 0x42002;

    . = 0x50000;
    .text : { *(.text) }

    lapc_close = 0x100000;
    lapc_far = 0xa000000;

    . = 0x201ff8;
    .gprel_i32_21_sec : { *(.gprel_i32_21_sec) }

    .nanoMIPS.abiflags : { *(.nanoMIPS.abiflags) }
}