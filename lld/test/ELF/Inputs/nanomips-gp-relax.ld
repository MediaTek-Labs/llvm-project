SECTIONS
{
    _gp = 0x2000;

    . = 0x21f4;
    .gprel9_relax_sec : { *(.gprel9_relax_sec) }

    no_expand = 0x201ffc;
    a = 0x202000;
    b = 0x203000;

    in_range_18 = 0x41fff;
    out_range_18 = 0x42000;

    . = 0x50000;
    .text : { *(.text) }
    
    .nanoMIPS.abiflags : { *(.nanoMIPS.abiflags) }

    lapc_far = 0x250010;

}