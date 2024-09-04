SECTIONS
{
    _gp = 0x2000;

    . = 0x21f4;
    .gprel9_relax_sec : { *(.gprel9_relax_sec) }

    . = 0x41bf8;
    .gprel_i32_18_lapc_sec : { *(.gprel_i32_18_lapc_sec) }


    . = 0x41ffc;
    .gprel_i32_18_sec : { *(.gprel_i32_18_sec) }



    no_expand = 0x201ffc;
    a = 0x202000;
    b = 0x203000;

    in_range_18 = 0x41fff;
    out_range_18 = 0x42002;

    . = 0x50000;
    .text : { *(.text) }
    
    .nanoMIPS.abiflags : { *(.nanoMIPS.abiflags) }

    . = 0x201bf0;
    .gprel_i32_19_lapc_sec : { *(.gprel_i32_19_lapc_sec) }
    
    . = 0x201ff8;
    .gprel_i32_19_sec : { *(.gprel_i32_19_sec) }

    lapc_far_9 = 0x250010;
    lapc_far_18 = 0x241c06;
    lapc_far_19 = 0x401c02;
}