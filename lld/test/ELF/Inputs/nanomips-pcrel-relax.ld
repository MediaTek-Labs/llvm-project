SECTIONS 
{
    . = 0x1000;
    .text : { *(.text) }

    . = 0x2000;
    pc14_s1_start = .;
    .pc14_s1 : { *(.pc_14_s1) }
    
    pc14_s1_pc5u_far = pc14_s1_start + 0x20;
    pc14_s1_pc8_far = pc14_s1_start + 0x80;
    pc14_s1_pc15_far = pc14_s1_start + 0x4000;

    . = 0x3000;
    .lapc_expands : { *(.lapc_expands) }

    lapc_far = 0x0a000000;

    . = 0x4000;
    pc21_s1_start = .;
    .pc21_s1 : { *(.pc21_s1) }

    pc21_s1_pc22_far = pc21_s1_start + 0x200000;

    . = 0x5000;
    pc25_s1_start = .;
    .pc25_s1 : { *(.pc25_s1) }

    pc25_s1_pc11_far = pc25_s1_start + 0x400;

    .nanoMIPS.abiflags : { *(.nanoMIPS.abiflags) }

    . = 0x6000;
    .lapc_expands_2 : { *(.lapc_expands_2) }

    . = 0x7000;
    pc11_s1_start = .;
    .pc11_s1 : { *(.pc11_s1) }

    pc11_s1_pc12_far =  pc11_s1_start + 0x800;
}