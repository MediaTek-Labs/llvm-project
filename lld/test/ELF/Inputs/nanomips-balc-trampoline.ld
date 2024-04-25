SECTIONS {
    . = 0xf00;
    .before_text : { *(.before_text) }
    . = 0x1000;
    .text : { *(.with_text) *(.text) *(.with_text_after)}
    . = 0x2000;
    .other_text : { *(.other_text) }
    . = 0x3000;
    .tramp_expansion : { *(.tramp_expansion) }
    . = 0x4000;
    .align_section : { *(.align_section) }
    . = 0x5000;
    .candidate_calculation_sec : { *(.candidate_calculation_sec) }
    . = 0x20100e;
    .a_section : { *(.a_section) }
    . = 0x203408;
    .long_distance : { *(.long_distance) }
    . = 0x2000000;
    .transform_bc_section : { *(.transform_bc_section) }
    .bss : { *(.bss) }
    .nanoMIPS.abiflags : { *(.nanoMIPS.abiflags) }
}