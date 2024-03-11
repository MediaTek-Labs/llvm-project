SECTIONS {
    . = 0x20100;
    .text : { *(.text) }
    .other_text : { *(.other_text) }

    . = 0x820000;
    .expand_pc14_sec : { *(.expand_pc14_sec) }

    . = 0x824100;
    .pc14_far_sec : { *(.pc14_far_sec) }

    . = 0x825000;
    .expand_pc10_sec : { *(.expand_pc10_sec) }

    . = 0x825400;
    .pc10_far_sec : { *(.pc10_far_sec) }
    pc10_far_sec_lapc_far = . + 0x200000;

    . = 0x826000;
    .expand_pc7_sec : { *(.expand_pc7_sec) }

    . = 0x826080;
    .pc7_far : { *(.pc7_far_sec) }
    pc7_far_sec_lapc_far = . + 0x200000;
}