SECTIONS {
    . = 0x20100;
    .text : { *(.text) }
    .other_text : { *(.other_text) }

    . = 0x820000;
    .expand_pc14_sec : { *(.expand_pc14_sec) }

    . = 0x824100;
    .pc14_far_sec : { *(.pc14_far_sec) }
}