MEMORY {
    SEG1 : ORIGIN = 0x10000, LENGTH = 0x10000
    SEG2 : ORIGIN = 0x20000, LENGTH = 0x10000
    SEG3 : ORIGIN = 0x30000, LENGTH = 0x10000
}

SECTIONS {
    . += ORIGIN(SEG1);
    OVERLAY . : {
        .overlay1_sec1 { 
            *(.overlay1_sec1)
            LONG(0);
        }

        .overlay1_sec2 { 
            *(.overlay1_sec2)
            defined_in_overlay = .;
        }
    } > SEG1 AT> SEG2

    OVERLAY . : {
        .overlay2_sec1 { *(.overlay2_sec1) }

        .overlay2_sec2 { *(.overlay2_sec2) }
    } > SEG1 AT> SEG3

    OVERLAY . : {
        .overlay3_sec1 { *(.overlay3_sec1) }

        .overlay3_sec2 { *(.overlay3_sec2) }
    } > SEG1 AT> SEG3

    .text : { *(.text) } > SEG1
}
