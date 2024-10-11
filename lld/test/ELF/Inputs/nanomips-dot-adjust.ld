MEMORY {
    SEG1 : ORIGIN = 0x1000, LENGTH = 0x1000
}

SECTIONS {

    .text : {
        *(.text)
    } > SEG1 AT> SEG1

    . = ALIGN(0x100);

    .first . : {
        *(.first)
    } > SEG1 AT> SEG1
}