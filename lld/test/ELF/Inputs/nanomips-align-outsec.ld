MEMORY {
    SEG1 : ORIGIN = 0x1000, LENGTH = 0x1000
    SEG2 : ORIGIN = 0x2000, LENGTH = 0x1000
}

SECTIONS {

    .text : {
        *(.text)
    } > SEG1 AT> SEG1

    .first ORIGIN(SEG2) + SIZEOF(.text) : ALIGN(0x20) {
        *(.first)
    } > SEG2 AT> SEG1

    .second : ALIGN(0x40) {
        *(.second)
    } > SEG2 AT> SEG1
}