MEMORY {
    SEG1 : ORIGIN = 0x1000, LENGTH = 0x1000
    SEG2 : ORIGIN = 0x2000, LENGTH = 0x1000
}

SECTIONS {

    .text : {
        *(.text)
    } > SEG1 AT> SEG1

    .first (NOLOAD) : {
        *(.first)
    } > SEG2 AT> SEG1

    .second : {
        *(.second)
    } > SEG2 AT> SEG1

    .third (NOLOAD) : {
        *(.third)
    } > SEG2 AT> SEG1

    .nanoMIPS.abiflags : {
        *(.nanoMIPS.abiflags)
    } > SEG2 AT> SEG1



    .bss : {
        *(.bss)
    } > SEG2 AT> SEG1

    .data : {
        *(.data)
    } > SEG2 AT> SEG1
}