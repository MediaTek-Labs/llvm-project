SECTIONS
{
    . = 0x1000;
    .text : {
        *(.text)
    }

    . = 0x2000;
    /* Also tests if the _gp will be placed before of .sdata */
    .sdata : {
        *(.sdata)
    }

    .nanoMIPS.abiflags : {
        *(.nanoMIPS.abiflags)
    }
}
